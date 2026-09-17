#include <QTest>
#include <QPointer>

#include <cstring>
#include <functional>

#include <Common/Network/MessageSocket.h>

namespace
{
   using Common::MessageHeader;

   // Control exactly which bytes are buffered when a callback restarts listening.
   class BufferedSocket : public QTcpSocket
   {
   public:
      QByteArray input, output;
      qint64 failAfter = -1;
      qint64 failureResult = -1;
      bool disconnectOnWriteFailure = false;

      BufferedSocket()
      {
         this->setOpenMode(QIODevice::ReadWrite | QIODevice::Unbuffered);
         this->setSocketState(ConnectedState);
      }

      qint64 bytesAvailable() const override { return QTcpSocket::bytesAvailable() + this->input.size(); }
      bool atEnd() const override { return this->bytesAvailable() == 0; }
      void notify() { emit readyRead(); }
      void close() override
      {
         if (this->state() == UnconnectedState)
            return;
         this->setSocketState(UnconnectedState);
         this->setOpenMode(QIODevice::NotOpen);
         emit disconnected();
      }

   protected:
      qint64 readData(char* data, qint64 size) override
      {
         const auto count = qMin(size, qint64(this->input.size()));
         std::memcpy(data, this->input.constData(), count);
         this->input.remove(0, count);
         return count;
      }
      qint64 writeData(const char* data, qint64 size) override
      {
         if (this->failAfter >= 0 && this->output.size() >= this->failAfter)
         {
            if (this->disconnectOnWriteFailure)
               this->close();
            return this->failureResult;
         }
         const auto count = this->failAfter < 0 ? size : qMin(size, this->failAfter - this->output.size());
         this->output.append(data, count);
         return count;
      }
   };

   class TestPeer : public Common::MessageSocket
   {
      class Logger : public ILogger
      {
         void logDebug(const QString&) override {}
         void logError(const QString&) override {}
      };

   public:
      explicit TestPeer(BufferedSocket* socket) : MessageSocket(new Logger, socket) {}
      std::function<void()> dataHook, acceptHook, messageHook;
      QList<MessageHeader::MessageType> receivedTypes;

   private:
      void onNewDataReceived() override
      {
         // A hook may delete the peer, including the stored std::function.
         const auto hook = this->dataHook;
         if (hook)
            hook();
      }
      bool acceptsMessage(const Common::Message&) override
      {
         const auto hook = this->acceptHook;
         if (hook)
            hook();
         return true;
      }
      void onNewMessage(const Common::Message& message) override
      {
         this->receivedTypes.append(message.getHeader().getType());
         const auto hook = this->messageHook;
         if (hook)
            hook();
      }
   };

   QByteArray frame(MessageHeader::MessageType type, const google::protobuf::Message* message = nullptr)
   {
      const auto size = message ? message->ByteSizeLong() : 0;
      QByteArray bytes(MessageHeader::HEADER_SIZE + size, Qt::Uninitialized);
      Common::Message::writeMessageToBuffer(bytes.data(), bytes.size(), MessageHeader(type, size, Common::Hash()), message);
      return bytes;
   }

   void installHook(TestPeer& peer, const QString& stage, const std::function<void()>& hook)
   {
      if (stage == "data")
         peer.dataHook = hook;
      else if (stage == "accept")
         peer.acceptHook = hook;
      else if (stage == "message")
         peer.messageHook = hook;
      else
         QObject::connect(&peer, &Common::MessageSocket::newMessage, &peer, hook);
   }
}

class MessageSocketTests : public QObject
{
   Q_OBJECT

private slots:
   void sendWriteFailure_data()
   {
      QTest::addColumn<int>("failAfter");
      QTest::addColumn<int>("failureResult");
      QTest::addColumn<bool>("withBody");
      for (int after : {0, 1, 3, 8, MessageHeader::HEADER_SIZE, MessageHeader::HEADER_SIZE + 3, MessageHeader::HEADER_SIZE + 4099})
         for (int result : {-1, 0})
            for (bool body : {false, true})
            {
               if (!body && after >= MessageHeader::HEADER_SIZE)
                  continue;
               QTest::newRow(qPrintable(QString("after=%1-result=%2-body=%3").arg(after).arg(result).arg(body)))
                  << after << result << body;
            }
   }

   void sendWriteFailure()
   {
      QFETCH(int, failAfter);
      QFETCH(int, failureResult);
      QFETCH(bool, withBody);
      auto* socket = new BufferedSocket;
      TestPeer peer(socket);
      peer.startListening();
      socket->failAfter = failAfter;
      socket->failureResult = failureResult;
      Protos::GUI::JoinRoom message;
      message.set_name(std::string(8192, 'x'));
      if (withBody)
         peer.send(MessageHeader::GUI_JOIN_ROOM, message);
      else
         peer.send(MessageHeader::GUI_REFRESH);

      QVERIFY(!peer.isConnected());
      QCOMPARE(socket->output.size(), failAfter);
      const auto incompleteFrame = socket->output;
      socket->failAfter = -1;
      peer.send(MessageHeader::GUI_REFRESH);
      QCOMPARE(socket->output, incompleteFrame);
   }

   void successfulSends()
   {
      auto* socket = new BufferedSocket;
      TestPeer peer(socket);
      peer.startListening();
      Protos::GUI::JoinRoom message;
      message.set_name("room");
      peer.send(MessageHeader::GUI_JOIN_ROOM, message);
      peer.send(MessageHeader::GUI_REFRESH);
      QCOMPARE(socket->output, frame(MessageHeader::GUI_JOIN_ROOM, &message) + frame(MessageHeader::GUI_REFRESH));
      QVERIFY(peer.isConnected());
   }

   void oversizedSend()
   {
      auto* socket = new BufferedSocket;
      TestPeer peer(socket);
      peer.startListening();
      Protos::GUI::JoinRoom message;
      // The string alone reaches the limit; the protobuf tag and length exceed it.
      message.mutable_name()->assign(100 * 1024 * 1024, 'x');
      peer.send(MessageHeader::GUI_JOIN_ROOM, message);
      QVERIFY(!peer.isConnected());
      QVERIFY(socket->output.isEmpty());
   }

   void sendFailureDisconnectDeletesPeer_data()
   {
      QTest::addColumn<bool>("duringWrite");
      QTest::newRow("close-after-failure") << false;
      QTest::newRow("disconnect-during-write") << true;
   }

   void sendFailureDisconnectDeletesPeer()
   {
      QFETCH(bool, duringWrite);
      auto* socket = new BufferedSocket;
      QPointer<TestPeer> peer = new TestPeer(socket);
      peer->startListening();
      const auto connection = connect(socket, &QTcpSocket::disconnected, this, [&] { delete peer.data(); });
      socket->failAfter = 0;
      socket->disconnectOnWriteFailure = duringWrite;
      peer->send(MessageHeader::GUI_REFRESH);
      const bool deleted = peer.isNull();
      disconnect(connection);
      delete peer.data();
      QVERIFY(deleted);
   }

   void rejectNullWireType_data()
   {
      QTest::addColumn<quint32>("payloadSize");
      QTest::addColumn<QByteArray>("bufferedData");
      QTest::addColumn<bool>("fragmentedHeader");
      const auto validFrame = frame(MessageHeader::GUI_REFRESH);
      for (bool fragmented : {false, true})
      {
         const auto suffix = fragmented ? "-fragmented" : "";
         QTest::newRow(qPrintable(QString("empty%1").arg(suffix))) << quint32(0) << QByteArray() << fragmented;
         QTest::newRow(qPrintable(QString("missing-body%1").arg(suffix))) << quint32(123) << QByteArray() << fragmented;
         QTest::newRow(qPrintable(QString("embedded-frame%1").arg(suffix)))
            << quint32(validFrame.size()) << validFrame << fragmented;
         QTest::newRow(qPrintable(QString("following-frame%1").arg(suffix))) << quint32(0) << validFrame << fragmented;
      }
   }

   void rejectNullWireType()
   {
      QFETCH(quint32, payloadSize);
      QFETCH(QByteArray, bufferedData);
      QFETCH(bool, fragmentedHeader);
      auto* socket = new BufferedSocket;
      TestPeer peer(socket);
      int accepted = 0;
      int signaled = 0;
      int disconnected = 0;
      peer.acceptHook = [&] { ++accepted; };
      connect(&peer, &Common::MessageSocket::newMessage, this, [&] { ++signaled; });
      connect(socket, &QTcpSocket::disconnected, this, [&] { ++disconnected; });

      QByteArray header(MessageHeader::HEADER_SIZE, Qt::Uninitialized);
      MessageHeader::writeHeader(header.data(), MessageHeader(MessageHeader::NULL_MESS, payloadSize, Common::Hash()));
      if (fragmentedHeader)
      {
         socket->input = header.first(header.size() - 1);
         peer.startListening();
         QVERIFY(peer.isConnected());
         QCOMPARE(accepted, 0);
         QCOMPARE(signaled, 0);
         socket->input += header.last(1) + bufferedData;
         socket->notify();
      }
      else
      {
         socket->input = header + bufferedData;
         peer.startListening();
      }

      QVERIFY(!peer.isConnected());
      QCOMPARE(disconnected, 1);
      QCOMPARE(accepted, 0);
      QCOMPARE(signaled, 0);
      QVERIFY(peer.receivedTypes.isEmpty());
   }

   void nullWireTypeDisconnectDeletesPeer()
   {
      auto* socket = new BufferedSocket;
      QPointer<TestPeer> peer = new TestPeer(socket);
      const auto connection = connect(socket, &QTcpSocket::disconnected, this, [&] { delete peer.data(); });
      socket->input = frame(MessageHeader::NULL_MESS);
      peer->startListening();
      const bool deleted = peer.isNull();
      disconnect(connection);
      delete peer.data();
      QVERIFY(deleted);
   }

   void restartDuringCallback_data()
   {
      QTest::addColumn<QString>("stage");
      QTest::addColumn<bool>("emptyFirst");
      QTest::addColumn<int>("fragment");
      for (const auto* stage : {"data", "accept", "message", "signal"})
         for (bool empty : {false, true})
            for (int fragment : {0, 1, 2})
               QTest::newRow(qPrintable(QString("%1-empty=%2-fragment=%3").arg(stage).arg(empty).arg(fragment)))
                  << QString(stage) << empty << fragment;
   }

   void restartDuringCallback()
   {
      QFETCH(QString, stage);
      QFETCH(bool, emptyFirst);
      QFETCH(int, fragment);
      auto* socket = new BufferedSocket;
      TestPeer peer(socket);
      bool restarted = false;
      bool insideRestart = false;
      bool nestedDispatch = false;
      installHook(peer, stage, [&] {
         if (restarted)
            return;
         restarted = true;
         insideRestart = true;
         peer.stopListening();
         peer.startListening();
         insideRestart = false;
      });
      QList<MessageHeader::MessageType> signaledTypes;
      QList<QString> roomNames;
      connect(&peer, &Common::MessageSocket::newMessage, this, [&](const Common::Message& message) {
         nestedDispatch |= insideRestart;
         signaledTypes.append(message.getHeader().getType());
         if (message.getHeader().getType() == MessageHeader::GUI_JOIN_ROOM)
            roomNames.append(QString::fromStdString(message.getMessage<Protos::GUI::JoinRoom>().name()));
      });

      Protos::GUI::JoinRoom first, second;
      first.set_name("first");
      second.set_name("second");
      const auto firstType = emptyFirst ? MessageHeader::GUI_REFRESH : MessageHeader::GUI_JOIN_ROOM;
      const auto firstFrame = frame(firstType, emptyFirst ? nullptr : &first);
      const auto secondFrame = frame(MessageHeader::GUI_JOIN_ROOM, &second);
      // Test a complete next frame, a partial header, and a partial body.
      const auto split = fragment == 0 ? secondFrame.size() : fragment == 1 ? 3 : MessageHeader::HEADER_SIZE + 1;
      socket->input = firstFrame + secondFrame.left(split);
      peer.startListening();

      QVERIFY(restarted);
      QVERIFY(!nestedDispatch);
      QCOMPARE(signaledTypes.size(), fragment == 0 ? 2 : 1);
      if (fragment != 0)
      {
         socket->input += secondFrame.mid(split);
         socket->notify();
      }
      const QList<MessageHeader::MessageType> expectedTypes {firstType, MessageHeader::GUI_JOIN_ROOM};
      QCOMPARE(peer.receivedTypes, expectedTypes);
      QCOMPARE(signaledTypes, expectedTypes);
      QCOMPARE(roomNames, emptyFirst ? QList<QString>{"second"} : QList<QString>({"first", "second"}));
      QCOMPARE(socket->bytesAvailable(), 0);
      QVERIFY(peer.isConnected());

      // Processing must also resume on a later readyRead after the guard is released.
      socket->input += frame(MessageHeader::GUI_REFRESH_NETWORK_INTERFACES);
      socket->notify();
      QCOMPARE(signaledTypes.size(), 3);
      QCOMPARE(signaledTypes.last(), MessageHeader::GUI_REFRESH_NETWORK_INTERFACES);
   }

   void deletionDuringCallback_data()
   {
      QTest::addColumn<QString>("stage");
      for (const auto* stage : {"data", "accept", "message", "signal"})
         QTest::newRow(stage) << QString(stage);
   }

   void deletionDuringCallback()
   {
      QFETCH(QString, stage);
      auto* socket = new BufferedSocket;
      QPointer<TestPeer> peer = new TestPeer(socket);
      installHook(*peer, stage, [&] { delete peer.data(); });
      socket->input = frame(MessageHeader::GUI_REFRESH) + frame(MessageHeader::GUI_REFRESH_NETWORK_INTERFACES);
      peer->startListening();
      QVERIFY(peer.isNull());
   }

   void stopThenResume()
   {
      auto* socket = new BufferedSocket;
      TestPeer peer(socket);
      peer.messageHook = [&] { peer.stopListening(); };
      socket->input = frame(MessageHeader::GUI_REFRESH) + frame(MessageHeader::GUI_REFRESH_NETWORK_INTERFACES);
      peer.startListening();
      QCOMPARE(peer.receivedTypes.size(), 1);
      QCOMPARE(socket->bytesAvailable(), MessageHeader::HEADER_SIZE);
      peer.startListening();
      QCOMPARE(peer.receivedTypes.size(), 2);
      QCOMPARE(peer.receivedTypes.last(), MessageHeader::GUI_REFRESH_NETWORK_INTERFACES);
      QCOMPARE(socket->bytesAvailable(), 0);
   }
};

QTEST_GUILESS_MAIN(MessageSocketTests)
#include "MessageSocketTests.moc"
