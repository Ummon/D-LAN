#include <QTest>
#include <QPointer>
#include <QThread>
#include <QTcpServer>

#include <cstring>
#include <functional>
#include <memory>
#include <limits>

#include <google/protobuf/descriptor.pb.h>
#include <google/protobuf/dynamic_message.h>

#include <Common/Constants.h>
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
      qint64 maxWrite = std::numeric_limits<qint64>::max();
      bool disconnectOnWriteFailure = false;

      BufferedSocket()
      {
         this->setOpenMode(QIODevice::ReadWrite | QIODevice::Unbuffered);
         this->setSocketState(ConnectedState);
      }

      qint64 bytesAvailable() const override { return QTcpSocket::bytesAvailable() + this->input.size(); }
      bool atEnd() const override { return this->bytesAvailable() == 0; }
      void notify() { emit readyRead(); }
      void reconnect()
      {
         this->input.clear();
         this->setOpenMode(QIODevice::ReadWrite | QIODevice::Unbuffered);
         this->setSocketState(ConnectedState);
      }
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
         const auto available = qMin(size, this->maxWrite);
         const auto count = this->failAfter < 0 ? available : qMin(available, this->failAfter - this->output.size());
         this->output.append(data, count);
         return count;
      }
   };

   class ThreadCheckedSocket : public BufferedSocket
   {
   public:
      mutable int foreignThreadReads = 0;
      bool atEnd() const override
      {
         if (QThread::currentThread() != this->thread())
         {
            ++this->foreignThreadReads;
            return true; // Record the violation without reading another thread's buffer.
         }
         return BufferedSocket::atEnd();
      }
   };

   class TestPeer : public Common::MessageSocket
   {
      class Logger : public ILogger
      {
      public:
         explicit Logger(QStringList* messages = nullptr) : messages(messages) {}
      private:
         void logDebug(const QString& message) override { if (this->messages) this->messages->append(message); }
         void logError(const QString&) override {}
         QStringList* messages;
      };

   public:
      explicit TestPeer(QAbstractSocket* socket, QStringList* logs = nullptr,
         const Common::Hash& localID = {}, const Common::Hash& remoteID = {}) :
         MessageSocket(new Logger(logs), socket, localID, remoteID) {}
      std::function<void()> dataHook, acceptHook, messageHook, disconnectHook;
      QList<MessageHeader::MessageType> receivedTypes;
      int disconnections = 0;

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
      void onDisconnected() override
      {
         ++this->disconnections;
         const auto hook = this->disconnectHook;
         if (hook)
            hook();
      }
   };

   QByteArray frame(MessageHeader::MessageType type, const google::protobuf::Message* message = nullptr,
      const Common::Hash& sender = {})
   {
      const auto size = message ? message->ByteSizeLong() : 0;
      QByteArray bytes(MessageHeader::HEADER_SIZE + size, Qt::Uninitialized);
      Common::Message::writeMessageToBuffer(bytes.data(), bytes.size(), MessageHeader(type, size, sender), message);
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
   void adoptedSocketOutlivesPreviousParent()
   {
      auto previousOwner = std::make_unique<QObject>();
      QPointer<BufferedSocket> socket = new BufferedSocket;
      socket->setParent(previousOwner.get());
      {
         TestPeer peer(socket.data());
         QVERIFY(socket->parent() == nullptr);
         previousOwner.reset();
         QVERIFY(!socket.isNull());

         socket->input = frame(MessageHeader::GUI_REFRESH);
         peer.startListening();
         QCOMPARE(peer.receivedTypes, QList<MessageHeader::MessageType>{MessageHeader::GUI_REFRESH});
      }
      QCoreApplication::sendPostedEvents(socket.data(), QEvent::DeferredDelete);
      QVERIFY(socket.isNull());
   }

   void disconnectWhilePaused_data()
   {
      QTest::addColumn<bool>("fixedLocalID");
      QTest::addColumn<bool>("fixedRemoteID");
      for (bool local : {false, true})
         for (bool remote : {false, true})
            QTest::newRow(qPrintable(QString("fixed-local=%1-remote=%2").arg(local).arg(remote))) << local << remote;
   }

   void disconnectWhilePaused()
   {
      QFETCH(bool, fixedLocalID);
      QFETCH(bool, fixedRemoteID);
      QTcpServer server;
      QVERIFY(server.listen(QHostAddress::LocalHost));
      const Common::Hash firstID(QByteArray(Common::Hash::HASH_SIZE, 'a'));
      const Common::Hash secondID(QByteArray(Common::Hash::HASH_SIZE, 'b'));
      const auto localID = fixedLocalID ? secondID : Common::Hash();
      const auto remoteID = fixedRemoteID ? firstID : Common::Hash();
      auto* socket = new QTcpSocket;
      TestPeer peer(socket, nullptr, localID, remoteID);
      socket->connectToHost(QHostAddress::LocalHost, server.serverPort());
      QTRY_VERIFY(peer.isConnected() && server.hasPendingConnections());
      std::unique_ptr<QTcpSocket> remote(server.nextPendingConnection());
      peer.startListening();
      remote->write(frame(MessageHeader::GUI_REFRESH, nullptr, firstID));
      QTRY_COMPARE(peer.receivedTypes.size(), 1);
      QCOMPARE(peer.getRemoteID(), firstID);
      QCOMPARE(peer.getLocalID(), fixedLocalID ? localID : firstID);

      // Leave a parsed header waiting for its body in the old session.
      int dataNotifications = 0;
      peer.dataHook = [&] { ++dataNotifications; };
      Protos::GUI::JoinRoom message;
      message.set_name(std::string(1024, 'x'));
      remote->write(frame(MessageHeader::GUI_JOIN_ROOM, &message, firstID).first(MessageHeader::HEADER_SIZE));
      QTRY_VERIFY(dataNotifications > 0 && socket->bytesAvailable() == 0);
      peer.stopListening();
      peer.startListening();
      peer.stopListening();
      remote->abort();
      QTRY_VERIFY(!peer.isConnected());
      QCOMPARE(peer.disconnections, 1);
      QCOMPARE(peer.getLocalID(), localID);
      QCOMPARE(peer.getRemoteID(), remoteID);

      socket->connectToHost(QHostAddress::LocalHost, server.serverPort());
      QTRY_VERIFY(peer.isConnected() && server.hasPendingConnections());
      remote.reset(server.nextPendingConnection());
      peer.startListening();
      const auto nextID = fixedRemoteID ? firstID : secondID;
      remote->write(frame(MessageHeader::GUI_REFRESH, nullptr, nextID));
      QTRY_COMPARE(peer.receivedTypes.size(), 2);
      QVERIFY(peer.isConnected());
      QCOMPARE(peer.getRemoteID(), nextID);
      QCOMPARE(peer.getLocalID(), fixedLocalID ? localID : nextID);
      QCOMPARE(peer.disconnections, 1);
   }

   void disconnectOnTransferThread_data()
   {
      QTest::addColumn<bool>("resumeBeforeNotification");
      QTest::newRow("queued-cleanup") << false;
      QTest::newRow("resume-before-queued-cleanup") << true;
   }

   void disconnectOnTransferThread()
   {
      QFETCH(bool, resumeBeforeNotification);
      auto* socket = new BufferedSocket;
      TestPeer peer(socket);
      const Common::Hash firstID(QByteArray(Common::Hash::HASH_SIZE, 'a'));
      const Common::Hash secondID(QByteArray(Common::Hash::HASH_SIZE, 'b'));
      socket->input = frame(MessageHeader::GUI_REFRESH, nullptr, firstID);
      peer.startListening();
      peer.stopListening();
      QThread* callbackThread = nullptr;
      peer.disconnectHook = [&] { callbackThread = QThread::currentThread(); };

      QThread worker;
      worker.start();
      auto* mainThread = QThread::currentThread();
      const bool moved = socket->moveToThread(&worker);
      bool restored = false;
      if (moved)
         QMetaObject::invokeMethod(socket, [&] {
            socket->close();
            restored = socket->moveToThread(mainThread);
         }, Qt::BlockingQueuedConnection);
      worker.quit();
      worker.wait();
      QVERIFY(moved);
      QVERIFY(restored);
      QCOMPARE(peer.disconnections, 0); // The worker must not run the callback.
      if (!resumeBeforeNotification)
      {
         QCoreApplication::sendPostedEvents(&peer, QEvent::MetaCall);
         QCOMPARE(peer.disconnections, 1);
         QVERIFY(peer.getRemoteID().isNull());
      }

      socket->reconnect();
      socket->input = frame(MessageHeader::GUI_REFRESH, nullptr, secondID);
      peer.startListening();
      QCOMPARE(callbackThread, mainThread);
      QCOMPARE(peer.disconnections, 1);
      QCOMPARE(peer.receivedTypes.size(), 2);
      QCOMPARE(peer.getRemoteID(), secondID);
      // An old queued notification must not clear the new session's identity.
      QCoreApplication::sendPostedEvents(&peer, QEvent::MetaCall);
      QCOMPARE(peer.disconnections, 1);
      QCOMPARE(peer.getRemoteID(), secondID);
   }

   void boundedDebugLogging_data()
   {
      QTest::addColumn<QByteArray>("body");
      QTest::addColumn<bool>("omitted");
      QTest::newRow("small") << QByteArray("small diagnostic text") << false;
      QTest::newRow("large-payload") << QByteArray(16384, 'x') << true;
      QTest::newRow("expanded-json") << QByteArray(2048, '\x01') << true;
   }

   void boundedDebugLogging()
   {
#ifndef DEBUG
      QSKIP("Debug logging is disabled in this build.");
#else
      QFETCH(QByteArray, body);
      QFETCH(bool, omitted);
      QStringList logs;
      auto* socket = new BufferedSocket;
      TestPeer peer(socket, &logs);
      peer.startListening();
      Protos::GUI::JoinRoom message;
      message.set_name(body.toStdString());
      logs.clear();
      peer.send(MessageHeader::GUI_JOIN_ROOM, message);
      QCOMPARE(logs.size(), 1);
      const auto sentLog = logs.first();
      logs.clear();
      socket->input = socket->output;
      socket->notify();
      QCOMPARE(logs.size(), 1);
      for (const auto& log : {sentLog, logs.first()})
      {
         QVERIFY(log.size() < 9 * 1024);
         QVERIFY(log.contains("JOIN_ROOM"));
         QVERIFY(log.contains(QString("size = %1").arg(message.ByteSizeLong())));
         QCOMPARE(log.contains("message body omitted"), omitted);
         if (!omitted)
            QVERIFY(log.contains(QString::fromUtf8(body)));
      }
      QCOMPARE(peer.receivedTypes.size(), 1);
#endif
   }

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

   void cachedSerializationMatchesWireFormat_data()
   {
      QTest::addColumn<int>("payloadSize");
      QTest::addColumn<bool>("shortWrites");
      for (const int size : { 0, 1, 127, 128, Common::Constants::PROTOBUF_STREAMING_BUFFER_SIZE,
                              3 * Common::Constants::PROTOBUF_STREAMING_BUFFER_SIZE + 17 })
         for (const bool shortWrites : { false, true })
            QTest::newRow(qPrintable(QString("size=%1-short=%2").arg(size).arg(shortWrites))) << size << shortWrites;
   }

   void cachedSerializationMatchesWireFormat()
   {
      QFETCH(int, payloadSize);
      QFETCH(bool, shortWrites);
      auto* socket = new BufferedSocket;
      const auto sender = Common::Hash::rand();
      TestPeer peer(socket, nullptr, sender);
      peer.startListening();
      // Each header field must fit in a write; larger protobuf buffers will
      // exercise the adapter's short-write handling.
      Protos::GUI::State message;
      if (payloadSize > 0)
      {
         auto* download = message.add_downloads();
         download->set_id(123);
         download->mutable_local_entry()->set_name(std::string(payloadSize, 'x'));
         download->mutable_local_entry()->mutable_shared_entry()->mutable_id()->set_hash(sender.getData(), Common::Hash::HASH_SIZE);
         message.mutable_stats()->set_download_rate(456);
      }
      const QByteArray expected = frame(MessageHeader::GUI_STATE, &message, sender);
      if (shortWrites)
         socket->maxWrite = Common::Hash::HASH_SIZE; // Large enough for each header field.
      peer.send(MessageHeader::GUI_STATE, message);
      peer.send(MessageHeader::GUI_REFRESH);
      QCOMPARE(socket->output, expected + frame(MessageHeader::GUI_REFRESH, nullptr, sender));
      QVERIFY(peer.isConnected());
      const auto decoded = Common::Message::readMessage(socket->output.constData(), expected.size());
      QCOMPARE(decoded.getMessage<Protos::GUI::State>().SerializeAsString(), message.SerializeAsString());
   }

   void sizesAreRefreshedAfterNestedMutation()
   {
      auto* socket = new BufferedSocket;
      TestPeer peer(socket);
      peer.startListening();
      Protos::GUI::State message;
      message.add_downloads()->mutable_local_entry()->set_name("small");
      peer.send(MessageHeader::GUI_STATE, message);
      QCOMPARE(socket->output, frame(MessageHeader::GUI_STATE, &message));
      socket->output.clear();

      // Compute an old size, then modify a nested field. send() must refresh
      // every nested cache, rather than relying on GetCachedSize() from before.
      (void)message.ByteSizeLong();
      message.mutable_downloads(0)->mutable_local_entry()->set_name(std::string(17000, 'y'));
      peer.send(MessageHeader::GUI_STATE, message);
      QCOMPARE(socket->output, frame(MessageHeader::GUI_STATE, &message));
      socket->output.clear();
      message.clear_downloads();
      peer.send(MessageHeader::GUI_STATE, message);
      QCOMPARE(socket->output, frame(MessageHeader::GUI_STATE, &message));
      QVERIFY(peer.isConnected());
   }

   void rejectsUninitializedMessageBeforeWriting()
   {
      // D-LAN's schemas use proto3, so build a proto2 message to verify that
      // bypassing the normal serializer does not bypass required-field checks.
      google::protobuf::FileDescriptorProto file;
      file.set_name("required-field-test.proto");
      file.set_syntax("proto2");
      auto* descriptor = file.add_message_type();
      descriptor->set_name("RequiredMessage");
      auto* field = descriptor->add_field();
      field->set_name("name");
      field->set_number(1);
      field->set_type(google::protobuf::FieldDescriptorProto::TYPE_STRING);
      field->set_label(google::protobuf::FieldDescriptorProto::LABEL_REQUIRED);
      google::protobuf::DescriptorPool pool;
      const auto* built = pool.BuildFile(file);
      QVERIFY(built);
      google::protobuf::DynamicMessageFactory factory;
      std::unique_ptr<google::protobuf::Message> message(factory.GetPrototype(built->message_type(0))->New());
      QVERIFY(!message->IsInitialized());
      auto* socket = new BufferedSocket;
      TestPeer peer(socket);
      peer.startListening();
      peer.send(MessageHeader::GUI_JOIN_ROOM, *message);
      QVERIFY(socket->output.isEmpty());
      QVERIFY(!peer.isConnected());
   }

   void rejectNullSend_data()
   {
      QTest::addColumn<bool>("withBody");
      QTest::newRow("header-only") << false;
      QTest::newRow("with-body") << true;
   }

   void rejectNullSend()
   {
      QFETCH(bool, withBody);
      auto* socket = new BufferedSocket;
      TestPeer peer(socket);
      peer.startListening();
      int disconnected = 0;
      connect(socket, &QTcpSocket::disconnected, this, [&] { ++disconnected; });
      Protos::GUI::JoinRoom message;
      message.set_name("room");
      if (withBody)
         peer.send(MessageHeader::NULL_MESS, message);
      else
         peer.send(MessageHeader::NULL_MESS);

      QVERIFY(socket->output.isEmpty());
      QVERIFY(!peer.isConnected());
      QCOMPARE(disconnected, 1);
   }

   void nullSendDisconnectDeletesPeer()
   {
      auto* socket = new BufferedSocket;
      QPointer<TestPeer> peer = new TestPeer(socket);
      peer->startListening();
      const auto connection = connect(socket, &QTcpSocket::disconnected, this, [&] { delete peer.data(); });
      peer->send(MessageHeader::NULL_MESS);
      const bool deleted = peer.isNull();
      disconnect(connection);
      delete peer.data();
      QVERIFY(deleted);
      QVERIFY(socket->output.isEmpty());
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

   void stopFromDataHook_data()
   {
      QTest::addColumn<bool>("headerAlreadyRead");
      QTest::newRow("before-header") << false;
      QTest::newRow("waiting-for-body") << true;
   }

   void stopFromDataHook()
   {
      QFETCH(bool, headerAlreadyRead);
      auto* socket = new BufferedSocket;
      TestPeer peer(socket);
      int accepted = 0;
      QList<MessageHeader::MessageType> signaledTypes;
      QString roomName;
      peer.acceptHook = [&] { ++accepted; };
      connect(&peer, &Common::MessageSocket::newMessage, this, [&](const Common::Message& message) {
         signaledTypes.append(message.getHeader().getType());
         if (message.getHeader().getType() == MessageHeader::GUI_JOIN_ROOM)
            roomName = QString::fromStdString(message.getMessage<Protos::GUI::JoinRoom>().name());
      });
      Protos::GUI::JoinRoom message;
      message.set_name("room");
      const auto firstFrame = frame(MessageHeader::GUI_JOIN_ROOM, &message);
      const auto secondFrame = frame(MessageHeader::GUI_REFRESH);
      if (headerAlreadyRead)
      {
         socket->input = firstFrame.first(MessageHeader::HEADER_SIZE + 1);
         peer.startListening();
         QVERIFY(peer.receivedTypes.isEmpty());
         socket->input += firstFrame.mid(MessageHeader::HEADER_SIZE + 1) + secondFrame;
      }
      else
         socket->input = firstFrame + secondFrame;

      peer.dataHook = [&] { peer.stopListening(); };
      if (headerAlreadyRead)
         socket->notify();
      else
         peer.startListening();

      const auto unread = (headerAlreadyRead ? firstFrame.mid(MessageHeader::HEADER_SIZE) : firstFrame) + secondFrame;
      QCOMPARE(accepted, 0);
      QVERIFY(peer.receivedTypes.isEmpty());
      QVERIFY(signaledTypes.isEmpty());
      QCOMPARE(socket->bytesAvailable(), unread.size());
      QCOMPARE(socket->peek(unread.size()), unread);

      peer.dataHook = {};
      peer.startListening();
      const QList<MessageHeader::MessageType> expected {MessageHeader::GUI_JOIN_ROOM, MessageHeader::GUI_REFRESH};
      QCOMPARE(peer.receivedTypes, expected);
      QCOMPARE(signaledTypes, expected);
      QCOMPARE(accepted, 2);
      QCOMPARE(roomName, QString("room"));
      QCOMPARE(socket->bytesAvailable(), 0);
   }

   void streamHandoff_data()
   {
      QTest::addColumn<QString>("stage");
      QTest::addColumn<QByteArray>("trailingData");
      for (const auto* stage : {"message", "signal"})
      {
         QTest::newRow(qPrintable(QString("%1-empty-buffer").arg(stage))) << QString(stage) << QByteArray();
         QTest::newRow(qPrintable(QString("%1-buffered-stream").arg(stage))) << QString(stage) << QByteArray("raw chunk bytes");
      }
   }

   void streamHandoff()
   {
      QFETCH(QString, stage);
      QFETCH(QByteArray, trailingData);
      QObject previousOwner;
      QThread worker;
      worker.start();
      auto* socket = new ThreadCheckedSocket;
      socket->setParent(&previousOwner);
      TestPeer peer(socket);
      bool moved = false;
      installHook(peer, stage, [&] {
         peer.stopListening();
         moved = socket->moveToThread(&worker);
      });
      socket->input = frame(MessageHeader::GUI_REFRESH) + trailingData;
      peer.startListening();

      // Restore ownership and stop the worker before any assertion can return.
      bool restored = false;
      auto* mainThread = QThread::currentThread();
      if (moved)
         QMetaObject::invokeMethod(socket, [&] { restored = socket->moveToThread(mainThread); }, Qt::BlockingQueuedConnection);
      worker.quit();
      worker.wait();

      QVERIFY(moved);
      QVERIFY(restored);
      QCOMPARE(socket->foreignThreadReads, 0);
      QCOMPARE(peer.receivedTypes, QList<MessageHeader::MessageType>{MessageHeader::GUI_REFRESH});
      QCOMPARE(socket->readAll(), trailingData);
   }
};

QTEST_GUILESS_MAIN(MessageSocketTests)
#include "MessageSocketTests.moc"
