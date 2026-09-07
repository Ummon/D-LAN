#include <QCoreApplication>
#include <QCryptographicHash>
#include <QDir>
#include <QFile>
#include <QFileInfo>
#include <QStringList>
#include <QTemporaryDir>
#include <QTest>
#include <memory>

#include <Common/Constants.h>
#include <Common/PersistentData.h>
#include <Common/Settings.h>
#include <Protos/core_settings.pb.h>
#include <priv/ChatMessages.h>
#include <priv/ChatSystem.h>
#include <Core/PeerManager/Builder.h>
#include <QSignalSpy>

namespace
{
   const auto FOLDER = CS::ChatMessages::FOLDER_TYPE_MESSAGES_SAVED;

   class NetworkListener : public NL::INetworkListener
   {
   public:
      int limit = 256;
      QList<Protos::Common::ChatMessages> sent;
      QSharedPointer<NL::ISearch> newSearch() override { return {}; }
      void rebindSockets() override {}
      int getMaxUDPMessageSize() const override { return this->limit; }
      SendStatus send(Common::MessageHeader::MessageType type, const google::protobuf::Message& data,
         const Common::Hash& = Common::Hash()) override
      {
         if (data.ByteSizeLong() > static_cast<size_t>(this->limit))
            return SendStatus::MESSAGE_TOO_LARGE;
         if (type == Common::MessageHeader::CORE_CHAT_MESSAGES)
            this->sent << static_cast<const Protos::Common::ChatMessages&>(data);
         return SendStatus::OK;
      }
      void requestHistory()
      {
         Protos::Core::GetLastChatMessages request;
         request.set_number(500);
         const auto bytes = request.SerializeAsString();
         emit received(Common::Message::readMessageBody(
            Common::MessageHeader(Common::MessageHeader::CORE_GET_LAST_CHAT_MESSAGES, bytes.size(), Common::Hash::rand()), bytes.data()));
      }
   };

   QString roomFile(const QString& room)
   {
      const auto digest = QString::fromLatin1(QCryptographicHash::hash(room.toUtf8(), QCryptographicHash::Sha256).toHex());
      return Common::Constants::DIR_CHAT_MESSAGES + "/rooms/" + Common::Constants::FILE_CHAT_ROOM_MESSAGES.arg(digest);
   }

   Protos::Common::ChatMessages message(const QString& room, quint64 id)
   {
      Protos::Common::ChatMessages result;
      auto* entry = result.add_messages();
      entry->set_id(id);
      entry->set_time(1000);
      entry->set_message("saved message");
      entry->set_chat_room(room.toStdString());
      return result;
   }
}

class Tests : public QObject
{
   Q_OBJECT
private slots:
   void init()
   {
      this->directory = std::make_unique<QTemporaryDir>();
      QVERIFY(this->directory->isValid());
      Common::Global::setDataFolder(FOLDER, this->directory->path());
      QVERIFY(QDir(this->directory->path()).mkpath(Common::Constants::DIR_CHAT_MESSAGES));
   }

   void cleanup()
   {
      Common::Global::setDataFolderToDefault(FOLDER);
      this->directory.reset();
   }

   void sendRequiresSpaceForHistory()
   {
      const auto peers = PM::Builder::newPeerManager({});
      const auto network = QSharedPointer<NetworkListener>::create();
      CS::ChatSystem chat(peers, network);
      QSignalSpy notifications(&chat, &CS::IChatSystem::newMessages);

      // Walk the boundary: the first rejected message is only one text byte larger
      // than the last accepted one. Both its live and stored forms include the ID.
      QString lastAccepted;
      for (int length = 1; length < 256; ++length)
      {
         const QString text(length, 'x');
         const int sentBefore = network->sent.size();
         const int notificationsBefore = notifications.size();
         Protos::Common::ChatMessages before;
         chat.getLastChatMessages(before);
         const auto status = chat.send(text);
         if (status == CS::IChatSystem::SendStatus::MESSAGE_TOO_LARGE)
         {
            QVERIFY(!lastAccepted.isEmpty());
            QCOMPARE(network->sent.size(), sentBefore);
            QCOMPARE(notifications.size(), notificationsBefore);
            Protos::Common::ChatMessages after;
            chat.getLastChatMessages(after);
            QCOMPARE(after.SerializeAsString(), before.SerializeAsString());
            // With the timestamp omitted, a message this size still fits on the wire.
            auto live = network->sent.last();
            live.mutable_messages(0)->set_id(std::numeric_limits<quint64>::max());
            live.mutable_messages(0)->set_message(text.toStdString());
            QVERIFY(live.ByteSizeLong() <= static_cast<size_t>(network->limit));
            return;
         }
         QCOMPARE(status, CS::IChatSystem::SendStatus::OK);
         QCOMPARE(network->sent.last().messages(0).time(), quint64(0));
         const auto stored = qvariant_cast<Protos::Common::ChatMessages>(notifications.last().at(0));
         QVERIFY(stored.messages(0).time() > 0);
         QVERIFY(stored.ByteSizeLong() <= static_cast<size_t>(network->limit));
         lastAccepted = text;
      }
      QFAIL("No message reached the history size limit");
   }

   void historyUsesEffectiveLimit()
   {
      const auto peers = PM::Builder::newPeerManager({});
      const auto network = QSharedPointer<NetworkListener>::create();
      CS::ChatSystem chat(peers, network);
      for (int i = 0; i < 5; ++i)
         QCOMPARE(chat.send(QString(100, 'x')), CS::IChatSystem::SendStatus::OK);
      network->sent.clear();
      network->requestHistory();
      int count = 0;
      for (const auto& packet : network->sent)
      {
         QVERIFY(packet.ByteSizeLong() <= static_cast<size_t>(network->limit));
         count += packet.messages_size();
         for (const auto& entry : packet.messages())
            QVERIFY(entry.time() > 0);
      }
      QCOMPARE(count, 5);
      QVERIFY(network->sent.size() > 1);
   }

   void distinctRoomHistories()
   {
      const QStringList rooms { "General", "general", "a/b", "a&#47;b", QString(10000, 'x'),
         QString(QChar(0x00E9)), QString("e") + QChar(0x0301) };
      for (int i = 0; i < rooms.size(); ++i)
      {
         CS::ChatMessages history;
         history.add(message(rooms[i], i + 1));
         history.saveForRoom(rooms[i]);
         QVERIFY(QFile::exists(this->directory->filePath(roomFile(rooms[i]))));
         QVERIFY(QFileInfo(roomFile(rooms[i])).fileName().size() < 100);
      }
      QCOMPARE(QDir(this->directory->filePath("chat/rooms")).entryList(QDir::Files).size(), rooms.size());
      for (int i = 0; i < rooms.size(); ++i)
      {
         CS::ChatMessages history;
         history.loadForRoom(rooms[i]);
         QCOMPARE(history.getLastMessageIDs(10), QList<quint64> { quint64(i + 1) });
      }
   }

   void rejectForeignMessagesInNewHistory()
   {
      QVERIFY(QDir(this->directory->path()).mkpath("chat/rooms"));
      auto mixed = message("General", 1);
      mixed.MergeFrom(message("general", 2));
      Common::PersistentData::setValue(roomFile("General"), mixed, FOLDER);
      CS::ChatMessages history;
      history.loadForRoom("General");
      QCOMPARE(history.getLastMessageIDs(10), QList<quint64> { 1 });
      history.saveForRoom("General");
      Protos::Common::ChatMessages saved;
      Common::PersistentData::getValue(roomFile("General"), saved, FOLDER);
      QCOMPARE(saved.messages_size(), 1);
   }

   void rejectCorruptHistory()
   {
      QVERIFY(QDir(this->directory->path()).mkpath("chat/rooms"));
      QFile corrupt(this->directory->filePath(roomFile("General")));
      QVERIFY(corrupt.open(QIODevice::WriteOnly));
      corrupt.write("not a history");
      corrupt.close();
      CS::ChatMessages history;
      history.loadForRoom("General");
      QVERIFY(history.getMessages().isEmpty());
   }

   void retryFailedSave()
   {
      QFile obstruction(this->directory->filePath("chat/rooms"));
      QVERIFY(obstruction.open(QIODevice::WriteOnly));
      obstruction.close();
      CS::ChatMessages history;
      history.add(message("General", 1));
      history.saveForRoom("General");
      QCOMPARE(history.getLastMessageIDs(10), QList<quint64> { 1 });
      QVERIFY(obstruction.remove());
      history.saveForRoom("General");
      QVERIFY(QFile::exists(this->directory->filePath(roomFile("General"))));
   }

   void mainChatKeepsItsFilename()
   {
      CS::ChatMessages history;
      history.add(message(QString(), 1));
      history.saveForRoom();
      QVERIFY(QFile::exists(this->directory->filePath("chat/" + Common::Constants::FILE_CHAT_MESSAGES)));
      CS::ChatMessages reloaded;
      reloaded.loadForRoom();
      QCOMPARE(reloaded.getLastMessageIDs(10), QList<quint64> { 1 });
   }

private:
   std::unique_ptr<QTemporaryDir> directory;
};

int main(int argc, char** argv)
{
   QCoreApplication app(argc, argv);
   auto* settings = new Protos::Core::Settings();
   settings->set_max_number_of_stored_chat_messages(500);
   settings->set_get_last_chat_messages_period(2000);
   settings->set_save_chat_messages_period(90000);
   settings->set_max_udp_datagram_size(16356); // Deliberately different from the mock's effective limit.
   settings->set_peer_timeout_factor(3.2);
   settings->set_peer_imalive_period(5000);
   settings->set_pending_socket_timeout(10000);
   settings->set_nick("test");
   const auto peerID = Common::Hash::rand();
   settings->mutable_peer_id()->set_hash(peerID.getData(), Common::Hash::HASH_SIZE);
   SETTINGS.setSettingsMessage(settings);
   int result;
   {
      Tests tests;
      result = QTest::qExec(&tests, argc, argv);
   }
   SETTINGS.free();
   google::protobuf::ShutdownProtobufLibrary();
   return result;
}

#include "Tests.moc"
