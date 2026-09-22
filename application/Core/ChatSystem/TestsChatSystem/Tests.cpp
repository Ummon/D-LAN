#include <QCoreApplication>
#include <QCryptographicHash>
#include <QDir>
#include <QFile>
#include <QFileInfo>
#include <QHostAddress>
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
      QList<Common::Hash> historyRequestRecipients;
      QSharedPointer<NL::ISearch> newSearch() override { return {}; }
      void rebindSockets() override {}
      int getMaxUDPMessageSize() const override { return this->limit; }
      SendStatus send(Common::MessageHeader::MessageType type, const google::protobuf::Message& data,
         const Common::Hash& peerID = Common::Hash()) override
      {
         if (data.ByteSizeLong() > static_cast<size_t>(this->limit))
            return SendStatus::MESSAGE_TOO_LARGE;
         if (type == Common::MessageHeader::CORE_CHAT_MESSAGES)
            this->sent << static_cast<const Protos::Common::ChatMessages&>(data);
         else if (type == Common::MessageHeader::CORE_GET_LAST_CHAT_MESSAGES)
            this->historyRequestRecipients << peerID;
         return SendStatus::OK;
      }
      void requestHistory(const QString& room = QString())
      {
         Protos::Core::GetLastChatMessages request;
         request.set_number(500);
         if (!room.isEmpty())
            request.set_chat_room(room.toStdString());
         const auto bytes = request.SerializeAsString();
         emit received(Common::Message::readMessageBody(
            Common::MessageHeader(Common::MessageHeader::CORE_GET_LAST_CHAT_MESSAGES, bytes.size(), Common::Hash::rand()), bytes.data()));
      }
      void receiveIMAlive(const Common::Hash& peerID, const QStringList& rooms)
      {
         Protos::Core::IMAlive IMAlive;
         for (const auto& room : rooms)
            IMAlive.add_chat_rooms(room.toStdString());
         const auto bytes = IMAlive.SerializeAsString();
         emit received(Common::Message::readMessageBody(
            Common::MessageHeader(Common::MessageHeader::CORE_IM_ALIVE, bytes.size(), peerID), bytes.data()));
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
      // Joining or leaving a room saves the settings.
      Common::Global::setDataFolder(Common::Global::DataFolderType::ROAMING, this->directory->path());
      QVERIFY(QDir(this->directory->path()).mkpath(Common::Constants::DIR_CHAT_MESSAGES));
   }

   void cleanup()
   {
      Common::Global::setDataFolderToDefault(FOLDER);
      Common::Global::setDataFolderToDefault(Common::Global::DataFolderType::ROAMING);
      SETTINGS.set("joined_chat_rooms", QList<QString>()); // Otherwise the next test's chat system would join them.
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

   void historyRequestedOnlyFromAvailablePeers()
   {
      const auto peers = PM::Builder::newPeerManager({});
      const auto network = QSharedPointer<NetworkListener>::create();
      auto addPeer = [&](quint32 version) {
         const auto ID = Common::Hash::rand();
         peers->updatePeer(ID, QHostAddress::LocalHost, 1, "peer", 0, QString(), 0, 0, version);
         return ID;
      };
      const auto available = addPeer(Common::Constants::PROTOCOL_VERSION);
      addPeer(Common::Constants::PROTOCOL_VERSION ^ 1u); // Incompatible.
      peers->getPeer(addPeer(Common::Constants::PROTOCOL_VERSION))->block(60 * 1000);
      QCOMPARE(peers->getPeers().size(), 3);

      // The recipient is chosen randomly: repeat to make a wrong choice nearly certain to show up.
      CS::ChatSystem chat(peers, network);
      for (int i = 0; i < 30; ++i)
         QVERIFY(QMetaObject::invokeMethod(&chat, "retrieveLastChatMessages")); // Normally called periodically.
      QCOMPARE(network->historyRequestRecipients.size(), 30);
      for (const auto& recipient : network->historyRequestRecipients)
         QCOMPARE(recipient, available);
   }

   void leftRoomHistoryNotShared()
   {
      const auto peers = PM::Builder::newPeerManager({});
      const auto network = QSharedPointer<NetworkListener>::create();
      CS::ChatSystem chat(peers, network);

      // A remote peer stays in the room, so the room is still known after we leave it.
      const auto peerID = Common::Hash::rand();
      peers->updatePeer(peerID, QHostAddress::LocalHost, 1, "peer", 0, QString(), 0, 0, Common::Constants::PROTOCOL_VERSION);
      network->receiveIMAlive(peerID, { "General" });

      chat.joinRoom("General");
      QCOMPARE(chat.send("hello", "General"), CS::IChatSystem::SendStatus::OK);
      network->sent.clear();
      network->requestHistory("General");
      QCOMPARE(network->sent.size(), 1);

      chat.leaveRoom("General");
      const auto rooms = chat.getRooms();
      QCOMPARE(rooms.size(), 1);
      QVERIFY(!rooms.first().joined);

      network->sent.clear();
      network->requestHistory("General");
      QVERIFY(network->sent.isEmpty());
      Protos::Common::ChatMessages messages;
      chat.getLastChatMessages(messages, std::numeric_limits<int>::max(), "General");
      QCOMPARE(messages.messages_size(), 0);
   }

   void roomHistorySavedOnLeaveAndExit()
   {
      const auto peers = PM::Builder::newPeerManager({});
      const auto network = QSharedPointer<NetworkListener>::create();
      const auto exists = [&](const QString& file) { return QFile::exists(this->directory->filePath(file)); };
      {
         CS::ChatSystem chat(peers, network);
         chat.joinRoom("Left");
         chat.joinRoom("Kept");
         QCOMPARE(chat.send("hello"), CS::IChatSystem::SendStatus::OK);
         QCOMPARE(chat.send("bye", "Left"), CS::IChatSystem::SendStatus::OK);
         QCOMPARE(chat.send("hi", "Kept"), CS::IChatSystem::SendStatus::OK);

         chat.leaveRoom("Left");
         QVERIFY(exists(roomFile("Left")));
         QVERIFY(!exists(roomFile("Kept")));

         // Nobody else is in the left room: it's forgotten.
         const auto rooms = chat.getRooms();
         QCOMPARE(rooms.size(), 1);
         QCOMPARE(rooms.first().name, QString("Kept"));
         QCOMPARE(chat.send("again", "Left"), CS::IChatSystem::SendStatus::UNABLE_TO_SEND);
      }
      // The main chat and the joined rooms are saved on exit.
      QVERIFY(exists(roomFile("Kept")));
      QVERIFY(exists(Common::Constants::DIR_CHAT_MESSAGES + '/' + Common::Constants::FILE_CHAT_MESSAGES));
   }

   void roomsFollowPeerIMAlive()
   {
      const auto peers = PM::Builder::newPeerManager({});
      const auto network = QSharedPointer<NetworkListener>::create();
      CS::ChatSystem chat(peers, network);
      chat.joinRoom("Joined");

      const auto peerID = Common::Hash::rand();
      peers->updatePeer(peerID, QHostAddress::LocalHost, 1, "peer", 0, QString(), 0, 0, Common::Constants::PROTOCOL_VERSION);
      auto numberOfPeersByRoom = [&] {
         QMap<QString, int> result;
         for (const auto& room : chat.getRooms())
            result[room.name] = room.peers.size();
         return result;
      };

      network->receiveIMAlive(peerID, { "Joined", "A", "B" });
      QCOMPARE(numberOfPeersByRoom(), (QMap<QString, int> { { "Joined", 1 }, { "A", 1 }, { "B", 1 } }));

      // The peer left 'B': it's forgotten because we haven't joined it, unlike 'Joined'.
      network->receiveIMAlive(peerID, { "A" });
      QCOMPARE(numberOfPeersByRoom(), (QMap<QString, int> { { "Joined", 0 }, { "A", 1 } }));

      network->receiveIMAlive(peerID, {});
      QCOMPARE(numberOfPeersByRoom(), (QMap<QString, int> { { "Joined", 0 } }));
   }

   void joinRoomEmitsSavedHistory()
   {
      QVERIFY(QDir(this->directory->path()).mkpath("chat/rooms"));
      Common::PersistentData::setValue(Common::Constants::DIR_CHAT_MESSAGES + '/' + Common::Constants::FILE_CHAT_MESSAGES,
         message(QString(), 1), FOLDER);
      Common::PersistentData::setValue(roomFile("General"), message("General", 2), FOLDER);

      const auto peers = PM::Builder::newPeerManager({});
      const auto network = QSharedPointer<NetworkListener>::create();
      CS::ChatSystem chat(peers, network);
      QSignalSpy notifications(&chat, &CS::IChatSystem::newMessages);

      Protos::Common::ChatMessages mainChat;
      chat.getLastChatMessages(mainChat);
      QCOMPARE(mainChat.messages_size(), 1);
      QCOMPARE(mainChat.messages(0).id(), quint64(1));

      chat.joinRoom("General");
      QCOMPARE(notifications.size(), 1);
      const auto roomHistory = qvariant_cast<Protos::Common::ChatMessages>(notifications.first().at(0));
      QCOMPARE(roomHistory.messages_size(), 1);
      QCOMPARE(roomHistory.messages(0).id(), quint64(2));

      chat.joinRoom("General"); // Already joined.
      chat.joinRoom("Empty"); // No saved history.
      QCOMPARE(notifications.size(), 1);
   }

   void validateIncomingTimestamps()
   {
      const quint64 now = QDateTime::currentMSecsSinceEpoch();
      auto batch = message(QString(), 1); // Old history remains valid.
      const QList<quint64> times { 0, now + 60000, now + 10 * 60000,
         quint64(std::numeric_limits<qint64>::max()), std::numeric_limits<quint64>::max() };
      for (int i = 0; i < times.size(); ++i)
      {
         auto entry = message(QString(), i + 2);
         entry.mutable_messages(0)->set_time(times[i]);
         batch.MergeFrom(entry);
      }
      CS::ChatMessages history;
      const auto inserted = history.add(batch);
      QCOMPARE(inserted.size(), 3);
      Protos::Common::ChatMessages saved;
      history.fillProtoChatMessages(saved);
      QCOMPARE(saved.messages(0).time(), quint64(1000));
      for (int i = 1; i < saved.messages_size(); ++i)
      {
         QVERIFY(saved.messages(i).time() >= now);
         QVERIFY(saved.messages(i).time() <= quint64(QDateTime::currentMSecsSinceEpoch()));
      }
      // Rejected IDs must remain usable for later valid messages.
      QCOMPARE(history.add(message(QString(), 4)).size(), 1);
   }

   void futureMessagesCannotBlockLiveMessages()
   {
      const auto peers = PM::Builder::newPeerManager({});
      const auto network = QSharedPointer<NetworkListener>::create();
      CS::ChatSystem chat(peers, network);
      QSignalSpy notifications(&chat, &CS::IChatSystem::newMessages);
      Protos::Common::ChatMessages batch;
      const quint64 future = QDateTime::currentMSecsSinceEpoch() + 24 * 60 * 60 * 1000;
      for (int i = 0; i < 500; ++i)
      {
         auto entry = message(QString(), i + 1);
         entry.mutable_messages(0)->set_time(future);
         batch.MergeFrom(entry);
      }
      auto receive = [&](const Protos::Common::ChatMessages& entries) {
         const auto bytes = entries.SerializeAsString();
         emit network->received(Common::Message::readMessageBody(
            Common::MessageHeader(Common::MessageHeader::CORE_CHAT_MESSAGES, bytes.size(), Common::Hash::rand()), bytes.data()));
      };
      receive(batch);
      QVERIFY(notifications.isEmpty());
      auto live = message(QString(), 501);
      live.mutable_messages(0)->clear_time();
      receive(live);
      QCOMPARE(notifications.size(), 1);
      Protos::Common::ChatMessages saved;
      chat.getLastChatMessages(saved);
      QCOMPARE(saved.messages_size(), 1);
      QCOMPARE(saved.messages(0).id(), quint64(501));
   }

   void cleanSavedFutureTimestamps()
   {
      QVERIFY(QDir(this->directory->path()).mkpath("chat/rooms"));
      auto batch = message("General", 1);
      auto skewed = message("General", 2);
      skewed.mutable_messages(0)->set_time(QDateTime::currentMSecsSinceEpoch() + 60000);
      batch.MergeFrom(skewed);
      auto invalid = message("General", 3);
      invalid.mutable_messages(0)->set_time(std::numeric_limits<quint64>::max());
      batch.MergeFrom(invalid);
      Common::PersistentData::setValue(roomFile("General"), batch, FOLDER);
      CS::ChatMessages history;
      history.loadForRoom("General");
      QCOMPARE(history.getMessages().size(), 2);
      history.saveForRoom("General");
      Protos::Common::ChatMessages saved;
      Common::PersistentData::getValue(roomFile("General"), saved, FOLDER);
      QCOMPARE(saved.messages_size(), 2);
      QVERIFY(saved.messages(1).time() <= quint64(QDateTime::currentMSecsSinceEpoch()));
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
