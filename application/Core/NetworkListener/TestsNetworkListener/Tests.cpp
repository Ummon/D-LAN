/**
  * D-LAN - A decentralized LAN file sharing software.
  * Copyright (C) 2010-2012 Greg Burri <greg.burri@gmail.com>
  *
  * This program is free software: you can redistribute it and/or modify
  * it under the terms of the GNU General Public License as published by
  * the Free Software Foundation, either version 3 of the License, or
  * (at your option) any later version.
  *
  * This program is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  *
  * You should have received a copy of the GNU General Public License
  * along with this program.  If not, see <http://www.gnu.org/licenses/>.
  */

#include <Tests.h>

#include <QTest>
#include <QHostAddress>
#include <QAbstractSocket>
#include <QTcpServer>
#include <QTcpSocket>
#include <QScopeGuard>

#include <Protos/common.pb.h>
#include <Protos/core_protocol.pb.h>

#include <Common/Global.h>
#include <Common/Settings.h>
#include <Common/LogManager/Builder.h>
#include <Common/Network/MessageHeader.h>

#include <Core/FileManager/Builder.h>
#include <Core/PeerManager/Builder.h>
#include <Core/UploadManager/Builder.h>
#include <Core/DownloadManager/Builder.h>
#include <Core/NetworkListener/Builder.h>
#include <Core/NetworkListener/ISearch.h>

#include <priv/Utils.h>
#include <priv/UDPListener.h>
#include <priv/Search.h>
#include <priv/NetworkListener.h>
#include <algorithm>
#include <limits>

#include <MockHashCache.h>

using namespace NL;

static const int DISCOVERY_TIMEOUT = 5000; // [ms]

namespace
{
   class PendingChunk : public DM::IChunkDownloader
   {
   public:
      Common::Hash getHash() const override { return Common::Hash(); }
      void addPeer(PM::IPeer*) override {}
      void rmPeer(PM::IPeer*) override {}
   };

   // Always supplies as many hashes as requested, exercising a full heartbeat.
   class BusyDownloadManager : public DM::IDownloadManager
   {
   public:
      void addDownload(const Protos::Common::Entry&, PM::IPeer*, const Common::Hash&, const QString&) override {}
      void addDownload(const Protos::Common::Entry&, PM::IPeer*, const QString&) override {}
      QList<DM::IDownload*> getDownloads() const override { return {}; }
      void moveDownloads(const QList<quint64>&, const QList<quint64>&, Protos::GUI::MoveDownloads::Position) override {}
      void removeAllCompleteDownloads() override {}
      void removeDownloads(QList<quint64>) override {}
      void pauseDownloads(QList<quint64>, bool) override {}
      int getDownloadRate() override { return 0; }
      QList<QSharedPointer<DM::IChunkDownloader>> getTheFirstUnfinishedChunks(int n) override
      {
         QList<QSharedPointer<DM::IChunkDownloader>> chunks;
         const QSharedPointer<DM::IChunkDownloader> chunk(new PendingChunk());
         for (int i = 0; i < n; ++i)
            chunks << chunk;
         return chunks;
      }
      QList<QSharedPointer<DM::IChunkDownloader>> getTheOldestUnfinishedChunks(int n) override
      {
         return this->getTheFirstUnfinishedChunks(n);
      }
   };
}

Tests::Tests()
{
}

void Tests::initTestCase()
{
   LM::Builder::initMsgHandler();

   qDebug() << "===== initTestCase() =====";
   try
   {
      const QString tempFolder = Common::Global::setCurrentDirToTemp("NetworkListenerTests");
      qDebug() << "Application directory path (where the persistent data is put): " <<
         Common::Global::getDataFolder(Common::Global::DataFolderType::LOCAL, false);
      qDebug() << "The files created during this test are put in: " << tempFolder;
   }
   catch (Common::Global::UnableToSetTempDirException& e)
   {
      QFAIL(e.errorMessage.toUtf8());
   }

   this->peerIDs <<
      Common::Hash::fromStr("11111111111111111111111111111111111111111111111111111111").value() <<
      Common::Hash::fromStr("22222222222222222222222222222222222222222222222222222222").value();

   for (int i = 0; i < this->peerIDs.size(); i++)
      this->instances << this->createInstance(this->peerIDs[i], QString("peer#%1").arg(i + 1));

   connect(this->instances[0].networkListener.data(), &INetworkListener::received, this, [this](const Common::Message& message) {
      this->receivedMessages << message;
   });
}

Tests::Instance Tests::createInstance(const Common::Hash& ID, const QString& nick)
{
   Instance instance;

   instance.hashCache = QSharedPointer<HC::IHashCache>(new MockHashCache());
   instance.fileManager = FM::Builder::newFileManager(instance.hashCache);

   SETTINGS.set("peer_id", ID); // The ID is read by the peer manager when created.
   instance.peerManager = PM::Builder::newPeerManager(instance.fileManager);
   instance.peerManager->setNick(nick);

   instance.uploadManager = UM::Builder::newUploadManager(instance.peerManager);
   instance.downloadManager = DM::Builder::newDownloadManager(instance.fileManager, instance.peerManager);
   instance.networkListener = NL::Builder::newNetworkListener(instance.fileManager, instance.peerManager, instance.uploadManager, instance.downloadManager);

   return instance;
}

/**
  * @return true if each instance knows the other one.
  */
bool Tests::peersDiscovered() const
{
   for (int i = 0; i < this->instances.size(); i++)
      for (int j = 0; j < this->instances.size(); j++)
      {
         if (i == j)
            continue;
         PM::IPeer* peer = this->instances[i].peerManager->getPeer(this->peerIDs[j]);
         if (!peer || !peer->isAvailable())
            return false;
      }
   return true;
}

void Tests::multicastGroupIPv4()
{
   qDebug() << "===== multicastGroupIPv4() =====";

   // 3960285976 = 236.13.43.24, see 'multicast_group' in "Protos/core_settings.proto".
   QCOMPARE(Utils::getMulticastGroup(QAbstractSocket::IPv4Protocol), QHostAddress("236.13.43.24"));
}

void Tests::multicastGroupIPv6()
{
   qDebug() << "===== multicastGroupIPv6() =====";

   const QHostAddress group = Utils::getMulticastGroup(QAbstractSocket::IPv6Protocol);
   QCOMPARE(group.protocol(), QAbstractSocket::IPv6Protocol);

   const Q_IPV6ADDR address = group.toIPv6Address();
   QCOMPARE(address[0], 0xFF); // Multicast.
   QCOMPARE(address[1], 0x12); // Scope: link-local, transient.

   // The last four bytes are the IPv4 group.
   QCOMPARE(address[12], 0xEC);
   QCOMPARE(address[13], 0x0D);
   QCOMPARE(address[14], 0x2B);
   QCOMPARE(address[15], 0x18);

   // The group depends on the channel.
   SETTINGS.set("channel", QString("another channel"));
   QVERIFY(Utils::getMulticastGroup(QAbstractSocket::IPv6Protocol) != group);
   SETTINGS.set("channel", QString("main"));
   QCOMPARE(Utils::getMulticastGroup(QAbstractSocket::IPv6Protocol), group);
}

void Tests::addressToListenTo()
{
   qDebug() << "===== addressToListenTo() =====";

   // No address set: listen to any address.
   const QHostAddress anyAddress = Utils::getCurrentAddressToListenTo();
   QVERIFY(anyAddress == QHostAddress(QHostAddress::AnyIPv4) || anyAddress == QHostAddress(QHostAddress::AnyIPv6));

   // An address that doesn't exist must not be returned and must be reset by the sanitization.
   const QString unknownAddress("198.51.100.42"); // Reserved for documentation (RFC 5737), never assigned to an interface.
   SETTINGS.set("listen_address", unknownAddress);
   QCOMPARE(Utils::getCurrentAddressToListenTo(), anyAddress);
   QCOMPARE(SETTINGS.get<QString>("listen_address"), unknownAddress); // The getter has no side effect.

   Utils::sanitizeListenSettings();
   QVERIFY(SETTINGS.get<QString>("listen_address").isEmpty());

   // An existing address must be returned as is.
   const QString loopback = QHostAddress(QHostAddress::LocalHost).toString();
   SETTINGS.set("listen_address", loopback);
   QCOMPARE(Utils::getCurrentAddressToListenTo(), QHostAddress(loopback));
   Utils::sanitizeListenSettings();
   QCOMPARE(SETTINGS.get<QString>("listen_address"), loopback);

   SETTINGS.set("listen_address", QString(""));
}

void Tests::networkConfigurationSnapshot()
{
   auto interfaces = QNetworkInterface::allInterfaces();
   const auto configuration = Utils::getNetworkConfiguration(interfaces);
   std::reverse(interfaces.begin(), interfaces.end());
   QCOMPARE(Utils::getNetworkConfiguration(interfaces), configuration);
   QVERIFY(Utils::getNetworkConfiguration({}).isEmpty());
   if (!interfaces.isEmpty())
   {
      interfaces.removeLast();
      QVERIFY(Utils::getNetworkConfiguration(interfaces) != configuration);
   }
}

void Tests::sendToUnknownPeer()
{
   qDebug() << "===== sendToUnknownPeer() =====";

   Protos::Core::GetLastChatMessages message;
   message.set_number(1);

   const Common::Hash unknownPeer = Common::Hash::fromStr("33333333333333333333333333333333333333333333333333333333").value();
   QCOMPARE(
      this->instances[0].networkListener->send(Common::MessageHeader::CORE_GET_LAST_CHAT_MESSAGES, message, unknownPeer),
      INetworkListener::SendStatus::PEER_UNKNOWN
   );
}

void Tests::sendMessageTooLarge()
{
   qDebug() << "===== sendMessageTooLarge() =====";

   Protos::Core::Find findMessage;
   findMessage.set_tag(42);
   findMessage.mutable_pattern()->set_pattern(std::string(2 * SETTINGS.get<quint32>("max_udp_datagram_size"), 'a'));

   QCOMPARE(
      this->instances[0].networkListener->send(Common::MessageHeader::CORE_FIND, findMessage),
      INetworkListener::SendStatus::MESSAGE_TOO_LARGE
   );
}

void Tests::effectiveUDPMessageSize()
{
   const quint32 original = SETTINGS.get<quint32>("max_udp_datagram_size");
   const Instance& instance = this->instances[0];
   for (quint32 configured : { quint32(0), quint32(256), std::numeric_limits<quint32>::max() })
   {
      SETTINGS.set("max_udp_datagram_size", configured);
      UDPListener listener(instance.fileManager, instance.peerManager, instance.uploadManager, instance.downloadManager);
      SETTINGS.set("max_udp_datagram_size", original);
      const int expected = configured == 0 ? 0 : (configured == 256 ? 256 : 65536) - Common::MessageHeader::HEADER_SIZE;
      QCOMPARE(listener.getMaxUDPMessageSize(), expected);
      Protos::Core::Find message;
      message.mutable_pattern()->set_pattern(std::string(expected + 1, 'x'));
      QCOMPARE(listener.send(Common::MessageHeader::CORE_FIND, message), INetworkListener::SendStatus::MESSAGE_TOO_LARGE);
   }
}

void Tests::sendMulticast()
{
   qDebug() << "===== sendMulticast() =====";

   Protos::Core::Find findMessage;
   findMessage.set_tag(42);
   findMessage.mutable_pattern()->set_pattern("a pattern");

   QCOMPARE(
      this->instances[0].networkListener->send(Common::MessageHeader::CORE_FIND, findMessage),
      INetworkListener::SendStatus::OK
   );
}

void Tests::peerDiscovery()
{
   qDebug() << "===== peerDiscovery() =====";

#ifndef DEBUG
   QSKIP("The multicast loopback is only enabled in debug, the instances can't discover each other in release");
#endif

   QTRY_VERIFY_WITH_TIMEOUT(this->peersDiscovered(), DISCOVERY_TIMEOUT);

   QList<quint16> advertisedPorts;
   for (int i = 0; i < this->instances.size(); i++)
   {
      const int j = (i + 1) % this->instances.size();
      PM::IPeer* peer = this->instances[i].peerManager->getPeer(this->peerIDs[j]);
      QCOMPARE(peer->getNick(), QString("peer#%1").arg(j + 1));
      QVERIFY(peer->getPort() != 0);
      advertisedPorts << peer->getPort();
   }

   // Both instances run on the same machine, they must listen to different ports.
   QVERIFY(advertisedPorts[0] != advertisedPorts[1]);
}

void Tests::unicastReception()
{
   qDebug() << "===== unicastReception() =====";

   if (!this->peersDiscovered())
      QSKIP("The peers haven't discovered each other");

   this->receivedMessages.clear();

   Protos::Core::GetLastChatMessages message;
   message.set_number(7);

   // Sent by the second instance to the first one.
   QCOMPARE(
      this->instances[1].networkListener->send(Common::MessageHeader::CORE_GET_LAST_CHAT_MESSAGES, message, this->peerIDs[0]),
      INetworkListener::SendStatus::OK
   );

   auto received = [this]() {
      for (const Common::Message& m : std::as_const(this->receivedMessages))
         if (m.getHeader().getType() == Common::MessageHeader::CORE_GET_LAST_CHAT_MESSAGES)
            return true;
      return false;
   };
   QTRY_VERIFY_WITH_TIMEOUT(received(), DISCOVERY_TIMEOUT);

   for (const Common::Message& m : std::as_const(this->receivedMessages))
      if (m.getHeader().getType() == Common::MessageHeader::CORE_GET_LAST_CHAT_MESSAGES)
      {
         QCOMPARE(m.getHeader().getSenderID(), this->peerIDs[1]);
         QCOMPARE(m.getMessage<Protos::Core::GetLastChatMessages>().number(), 7u);
      }
}

void Tests::search()
{
   qDebug() << "===== search() =====";

   if (!this->peersDiscovered())
      QSKIP("The peers haven't discovered each other");

   this->receivedMessages.clear();

   // Launched by the second instance, received by the first one.
   QSharedPointer<ISearch> search = this->instances[1].networkListener->newSearch();

   Protos::Common::FindPattern pattern;
   pattern.set_pattern("something");

   const quint64 tag = search->search(pattern);
   QVERIFY(tag != 0);
   QCOMPARE(search->search(pattern), quint64(0)); // A search can only be launched once.

   auto received = [this, tag]() {
      for (const Common::Message& m : std::as_const(this->receivedMessages))
         if (m.getHeader().getType() == Common::MessageHeader::CORE_FIND && m.getMessage<Protos::Core::Find>().tag() == tag)
            return true;
      return false;
   };
   QTRY_VERIFY_WITH_TIMEOUT(received(), DISCOVERY_TIMEOUT);

   QVERIFY(search->elapsed() >= 0);
}

void Tests::searchResultReception()
{
#ifndef DEBUG
   QSKIP("The multicast loopback is only enabled in debug");
#endif
   QTRY_VERIFY_WITH_TIMEOUT(this->peersDiscovered(), DISCOVERY_TIMEOUT);
   const auto receiver = this->instances[0].networkListener;
   const auto sender = this->instances[1].networkListener;
   const auto firstSearch = receiver->newSearch();
   const auto secondSearch = receiver->newSearch();
   QList<Protos::Common::FindResult> firstResults;
   QList<Protos::Common::FindResult> secondResults;
   int receivedDatagrams = 0;
   QObject context;
   connect(firstSearch.data(), &ISearch::found, &context,
      [&](const Protos::Common::FindResult& result) { firstResults << result; });
   connect(secondSearch.data(), &ISearch::found, &context,
      [&](const Protos::Common::FindResult& result) { secondResults << result; });
   connect(receiver.data(), &INetworkListener::received, &context, [&](const Common::Message& message) {
      if (message.getHeader().getType() == Common::MessageHeader::CORE_FIND_RESULT &&
          message.getHeader().getSenderID() == this->peerIDs[1])
         ++receivedDatagrams;
   });

   Protos::Common::FindPattern pattern;
   pattern.set_pattern("network-listener-result-reception-test");
   const quint64 firstTag = firstSearch->search(pattern);
   const quint64 secondTag = secondSearch->search(pattern);
   QVERIFY(firstTag != 0);
   QVERIFY(secondTag != 0);
   QVERIFY(firstTag != secondTag);

   Protos::Common::FindResult response;
   response.set_tag(0); // Neither search owns this tag.
   // The receiver must attribute results to the datagram sender, not this claimed ID.
   response.mutable_peer_id()->set_hash(this->peerIDs[0].getData(), Common::Hash::HASH_SIZE);
   auto* entry = response.add_entries();
   entry->set_level(7);
   entry->mutable_entry()->set_name("first-result.txt");
   QCOMPARE(sender->send(Common::MessageHeader::CORE_FIND_RESULT, response, this->peerIDs[0]), INetworkListener::SendStatus::OK);
   QTRY_COMPARE_WITH_TIMEOUT(receivedDatagrams, 1, DISCOVERY_TIMEOUT);
   QVERIFY(firstResults.isEmpty());
   QVERIFY(secondResults.isEmpty());

   response.set_tag(firstTag);
   QCOMPARE(sender->send(Common::MessageHeader::CORE_FIND_RESULT, response, this->peerIDs[0]), INetworkListener::SendStatus::OK);
   QTRY_COMPARE_WITH_TIMEOUT(firstResults.size(), 1, DISCOVERY_TIMEOUT);
   QVERIFY(secondResults.isEmpty());
   Protos::Common::FindResult expected(response);
   expected.mutable_peer_id()->set_hash(this->peerIDs[1].getData(), Common::Hash::HASH_SIZE);
   QCOMPARE(firstResults.first().SerializeAsString(), expected.SerializeAsString());

   response.set_tag(secondTag);
   response.mutable_entries(0)->mutable_entry()->set_name("second-result.txt");
   QCOMPARE(sender->send(Common::MessageHeader::CORE_FIND_RESULT, response, this->peerIDs[0]), INetworkListener::SendStatus::OK);
   QTRY_COMPARE_WITH_TIMEOUT(secondResults.size(), 1, DISCOVERY_TIMEOUT);
   QCOMPARE(firstResults.size(), 1);
   expected = response;
   expected.mutable_peer_id()->set_hash(this->peerIDs[1].getData(), Common::Hash::HASH_SIZE);
   QCOMPARE(secondResults.first().SerializeAsString(), expected.SerializeAsString());

   response.set_tag(firstTag);
   QCOMPARE(sender->send(Common::MessageHeader::CORE_FIND_RESULT, response, this->peerIDs[0]), INetworkListener::SendStatus::OK);
   QTRY_COMPARE_WITH_TIMEOUT(firstResults.size(), 2, DISCOVERY_TIMEOUT);
   QCOMPARE(secondResults.size(), 1);
   expected.set_tag(firstTag);
   QCOMPARE(firstResults.last().SerializeAsString(), expected.SerializeAsString());
   QCOMPARE(receivedDatagrams, 4);
}

void Tests::searchSendFailure_data()
{
   QTest::addColumn<bool>("oversized");
   QTest::newRow("oversized request") << true;
   QTest::newRow("unavailable socket") << false;
}

void Tests::searchSendFailure()
{
   QFETCH(bool, oversized);
   const Instance& instance = this->instances[1];
   UDPListener listener(instance.fileManager, instance.peerManager, instance.uploadManager, instance.downloadManager);
   QTcpServer tcp;
   QVERIFY(tcp.listen(Utils::getCurrentAddressToListenTo(), 0));
   QVERIFY(listener.bindUnicastSocket(Utils::getCurrentAddressToListenTo(), tcp.serverPort()));
   QVERIFY(listener.startListening());
   if (!oversized)
      listener.closeSockets();

   Search search(listener);
   QCOMPARE(search.elapsed(), qint64(-1));
   int results = 0;
   connect(&search, &ISearch::found, this, [&](const Protos::Common::FindResult&) { ++results; });
   Protos::Common::FindPattern pattern;
   pattern.set_pattern(oversized ? std::string(listener.getMaxUDPMessageSize() + 1, 'x') : "something");
   QCOMPARE(search.search(pattern), quint64(0));
   QCOMPARE(search.search(pattern), quint64(0));
   QCOMPARE(search.elapsed(), qint64(-1));

   Protos::Common::FindResult result;
   result.set_tag(0);
   result.add_entries();
   emit listener.newFindResultMessage(result);
   QCOMPARE(results, 0);

   if (!oversized)
   {
      QVERIFY(listener.bindUnicastSocket(Utils::getCurrentAddressToListenTo(), tcp.serverPort()));
      QVERIFY(listener.startListening());
   }
   pattern.set_pattern("something");
   const quint64 tag = search.search(pattern);
   QVERIFY(tag != 0);
   QVERIFY(search.elapsed() >= 0);
   QCOMPARE(search.search(pattern), quint64(0));
   result.set_tag(tag);
   emit listener.newFindResultMessage(result);
   QCOMPARE(results, 1); // Failed attempts must not leave duplicate result subscriptions.
}

void Tests::searchResultLimit_data()
{
   QTest::addColumn<int>("initialCount");
   QTest::addColumn<int>("batchCount");
   const int limit = SETTINGS.get<quint32>("max_number_of_result_shown");
   QTest::newRow("partially fitting batch") << limit - 10 << 20;
   QTest::newRow("oversized first batch") << 0 << limit + 1;
   QTest::newRow("exactly fitting batch") << limit - 20 << 20;
   QTest::newRow("already exhausted") << limit << 20;
}

void Tests::searchResultLimit()
{
   QFETCH(int, initialCount);
   QFETCH(int, batchCount);
   const int limit = SETTINGS.get<quint32>("max_number_of_result_shown");
   const Instance& instance = this->instances[1];
   UDPListener listener(instance.fileManager, instance.peerManager, instance.uploadManager, instance.downloadManager);
   QTcpServer tcp;
   QVERIFY(tcp.listen(Utils::getCurrentAddressToListenTo(), 0));
   QVERIFY(listener.bindUnicastSocket(Utils::getCurrentAddressToListenTo(), tcp.serverPort()));
   QVERIFY(listener.startListening());
   Search search(listener);
   Protos::Common::FindPattern pattern;
   pattern.set_pattern("result limit test");
   const quint64 tag = search.search(pattern);
   QVERIFY(tag != 0);

   QList<Protos::Common::FindResult> received;
   connect(&search, &ISearch::found, this, [&](const Protos::Common::FindResult& result) { received << result; });
   if (initialCount > 0)
   {
      Protos::Common::FindResult initial;
      initial.set_tag(tag);
      for (int i = 0; i < initialCount; ++i)
         initial.add_entries();
      emit listener.newFindResultMessage(initial);
      QCOMPARE(received.size(), 1);
      QCOMPARE(received.first().entries_size(), initialCount);
      received.clear();
   }

   Protos::Common::FindResult batch;
   batch.set_tag(tag);
   batch.mutable_peer_id()->set_hash(this->peerIDs[1].getData(), Common::Hash::HASH_SIZE);
   for (int i = 0; i < batchCount; ++i)
   {
      auto* entry = batch.add_entries();
      entry->set_level(i);
      entry->mutable_entry()->set_name(QString::number(i).toStdString());
   }
   const std::string original = batch.SerializeAsString();
   batch.set_tag(tag ^ 1);
   emit listener.newFindResultMessage(batch);
   QVERIFY(received.isEmpty()); // An unrelated search must not consume the remaining allowance.
   batch.set_tag(tag);
   emit listener.newFindResultMessage(batch);
   QCOMPARE(batch.SerializeAsString(), original);

   const int expected = qMin(batchCount, limit - initialCount);
   QCOMPARE(received.size(), expected == 0 ? 0 : 1);
   if (expected > 0)
   {
      const auto& result = received.first();
      QCOMPARE(result.entries_size(), expected);
      QCOMPARE(result.tag(), tag);
      QCOMPARE(result.peer_id().hash(), batch.peer_id().hash());
      for (int i = 0; i < expected; ++i)
         QCOMPARE(result.entries(i).SerializeAsString(), batch.entries(i).SerializeAsString());
   }

   received.clear();
   Protos::Common::FindResult extra;
   extra.set_tag(tag);
   extra.add_entries();
   emit listener.newFindResultMessage(extra);
   QVERIFY(received.isEmpty()); // Every row has now reached the result limit.
}

void Tests::unavailableMulticastPeer_data()
{
   QTest::addColumn<bool>("blocked");
   QTest::newRow("blocked peer") << true;
   QTest::newRow("incompatible peer") << false;
}

void Tests::unavailableMulticastPeer()
{
#ifndef DEBUG
   QSKIP("The multicast loopback is only enabled in debug");
#endif
   QFETCH(bool, blocked);
   QTRY_VERIFY_WITH_TIMEOUT(this->peersDiscovered(), DISCOVERY_TIMEOUT);
   const auto manager = this->instances[0].peerManager;
   PM::IPeer* peer = manager->getPeer(this->peerIDs[1]);
   QVERIFY(peer);
   const auto sender = this->instances[1].networkListener;

   const QHostAddress address = peer->getIP();
   const quint16 port = peer->getPort();
   const QString nick = peer->getNick();
   const QString coreVersion = peer->getCoreVersion();
   const quint64 amount = peer->getSharingAmount();
   const quint32 downloadRate = peer->getDownloadRate();
   const quint32 uploadRate = peer->getUploadRate();
   const quint32 version = peer->getProtocolVersion();
   const auto restore = qScopeGuard([&]() {
      manager->updatePeer(peer->getID(), address, port, nick, amount, coreVersion, downloadRate, uploadRate, version);
      if (blocked)
         peer->block(0);
      QCoreApplication::processEvents();
   });

   auto receivedType = [&](Common::MessageHeader::MessageType type) {
      for (const Common::Message& message : std::as_const(this->receivedMessages))
         if (message.getHeader().getSenderID() == peer->getID() && message.getHeader().getType() == type)
            return true;
      return false;
   };
   Protos::Common::ChatMessages chat;
   chat.add_messages()->set_message("Multicast availability test");
   this->receivedMessages.clear();
   QCOMPARE(sender->send(Common::MessageHeader::CORE_CHAT_MESSAGES, chat), INetworkListener::SendStatus::OK);
   QTRY_VERIFY_WITH_TIMEOUT(receivedType(Common::MessageHeader::CORE_CHAT_MESSAGES), DISCOVERY_TIMEOUT);

   Protos::Core::IMAlive heartbeat;
   heartbeat.set_version(blocked ? version : version + 1);
   heartbeat.set_port(port);
   heartbeat.set_nick("unavailable peer");
   heartbeat.set_tag(123456);
   if (blocked)
      peer->block(60000, "NetworkListener regression test");
   QCOMPARE(sender->send(Common::MessageHeader::CORE_IM_ALIVE, heartbeat), INetworkListener::SendStatus::OK);
   QTRY_COMPARE(peer->getNick(), QString("unavailable peer"));
   QVERIFY(peer->isAlive());
   QVERIFY(!peer->isAvailable());

   this->receivedMessages.clear();
   QCOMPARE(sender->send(Common::MessageHeader::CORE_CHAT_MESSAGES, chat), INetworkListener::SendStatus::OK);
   Protos::Core::Find find;
   find.set_tag(123456);
   find.mutable_pattern()->set_pattern("unavailable peer search");
   QCOMPARE(sender->send(Common::MessageHeader::CORE_FIND, find), INetworkListener::SendStatus::OK);
   // A later heartbeat must still reach consumers and update the unavailable peer.
   heartbeat.set_nick("still unavailable");
   QCOMPARE(sender->send(Common::MessageHeader::CORE_IM_ALIVE, heartbeat), INetworkListener::SendStatus::OK);
   QTRY_COMPARE(peer->getNick(), QString("still unavailable"));
   QTest::qWait(50);
   QVERIFY(receivedType(Common::MessageHeader::CORE_IM_ALIVE));
   QVERIFY(!receivedType(Common::MessageHeader::CORE_CHAT_MESSAGES));
   QVERIFY(!receivedType(Common::MessageHeader::CORE_FIND));
   QVERIFY(!peer->isAvailable());

   QCOMPARE(sender->send(Common::MessageHeader::CORE_GOODBYE, Protos::Common::Null()), INetworkListener::SendStatus::OK);
   QTRY_VERIFY(!peer->isAlive());
   QVERIFY(receivedType(Common::MessageHeader::CORE_GOODBYE));
}

void Tests::heartbeatWithChatRooms_data()
{
   QTest::addColumn<QStringList>("roomNames");
   QTest::addColumn<bool>("expectOmissions");

   QTest::newRow("ordinary rooms") << QStringList { "general", "development" } << false;
   QTest::newRow("UTF-8 rooms") << QStringList { QString(200, QChar(0x20AC)), QString(200, QChar(0x00E9)) } << false;
   QTest::newRow("oversized room") << QStringList { QString(20000, 'a') } << true;
   QStringList manyRooms;
   for (int i = 0; i < 100; ++i)
      manyRooms << QString::number(i) + QString(200, QChar(0x20AC));
   QTest::newRow("too many rooms") << manyRooms << true;
}

void Tests::heartbeatWithChatRooms()
{
#ifndef DEBUG
   QSKIP("The multicast loopback is only enabled in debug");
#endif
   QFETCH(QStringList, roomNames);
   QFETCH(bool, expectOmissions);

   this->receivedMessages.clear();
   quint64 tag = 0;
   const Instance& instance = this->instances[1];
   const auto busyDownloads = QSharedPointer<DM::IDownloadManager>(new BusyDownloadManager());
   const auto networkListener = NL::Builder::newNetworkListener(
      instance.fileManager, instance.peerManager, instance.uploadManager, busyDownloads);
   // A scoped context disconnects the callback even if an assertion fails.
   QObject context;
   connect(networkListener.data(), &INetworkListener::IMAliveMessageToBeSend,
      &context, [&](Protos::Core::IMAlive& message) {
         tag = message.tag();
         for (const QString& room : roomNames)
            message.add_chat_rooms(room.toStdString());
      });

   auto receivedHeartbeat = [&]() {
      for (const Common::Message& message : std::as_const(this->receivedMessages))
         if (message.getHeader().getType() == Common::MessageHeader::CORE_IM_ALIVE &&
             message.getHeader().getSenderID() == this->peerIDs[1] &&
             message.getMessage<Protos::Core::IMAlive>().tag() == tag)
            return true;
      return false;
   };
   QTRY_VERIFY_WITH_TIMEOUT(receivedHeartbeat(), 2 * DISCOVERY_TIMEOUT);

   for (const Common::Message& message : std::as_const(this->receivedMessages))
   {
      if (message.getHeader().getType() != Common::MessageHeader::CORE_IM_ALIVE ||
          message.getHeader().getSenderID() != this->peerIDs[1])
         continue;
      const auto& heartbeat = message.getMessage<Protos::Core::IMAlive>();
      if (heartbeat.tag() != tag)
         continue;
      QVERIFY(heartbeat.ByteSizeLong() + Common::MessageHeader::HEADER_SIZE <= SETTINGS.get<quint32>("max_udp_datagram_size"));
      QCOMPARE(heartbeat.chat_rooms_size() < roomNames.size(), expectOmissions);
      for (int i = 0; i < heartbeat.chat_rooms_size(); ++i)
         QCOMPARE(QString::fromStdString(heartbeat.chat_rooms(i)), roomNames[i]);
      if (roomNames.size() > 1)
         QVERIFY(heartbeat.chat_rooms_size() > 0);
      if (!expectOmissions)
         QVERIFY(heartbeat.chunks_size() > 0);
   }
}

void Tests::sharedUnicastPort_data()
{
   QTest::addColumn<bool>("occupyTCP");
   QTest::addColumn<bool>("occupyUDP");
   QTest::addColumn<bool>("forceFallback");
   QTest::newRow("TCP conflict") << true << false << false;
   QTest::newRow("UDP conflict") << false << true << false;
   QTest::newRow("both occupied") << true << true << false;
   QTest::newRow("OS-selected fallback") << false << false << true;
}

void Tests::sharedUnicastPort()
{
   QFETCH(bool, occupyTCP);
   QFETCH(bool, occupyUDP);
   QFETCH(bool, forceFallback);

   const quint32 originalPort = SETTINGS.get<quint32>("unicast_base_port");
   const quint32 originalProtocol = SETTINGS.get<quint32>("listen_any");
   const QString originalAddress = SETTINGS.get<QString>("listen_address");
   const auto restore = qScopeGuard([&]() {
      SETTINGS.set("unicast_base_port", originalPort);
      SETTINGS.set("listen_any", originalProtocol);
      SETTINGS.set("listen_address", originalAddress);
   });
   SETTINGS.set("listen_any", static_cast<quint32>(Protos::Common::Interface::Address::IPv4));
   SETTINGS.set("listen_address", QString());

   QTcpServer tcpBlocker;
   QVERIFY(tcpBlocker.listen(QHostAddress::AnyIPv4, 0));
   const quint16 occupiedPort = tcpBlocker.serverPort();
   QUdpSocket udpBlocker;
   if (occupyUDP)
      QVERIFY(udpBlocker.bind(QHostAddress::AnyIPv4, occupiedPort, QUdpSocket::DontShareAddress));
   if (!occupyTCP)
      tcpBlocker.close();
   SETTINGS.set("unicast_base_port", forceFallback ? std::numeric_limits<quint32>::max() : quint32(occupiedPort));

   const Instance& instance = this->instances[1];
   const auto listener = NL::Builder::newNetworkListener(
      instance.fileManager, instance.peerManager, instance.uploadManager, instance.downloadManager);
   quint16 advertisedPort = 0;
   QObject context;
   connect(listener.data(), &INetworkListener::IMAliveMessageToBeSend, &context,
      [&](Protos::Core::IMAlive& message) { advertisedPort = message.port(); });
   QTRY_VERIFY(advertisedPort != 0);
   if (!forceFallback)
      QVERIFY(advertisedPort != occupiedPort);

   QTcpSocket client;
   client.connectToHost(QHostAddress::LocalHost, advertisedPort);
   QVERIFY(client.waitForConnected(1000));
   QUdpSocket probe;
   QVERIFY(!probe.bind(QHostAddress::AnyIPv4, advertisedPort, QUdpSocket::DontShareAddress));

   // Rebinding must release our own sockets before attempting the same port again.
   SETTINGS.set("unicast_base_port", quint32(advertisedPort));
   const quint16 previousPort = advertisedPort;
   advertisedPort = 0;
   listener->rebindSockets();
   QTRY_COMPARE(advertisedPort, previousPort);
}

void Tests::bindFailureAndRecovery()
{
   const quint32 originalPort = SETTINGS.get<quint32>("unicast_base_port");
   const quint32 originalMulticastPort = SETTINGS.get<quint32>("multicast_port");
   const quint32 originalProtocol = SETTINGS.get<quint32>("listen_any");
   const QString originalAddress = SETTINGS.get<QString>("listen_address");
   const auto restore = qScopeGuard([&]() {
      SETTINGS.set("unicast_base_port", originalPort);
      SETTINGS.set("multicast_port", originalMulticastPort);
      SETTINGS.set("listen_any", originalProtocol);
      SETTINGS.set("listen_address", originalAddress);
   });
   SETTINGS.set("listen_any", static_cast<quint32>(Protos::Common::Interface::Address::IPv4));
   SETTINGS.set("listen_address", QString());

   QUdpSocket multicastBlocker;
   QVERIFY(multicastBlocker.bind(QHostAddress::AnyIPv4, 0, QUdpSocket::DontShareAddress));
   SETTINGS.set("multicast_port", quint32(multicastBlocker.localPort()));
   QTcpServer tcpProbe;
   QVERIFY(tcpProbe.listen(QHostAddress::AnyIPv4, 0));
   const quint16 port = tcpProbe.serverPort();
   QUdpSocket udpProbe;
   QVERIFY(udpProbe.bind(QHostAddress::AnyIPv4, port, QUdpSocket::DontShareAddress));
   tcpProbe.close();
   udpProbe.close();
   SETTINGS.set("unicast_base_port", quint32(port));

   const Instance& instance = this->instances[1];
   const auto listener = NL::Builder::newNetworkListener(
      instance.fileManager, instance.peerManager, instance.uploadManager, instance.downloadManager);
   int heartbeats = 0;
   QObject context;
   connect(listener.data(), &INetworkListener::IMAliveMessageToBeSend, &context,
      [&](Protos::Core::IMAlive&) { ++heartbeats; });
   QCoreApplication::processEvents();
   QCOMPARE(heartbeats, 0);
   QCOMPARE(listener->send(Common::MessageHeader::CORE_GOODBYE, Protos::Common::Null()),
      INetworkListener::SendStatus::UNABLE_TO_SEND);
   // Multicast failure must roll back both unicast bindings.
   QVERIFY(tcpProbe.listen(QHostAddress::AnyIPv4, port));
   QVERIFY(udpProbe.bind(QHostAddress::AnyIPv4, port, QUdpSocket::DontShareAddress));
   tcpProbe.close();
   udpProbe.close();

   multicastBlocker.close();
   // Recover even when the interface configuration has not changed.
   QTRY_COMPARE_WITH_TIMEOUT(heartbeats, 1, 3500);
   listener->rebindSockets();
   listener->rebindSockets(); // Replace, rather than duplicate, the queued startup heartbeat.
   QTRY_COMPARE(heartbeats, 2);
   QCOMPARE(listener->send(Common::MessageHeader::CORE_GOODBYE, Protos::Common::Null()),
      INetworkListener::SendStatus::OK);

   // A failed unicast rebind must also suppress an already queued startup heartbeat.
   UDPListener udp(instance.fileManager, instance.peerManager, instance.uploadManager, instance.downloadManager);
   QVERIFY(tcpProbe.listen(QHostAddress::AnyIPv4, 0));
   QVERIFY(udp.bindUnicastSocket(QHostAddress::AnyIPv4, tcpProbe.serverPort()));
   QVERIFY(udp.startListening());
   int stoppedHeartbeats = 0;
   connect(&udp, &UDPListener::IMAliveMessageToBeSend, &context,
      [&](Protos::Core::IMAlive&) { ++stoppedHeartbeats; });
   QUdpSocket unicastBlocker;
   QVERIFY(unicastBlocker.bind(QHostAddress::AnyIPv4, 0, QUdpSocket::DontShareAddress));
   QVERIFY(!udp.bindUnicastSocket(QHostAddress::AnyIPv4, unicastBlocker.localPort()));
   QCoreApplication::processEvents();
   QCOMPARE(stoppedHeartbeats, 0);
   QCOMPARE(udp.send(Common::MessageHeader::CORE_GOODBYE), INetworkListener::SendStatus::UNABLE_TO_SEND);
}

void Tests::rejectZeroUnicastPort()
{
   const Instance& instance = this->instances[1];
   UDPListener listener(instance.fileManager, instance.peerManager, instance.uploadManager, instance.downloadManager);
   QVERIFY(!listener.bindUnicastSocket(QHostAddress::AnyIPv4, 0));
   QVERIFY(!listener.startListening());
   QCOMPARE(listener.send(Common::MessageHeader::CORE_GOODBYE), INetworkListener::SendStatus::UNABLE_TO_SEND);
}

void Tests::automaticRebinding()
{
   const Instance& instance = this->instances[1];
   QStringList configuration { "initial interface configuration" };
   NL::NetworkListener listener(instance.fileManager, instance.peerManager, instance.uploadManager,
      instance.downloadManager, [&]() { return configuration; });
   int heartbeats = 0;
   quint16 port = 0;
   connect(&listener, &INetworkListener::IMAliveMessageToBeSend, this,
      [&](Protos::Core::IMAlive& message) { ++heartbeats; port = message.port(); });
   QTRY_COMPARE(heartbeats, 1);
   QVERIFY(port != 0);

   auto check = [&]() { return QMetaObject::invokeMethod(&listener, "checkNetworkConfiguration", Qt::DirectConnection); };
   QVERIFY(check());
   QCoreApplication::processEvents();
   QCOMPARE(heartbeats, 1); // No repeated discovery or peer reset for an unchanged configuration.

   configuration << "changed address or interface flags";
   // Exercise the actual polling timer, without resetting a physical adapter.
   QTRY_COMPARE_WITH_TIMEOUT(heartbeats, 2, 3500);
   QTcpSocket tcp;
   const auto address = Utils::getCurrentAddressToListenTo();
   tcp.connectToHost(address == QHostAddress(QHostAddress::AnyIPv4) ? QHostAddress(QHostAddress::LocalHost) :
      address == QHostAddress(QHostAddress::AnyIPv6) ? QHostAddress(QHostAddress::LocalHostIPv6) : address, port);
   QVERIFY(tcp.waitForConnected(1000));
   QVERIFY(check());
   QCoreApplication::processEvents();
   QCOMPARE(heartbeats, 2);

   // Manual rebinding also updates the baseline, avoiding an extra automatic rebind.
   configuration << "another change";
   listener.rebindSockets();
   QTRY_COMPARE(heartbeats, 3);
   QVERIFY(check());
   QCoreApplication::processEvents();
   QCOMPARE(heartbeats, 3);

   const QString originalAddress = SETTINGS.get<QString>("listen_address");
   const auto restore = qScopeGuard([&]() { SETTINGS.set("listen_address", originalAddress); });
   const QString unavailableAddress("198.51.100.42");
   SETTINGS.set("listen_address", unavailableAddress);
   configuration.clear(); // Simulate an adapter disappearing with a selected address.
   QVERIFY(check());
   QCOMPARE(SETTINGS.get<QString>("listen_address"), unavailableAddress);
   QCOMPARE(listener.send(Common::MessageHeader::CORE_GOODBYE, Protos::Common::Null()),
      INetworkListener::SendStatus::UNABLE_TO_SEND);
   QVERIFY(check());
   QCoreApplication::processEvents();
   QCOMPARE(heartbeats, 3);

   SETTINGS.set("listen_address", originalAddress);
   configuration << "interface restored";
   QVERIFY(check());
   QTRY_COMPARE(heartbeats, 4);
   QCOMPARE(listener.send(Common::MessageHeader::CORE_GOODBYE, Protos::Common::Null()),
      INetworkListener::SendStatus::OK);
}

void Tests::cleanupTestCase()
{
   qDebug() << "===== cleanupTestCase() =====";

   this->receivedMessages.clear();
   this->instances.clear();
}
