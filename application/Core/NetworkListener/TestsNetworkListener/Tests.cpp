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

#include <MockHashCache.h>

using namespace NL;

static const int DISCOVERY_TIMEOUT = 5000; // [ms]

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

void Tests::cleanupTestCase()
{
   qDebug() << "===== cleanupTestCase() =====";

   this->receivedMessages.clear();
   this->instances.clear();
}
