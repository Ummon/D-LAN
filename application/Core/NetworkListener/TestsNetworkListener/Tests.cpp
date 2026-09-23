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
#include <QNetworkDatagram>
#include <QTcpServer>
#include <QTcpSocket>
#include <QScopeGuard>
#include <QTemporaryDir>
#include <Common/Constants.h>

#include <Protos/common.pb.h>
#include <Protos/core_protocol.pb.h>

#include <Common/Global.h>
#include <Common/Settings.h>
#include <Common/LogManager/Builder.h>
#include <Common/Network/MessageHeader.h>
#include <Common/Network/InterfacePolicy.h>

#include <Core/FileManager/Builder.h>
#include <Core/FileManager/IDataWriter.h>
#include <Core/PeerManager/Builder.h>
#include <Core/UploadManager/Builder.h>
#include <Core/DownloadManager/Builder.h>
#include <Core/DownloadManager/IDownload.h>
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
   // This address must be identical on Windows, Linux and Darwin, including
   // after the kernel embeds and clears a link-local interface scope.
   QCOMPARE(group, QHostAddress("ff12:0:318b:bc3f:d75a:c873:ec0d:2b18"));

   const Q_IPV6ADDR address = group.toIPv6Address();
   QCOMPARE(address[0], 0xFF); // Multicast.
   QCOMPARE(address[1], 0x12); // Scope: link-local, transient.
   QCOMPARE(address[2], 0);
   QCOMPARE(address[3], 0);

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

void Tests::multicastDestinationIPv6()
{
   const auto interfaces = Utils::getCurrentInterfacesToListenTo();
   if (interfaces.isEmpty())
      QSKIP("No active multicast interface for IPv6");
   const auto iface = interfaces.first();

   const auto group = Utils::getMulticastGroup(QAbstractSocket::IPv6Protocol);
   QUdpSocket receiver;
   QVERIFY(receiver.bind(QHostAddress::AnyIPv6, 0));
   QVERIFY(receiver.joinMulticastGroup(group, iface));

   QUdpSocket sender;
   QVERIFY(sender.bind(QHostAddress::AnyIPv6, 0));
   sender.setMulticastInterface(iface);
   sender.setSocketOption(QAbstractSocket::MulticastLoopbackOption, 1);
   const QByteArray payload("IPv6 multicast destination regression");
   QCOMPARE(sender.writeDatagram(payload, group, receiver.localPort()), qint64(payload.size()));
   QTRY_VERIFY_WITH_TIMEOUT(receiver.hasPendingDatagrams(), 2000);
   const auto datagram = receiver.receiveDatagram();
   QCOMPARE(datagram.data(), payload);
   auto destination = datagram.destinationAddress();
   destination.setScopeId(QString()); // Interface scope is not part of the group ID.
   // Merely receiving our own packet is insufficient: Darwin can deliver it
   // locally even when the destination differs from the requested group.
   QCOMPARE(destination, group);
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

void Tests::ipv6LoopbackFallback()
{
   const QString originalAddress = SETTINGS.get<QString>("listen_address");
   const quint32 originalProtocol = SETTINGS.get<quint32>("listen_any");
   const auto restore = qScopeGuard([&]() {
      SETTINGS.set("listen_address", originalAddress);
      SETTINGS.set("listen_any", originalProtocol);
   });
   const quint32 ipv6 = Protos::Common::Interface::Address::IPv6;
   const quint32 ipv4 = Protos::Common::Interface::Address::IPv4;
   SETTINGS.set("listen_address", QString());
   SETTINGS.set("listen_any", ipv6);

   QList<QNetworkInterface> loopbackInterfaces;
   QList<QNetworkInterface> ipv6LANInterfaces;
   QString loopbackIPv6;
   for (const auto& interface : QNetworkInterface::allInterfaces())
      for (const auto& entry : interface.addressEntries())
         if (entry.ip().protocol() == QAbstractSocket::IPv6Protocol)
         {
            if (interface.flags().testFlag(QNetworkInterface::IsLoopBack))
            {
               loopbackInterfaces << interface;
               loopbackIPv6 = entry.ip().toString();
            }
            else if (Common::isDefaultMulticastInterface(interface))
               ipv6LANInterfaces << interface;
            break;
         }
   if (loopbackInterfaces.isEmpty())
      QSKIP("No IPv6 loopback interface to exercise loopback-only fallback");

   // Supply interface snapshots without changing the host's adapters. IPv6
   // loopback must not keep discovery on IPv6 after the LAN adapter disappears.
   if (!ipv6LANInterfaces.isEmpty())
      QCOMPARE(Utils::getCurrentAddressToListenTo(ipv6LANInterfaces), QHostAddress(QHostAddress::AnyIPv6));
   QCOMPARE(Utils::getCurrentAddressToListenTo(loopbackInterfaces), QHostAddress(QHostAddress::AnyIPv4));
   QCOMPARE(Utils::getCurrentAddressToListenTo({}), QHostAddress(QHostAddress::AnyIPv4));
   QCOMPARE(SETTINGS.get<quint32>("listen_any"), ipv6); // Automatic rebinding preserves the preference.
   if (!ipv6LANInterfaces.isEmpty())
      QCOMPARE(Utils::getCurrentAddressToListenTo(ipv6LANInterfaces), QHostAddress(QHostAddress::AnyIPv6));

   Utils::sanitizeListenSettings(loopbackInterfaces);
   QCOMPARE(SETTINGS.get<quint32>("listen_any"), ipv4);

   // Explicit IPv6 loopback remains valid even without a multicast LAN adapter.
   SETTINGS.set("listen_any", ipv6);
   SETTINGS.set("listen_address", loopbackIPv6);
   Utils::sanitizeListenSettings(loopbackInterfaces);
   QCOMPARE(SETTINGS.get<QString>("listen_address"), loopbackIPv6);
   QCOMPARE(SETTINGS.get<quint32>("listen_any"), ipv6);
   QCOMPARE(Utils::getCurrentAddressToListenTo(loopbackInterfaces), QHostAddress(loopbackIPv6));

   // Losing an explicitly selected address also applies the corrected fallback.
   SETTINGS.set("listen_address", QString("198.51.100.42"));
   Utils::sanitizeListenSettings(loopbackInterfaces);
   QVERIFY(SETTINGS.get<QString>("listen_address").isEmpty());
   QCOMPARE(SETTINGS.get<quint32>("listen_any"), ipv4);
   QCOMPARE(Utils::getCurrentAddressToListenTo(loopbackInterfaces), QHostAddress(QHostAddress::AnyIPv4));
}

void Tests::macOSInterfaceSelection()
{
#ifndef Q_OS_MACOS
   QSKIP("macOS interface policy");
#else
   const QString originalAddress = SETTINGS.get<QString>("listen_address");
   const quint32 originalProtocol = SETTINGS.get<quint32>("listen_any");
   const auto restore = qScopeGuard([&]() {
      SETTINGS.set("listen_address", originalAddress);
      SETTINGS.set("listen_any", originalProtocol);
   });
   SETTINGS.set("listen_address", QString());
   SETTINGS.set("listen_any", quint32(Protos::Common::Interface::Address::IPv6));
   const auto selected = Utils::getCurrentInterfacesToListenTo();
   QList<QNetworkInterface> excluded;
   for (const auto& interface : QNetworkInterface::allInterfaces())
   {
      const auto name = interface.name();
      if (!name.startsWith("awdl") && !name.startsWith("llw") && !name.startsWith("utun") &&
          !name.startsWith("gif") && !name.startsWith("stf") &&
          !interface.flags().testFlag(QNetworkInterface::IsPointToPoint))
         continue;
      excluded << interface;
      for (const auto& chosen : selected)
         QVERIFY(chosen.index() != interface.index());

      // An existing explicit tunnel selection must remain usable.
      if (name.startsWith("utun") && !interface.addressEntries().isEmpty())
      {
         SETTINGS.set("listen_address", interface.addressEntries().first().ip().toString());
         Utils::sanitizeListenSettings();
         const auto explicitSelection = Utils::getCurrentInterfacesToListenTo();
         QCOMPARE(explicitSelection.size(), 1);
         QCOMPARE(explicitSelection.first().index(), interface.index());
         SETTINGS.set("listen_address", QString());
      }
   }
   if (excluded.isEmpty())
      QSKIP("No macOS auxiliary or tunnel interfaces available");
   // Auxiliary/tunnel IPv6 addresses alone must not enable IPv6 LAN discovery.
   QCOMPARE(Utils::getCurrentAddressToListenTo(excluded), QHostAddress(QHostAddress::AnyIPv4));
   Utils::sanitizeListenSettings(excluded);
   QCOMPARE(SETTINGS.get<quint32>("listen_any"), quint32(Protos::Common::Interface::Address::IPv4));
#endif
}

void Tests::networkConfigurationSnapshot()
{
   const QString originalAddress = SETTINGS.get<QString>("listen_address");
   const auto restore = qScopeGuard([&]() { SETTINGS.set("listen_address", originalAddress); });
   SETTINGS.set("listen_address", QString());

   auto interfaces = QNetworkInterface::allInterfaces();
   const auto configuration = Utils::getNetworkConfiguration(interfaces);
   std::reverse(interfaces.begin(), interfaces.end());
   QCOMPARE(Utils::getNetworkConfiguration(interfaces), configuration);
   QCOMPARE(Utils::getNetworkConfiguration({}), QStringList { QHostAddress(QHostAddress::AnyIPv4).toString() });

   // Interfaces not listened to, like loopback or virtual adapters that are down, must not trigger a rebinding.
   QList<QNetworkInterface> selected = Utils::getCurrentInterfacesToListenTo(interfaces);
   QCOMPARE(Utils::getNetworkConfiguration(selected), configuration);

   // Losing a listened interface must.
   if (!selected.isEmpty())
   {
      selected.removeLast();
      QVERIFY(Utils::getNetworkConfiguration(selected) != configuration);
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

void Tests::multicastOnLANInterface_data()
{
   QTest::addColumn<quint32>("protocol");
   QTest::newRow("IPv4") << quint32(Protos::Common::Interface::Address::IPv4);
   QTest::newRow("IPv6") << quint32(Protos::Common::Interface::Address::IPv6);
}

void Tests::multicastOnLANInterface()
{
#if !DEBUG
   QSKIP("The multicast loopback is only enabled in debug; this test sends and receives on the same host");
#endif

   QFETCH(quint32, protocol);
   const QString originalAddress = SETTINGS.get<QString>("listen_address");
   const quint32 originalProtocol = SETTINGS.get<quint32>("listen_any");
   const auto restore = qScopeGuard([&]() {
      SETTINGS.set("listen_address", originalAddress);
      SETTINGS.set("listen_any", originalProtocol);
   });
   SETTINGS.set("listen_address", QString());
   SETTINGS.set("listen_any", protocol);
   const QHostAddress address = Utils::getCurrentAddressToListenTo();
   if (protocol == Protos::Common::Interface::Address::IPv6 && address.protocol() != QAbstractSocket::IPv6Protocol)
      QSKIP("IPv6 is unavailable");
   if (Utils::getCurrentInterfacesToListenTo().isEmpty())
      QSKIP("No active multicast LAN interface for this protocol");
   const auto group = Utils::getMulticastGroup(address.protocol());
   const auto& instance = this->instances[0];
   // This temporary listener must not advertise another instance's ID on its temporary port.
   const Common::Hash originalID = SETTINGS.get<Common::Hash>("peer_id");
   SETTINGS.set("peer_id", Common::Hash::rand());
   const auto peerManager = PM::Builder::newPeerManager(instance.fileManager);
   SETTINGS.set("peer_id", originalID);
   QTcpServer tcp;
   QVERIFY(tcp.listen(address, 0));
   UDPListener listener(instance.fileManager, peerManager, instance.uploadManager, instance.downloadManager);
   QVERIFY(listener.bindUnicastSocket(address, tcp.serverPort()));
   QVERIFY(listener.startListening(Utils::getCurrentInterfacesToListenTo()));

   int checked = 0;
   for (const auto& iface : QNetworkInterface::allInterfaces())
   {
      if (!Common::isDefaultMulticastInterface(iface))
         continue;
      QHostAddress sourceAddress;
      for (const auto& entry : iface.addressEntries())
         if (entry.ip().protocol() == address.protocol())
         {
            sourceAddress = entry.ip();
            break;
         }
      if (sourceAddress.isNull())
         continue;
      ++checked;

      // An explicit address must still select only its own adapter.
      SETTINGS.set("listen_address", sourceAddress.toString());
      const auto selected = Utils::getCurrentInterfacesToListenTo();
      QCOMPARE(selected.size(), 1);
      QCOMPARE(selected.first().index(), iface.index());
      SETTINGS.set("listen_address", QString());

      // Observe outgoing packets on this specific adapter. Checking the receive
      // interface avoids accepting a packet looped back through another adapter.
      QUdpSocket observer;
      QVERIFY(observer.bind(address, SETTINGS.get<quint32>("multicast_port"),
         QUdpSocket::ShareAddress | QUdpSocket::ReuseAddressHint));
      QVERIFY(observer.joinMulticastGroup(group, iface));
      QCOMPARE(listener.send(Common::MessageHeader::CORE_GOODBYE), INetworkListener::SendStatus::OK);
      bool observed = false;
      const auto sentOnInterface = [&]() {
         while (observer.hasPendingDatagrams())
         {
            const auto packet = observer.receiveDatagram();
            if (packet.interfaceIndex() != uint(iface.index()) || packet.data().size() < Common::MessageHeader::HEADER_SIZE)
               continue;
            const auto header = Common::MessageHeader::readHeader(packet.data().constData());
            if (header.getType() == Common::MessageHeader::CORE_GOODBYE && header.getSenderID() == peerManager->getSelf()->getID())
               observed = true;
         }
         return observed;
      };
      QTRY_VERIFY_WITH_TIMEOUT(sentOnInterface(), 2000);

      QUdpSocket sender;
      QVERIFY(sender.bind(sourceAddress, 0));
      sender.setMulticastInterface(iface);
      sender.setSocketOption(QAbstractSocket::MulticastLoopbackOption, 1);
      const Common::Hash ID = Common::Hash::rand();
      Protos::Core::IMAlive heartbeat;
      heartbeat.set_version(Common::Constants::PROTOCOL_VERSION);
      heartbeat.set_port(sender.localPort());
      heartbeat.set_nick("LAN interface test");
      QByteArray datagram(1024, Qt::Uninitialized);
      const Common::MessageHeader header(Common::MessageHeader::CORE_IM_ALIVE, heartbeat.ByteSizeLong(), ID);
      const int size = Common::Message::writeMessageToBuffer(datagram.data(), datagram.size(), header, &heartbeat);
      QVERIFY(size > 0);
      bool received = false;
      QObject context;
      connect(&listener, &UDPListener::received, &context, [&](const Common::Message& message) {
         if (message.getHeader().getSenderID() == ID)
            received = true;
      });
      QCOMPARE(sender.writeDatagram(datagram.constData(), size, group, SETTINGS.get<quint32>("multicast_port")), size);
      QTRY_VERIFY_WITH_TIMEOUT(received, 2000);
      QVERIFY(peerManager->getPeer(ID)->isAvailable());
   }
   qInfo() << "Checked multicast reception and transmission on" << checked << "LAN adapters";
   if (checked == 0)
      QSKIP("No active multicast LAN interface for this protocol");
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
   QVERIFY(listener.startListening(Utils::getCurrentInterfacesToListenTo()));
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
      QVERIFY(listener.startListening(Utils::getCurrentInterfacesToListenTo()));
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
   QVERIFY(listener.startListening(Utils::getCurrentInterfacesToListenTo()));
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

void Tests::invalidIMAlivePort_data()
{
   QTest::addColumn<quint32>("invalidPort");
   QTest::newRow("zero") << quint32(0);
   QTest::newRow("above 65535") << quint32(65536 + 1234);
}

void Tests::invalidIMAlivePort()
{
#ifndef DEBUG
   QSKIP("The multicast loopback is only enabled in debug");
#endif
   QFETCH(quint32, invalidPort);
   QTRY_VERIFY_WITH_TIMEOUT(this->peersDiscovered(), DISCOVERY_TIMEOUT);
   PM::IPeer* peer = this->instances[0].peerManager->getPeer(this->peerIDs[1]);
   QVERIFY(peer);
   const auto sender = this->instances[1].networkListener;
   const quint16 port = peer->getPort();

   auto receivedNick = [&](const std::string& nick) {
      for (const Common::Message& message : std::as_const(this->receivedMessages))
         if (message.getHeader().getSenderID() == peer->getID() &&
             message.getHeader().getType() == Common::MessageHeader::CORE_IM_ALIVE &&
             message.getMessage<Protos::Core::IMAlive>().nick() == nick)
            return true;
      return false;
   };

   Protos::Core::IMAlive heartbeat;
   heartbeat.set_version(peer->getProtocolVersion());
   heartbeat.set_nick("invalid port");
   heartbeat.set_port(invalidPort);
   this->receivedMessages.clear();
   QCOMPARE(sender->send(Common::MessageHeader::CORE_IM_ALIVE, heartbeat), INetworkListener::SendStatus::OK);

   // A valid heartbeat sent afterwards marks the end of the invalid one's processing.
   heartbeat.set_nick("valid port");
   heartbeat.set_port(port);
   QCOMPARE(sender->send(Common::MessageHeader::CORE_IM_ALIVE, heartbeat), INetworkListener::SendStatus::OK);
   QTRY_VERIFY_WITH_TIMEOUT(receivedNick("valid port"), DISCOVERY_TIMEOUT);

   QVERIFY(!receivedNick("invalid port"));
   QCOMPARE(peer->getPort(), port);
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
   QStringList shortRooms;
   for (int i = 0; i < 4000; ++i)
      shortRooms << QString("%1").arg(i, 4, 10, QChar('0'));
   QTest::newRow("short rooms filling hash space") << shortRooms << true;
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
      QVERIFY(heartbeat.chunks_size() >= 4);
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
   QCOMPARE(instance.peerManager->getSelf()->getPort(), advertisedPort);
   QCOMPARE(instance.peerManager->getSelf()->getIP(), QHostAddress(QHostAddress::LocalHost));
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
   QCOMPARE(instance.peerManager->getSelf()->getPort(), previousPort);
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
   QVERIFY(!instance.peerManager->getSelf()->isAvailable());
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
   QVERIFY(instance.peerManager->getSelf()->isAvailable());
   listener->rebindSockets();
   listener->rebindSockets(); // Replace, rather than duplicate, the queued startup heartbeat.
   QTRY_COMPARE(heartbeats, 2);
   QCOMPARE(listener->send(Common::MessageHeader::CORE_GOODBYE, Protos::Common::Null()),
      INetworkListener::SendStatus::OK);

   // A failed unicast rebind must also suppress an already queued startup heartbeat.
   UDPListener udp(instance.fileManager, instance.peerManager, instance.uploadManager, instance.downloadManager);
   QVERIFY(tcpProbe.listen(QHostAddress::AnyIPv4, 0));
   QVERIFY(udp.bindUnicastSocket(QHostAddress::AnyIPv4, tcpProbe.serverPort()));
   QVERIFY(udp.startListening(Utils::getCurrentInterfacesToListenTo()));
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
   QVERIFY(!listener.startListening(Utils::getCurrentInterfacesToListenTo()));
   QCOMPARE(listener.send(Common::MessageHeader::CORE_GOODBYE), INetworkListener::SendStatus::UNABLE_TO_SEND);
}

void Tests::automaticRebinding()
{
   const Instance& instance = this->instances[1];
   QStringList configuration { "initial interface configuration" };
   NL::NetworkListener listener(instance.fileManager, instance.peerManager, instance.uploadManager,
      instance.downloadManager, [&](const QList<QNetworkInterface>&) { return configuration; });
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
   // Listen to any address meanwhile, without losing the selection.
   QTRY_COMPARE(heartbeats, 4);
   QCOMPARE(SETTINGS.get<QString>("listen_address"), unavailableAddress);
   QCOMPARE(listener.send(Common::MessageHeader::CORE_GOODBYE, Protos::Common::Null()),
      INetworkListener::SendStatus::OK);
   QVERIFY(check());
   QCoreApplication::processEvents();
   QCOMPARE(heartbeats, 4);

   SETTINGS.set("listen_address", originalAddress);
   configuration << "interface restored";
   QVERIFY(check());
   QTRY_COMPARE(heartbeats, 5);
   QCOMPARE(listener.send(Common::MessageHeader::CORE_GOODBYE, Protos::Common::Null()),
      INetworkListener::SendStatus::OK);
}

void Tests::startupKeepsUnavailableAddress()
{
   const Instance& instance = this->instances[1];
   const QString originalAddress = SETTINGS.get<QString>("listen_address");
   const auto restore = qScopeGuard([&]() { SETTINGS.set("listen_address", originalAddress); });
   const QString unavailableAddress("198.51.100.42");
   SETTINGS.set("listen_address", unavailableAddress);

   // The selected adapter is not up yet when the core starts.
   QStringList configuration { "adapter not up yet" };
   NL::NetworkListener listener(instance.fileManager, instance.peerManager, instance.uploadManager,
      instance.downloadManager, [&](const QList<QNetworkInterface>&) { return configuration; });
   int heartbeats = 0;
   connect(&listener, &INetworkListener::IMAliveMessageToBeSend, this,
      [&](Protos::Core::IMAlive&) { ++heartbeats; });
   // Listen to any address meanwhile, without losing the selection.
   QTRY_COMPARE(heartbeats, 1);
   QCOMPARE(SETTINGS.get<QString>("listen_address"), unavailableAddress);
   QCOMPARE(listener.send(Common::MessageHeader::CORE_GOODBYE, Protos::Common::Null()),
      INetworkListener::SendStatus::OK);
   auto indexes = [](const QList<QNetworkInterface>& interfaces) {
      QList<int> result;
      for (const auto& interface : interfaces)
         result << interface.index();
      return result;
   };
   const auto fallbackInterfaces = indexes(Utils::getCurrentInterfacesToListenTo());
   SETTINGS.set("listen_address", QString());
   QCOMPARE(fallbackInterfaces, indexes(Utils::getCurrentInterfacesToListenTo()));

   // The adapter comes up with the selected address (simulated with the original one).
   SETTINGS.set("listen_address", originalAddress);
   configuration << "adapter up";
   QVERIFY(QMetaObject::invokeMethod(&listener, "checkNetworkConfiguration", Qt::DirectConnection));
   QTRY_COMPARE(heartbeats, 2);
   QCOMPARE(listener.send(Common::MessageHeader::CORE_GOODBYE, Protos::Common::Null()),
      INetworkListener::SendStatus::OK);
}

void Tests::downloadOwnChunks_data()
{
   QTest::addColumn<QString>("address");
   QTest::addColumn<quint32>("protocol");
   QTest::addColumn<bool>("unfinishedSource");
   QTest::newRow("IPv4 port conflict") << QString() << quint32(Protos::Common::Interface::Address::IPv4) << false;
   QTest::newRow("IPv6 port conflict") << QString() << quint32(Protos::Common::Interface::Address::IPv6) << false;
   QTest::newRow("specific IPv6 address") << QString("::1") << quint32(Protos::Common::Interface::Address::IPv6) << false;
   QTest::newRow("resume from same unfinished file") << QString() << quint32(Protos::Common::Interface::Address::IPv4) << true;
}

void Tests::downloadOwnChunks()
{
   QFETCH(QString, address);
   QFETCH(quint32, protocol);
   QFETCH(bool, unfinishedSource);
   const quint32 originalPort = SETTINGS.get<quint32>("unicast_base_port");
   const quint32 originalProtocol = SETTINGS.get<quint32>("listen_any");
   const QString originalAddress = SETTINGS.get<QString>("listen_address");
   const auto restore = qScopeGuard([&]() {
      SETTINGS.set("unicast_base_port", originalPort);
      SETTINGS.set("listen_any", originalProtocol);
      SETTINGS.set("listen_address", originalAddress);
   });
   SETTINGS.set("listen_any", protocol);
   SETTINGS.set("listen_address", address);
   QTcpServer blocker;
   QVERIFY(blocker.listen(Utils::getCurrentAddressToListenTo(), 0));
   SETTINGS.set("unicast_base_port", quint32(blocker.serverPort()));

   QTemporaryDir directory;
   QVERIFY(directory.isValid());
   // Repeated full chunks exercise duplicate hashes and socket reuse; the last chunk is shorter.
   const QByteArray block(Common::Constants::CHUNK_SIZE, 'x');
   const QByteArray data = block + block + QByteArray(12345, 'y');
   QFile source(directory.filePath("source.bin"));
   QVERIFY(source.open(QIODevice::WriteOnly));
   QCOMPARE(source.write(data), data.size());
   source.close();
   const QString destination = directory.filePath("destination/");
   QVERIFY(QDir().mkpath(destination));

   Instance instance = this->createInstance(Common::Hash::rand(), "local chunks");
   QVERIFY(instance.peerManager->getSelf()->getPort() != blocker.serverPort());
   instance.fileManager->setSharedPaths({ { "source", source.fileName() }, { "destination", destination } });
   const Common::Hash destinationID = instance.fileManager->getSharedEntries().last().ID;
   Common::Hasher hasher;
   hasher.addData(block);
   const Common::Hash hash = hasher.getResult();
   QTRY_VERIFY_WITH_TIMEOUT(!instance.fileManager->getChunk(hash).isNull(), 10000);
   Protos::Common::Entry entry;
   QVERIFY(instance.fileManager->getChunk(hash)->populateEntry(&entry));
   QCOMPARE(entry.chunks_size(), 3);
   hasher.reset();
   hasher.addData(QByteArray(12345, 'y'));
   const Common::Hash tailHash = hasher.getResult();
   QTRY_VERIFY_WITH_TIMEOUT(!instance.fileManager->getChunk(tailHash).isNull(), 10000);
   // Refresh the entry after the final chunk has been hashed.
   QVERIFY(instance.fileManager->getChunk(hash)->populateEntry(&entry));
   entry.set_name("copy.bin");
   if (unfinishedSource)
   {
      // Only the incomplete destination remains: chunk zero can supply the rest of chunk one.
      Protos::Common::Entry localEntry(entry);
      localEntry.clear_shared_entry();
      localEntry.mutable_shared_entry()->mutable_id()->set_hash(destinationID.getData(), Common::Hash::HASH_SIZE);
      localEntry.set_path("/");
      const auto chunks = instance.fileManager->newFile(localEntry);
      QCOMPARE(chunks.size(), 3);
      QVERIFY(chunks[0]->getDataWriter()->write(block.constData(), block.size()));
      QVERIFY(!chunks[1]->getDataWriter()->write(block.constData(), 1234));
      QVERIFY(chunks[2]->getDataWriter()->write(data.constData() + 2 * block.size(), 12345));
      instance.fileManager->setSharedPaths({ { "destination", destination } });
      QTRY_COMPARE_WITH_TIMEOUT(instance.fileManager->getChunk(hash), chunks[0], 10000);
      QVERIFY(!chunks[1]->isComplete());
   }
   PM::IPeer* offline = instance.peerManager->createPeer(Common::Hash::rand(), "offline source");
   QVERIFY(!offline->isAvailable());
   instance.downloadManager->addDownload(entry, offline, destinationID, "/");
   const auto downloads = instance.downloadManager->getDownloads();
   QCOMPARE(downloads.size(), 1);
   // A heartbeat discovers the local source without help from another peer.
   QTRY_COMPARE_WITH_TIMEOUT(downloads.first()->getStatus(), Protos::Common::DownloadStatus::COMPLETE, 15000);
   QFile copy(destination + "copy.bin");
   QVERIFY(copy.open(QIODevice::ReadOnly));
   QCOMPARE(copy.readAll(), data);
   QVERIFY(source.open(QIODevice::ReadOnly));
   QCOMPARE(source.readAll(), data);
   instance.downloadManager->removeAllCompleteDownloads();
}

void Tests::cleanupTestCase()
{
   qDebug() << "===== cleanupTestCase() =====";

   this->receivedMessages.clear();
   this->instances.clear();
}
