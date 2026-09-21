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
  
#include <priv/Utils.h>
using namespace NL;

#include <QString>
#include <QNetworkInterface>
#include <QAbstractSocket>

#include <Protos/common.pb.h>
#include <Protos/core_settings.pb.h>

#include <Common/Settings.h>
#include <Common/ProtoHelper.h>
#include <Common/Network/InterfacePolicy.h>

#include <priv/Log.h>

QStringList Utils::getNetworkConfiguration(const QList<QNetworkInterface>& interfaces)
{
   QStringList configuration;
   for (const auto& interface : interfaces)
   {
      const QString identity = QString("%1|%2|%3|%4")
         .arg(interface.index()).arg(interface.name()).arg(static_cast<int>(interface.flags())).arg(interface.hardwareAddress());
      configuration << identity;
      for (const auto& entry : interface.addressEntries())
         configuration << QString("%1|%2|%3|%4")
            .arg(identity, entry.ip().toString(), entry.netmask().toString(), entry.broadcast().toString());
   }
   configuration.sort();
   return configuration;
}

QList<QNetworkInterface> Utils::getCurrentInterfacesToListenTo()
{
   QList<QNetworkInterface> interfaces;
   const auto allInterfaces = QNetworkInterface::allInterfaces();
   const QString addressToListen = SETTINGS.get<QString>("listen_address");
   const auto protocol = Utils::getCurrentAddressToListenTo(allInterfaces).protocol();
   for (const auto& interface : allInterfaces)
   {
      // An explicit address may select loopback or a tunnel. "Any" must use LAN adapters,
      // not the OS default multicast interface, which can be loopback on Windows.
      if (addressToListen.isEmpty() && !Common::isDefaultMulticastInterface(interface))
         continue;
      for (const auto& entry : interface.addressEntries())
      {
         // QHostAddress equality ignores IPv6 scope IDs. Match the full address
         // so tunnels sharing a link-local address are selected independently.
         if (addressToListen.isEmpty() ? entry.ip().protocol() == protocol : entry.ip().toString() == addressToListen)
         {
            interfaces << interface;
            break; // Join each adapter once, even if it has several matching addresses.
         }
      }
   }
   return interfaces;
}

/**
  * @return true if the given address is currently assigned to one of the network interfaces.
  */
bool Utils::addressExists(const QString& address, const QList<QNetworkInterface>& interfaces)
{
   for (const auto& interface : interfaces)
      for (const auto& entry : interface.addressEntries())
         if (entry.ip().toString() == address)
            return true;
   return false;
}

/**
  * @return true if IPv6 is available on an active multicast LAN adapter.
  * Only adapters eligible for automatic discovery count, excluding macOS auxiliary interfaces and tunnels.
  */
bool Utils::hasIPv6(const QList<QNetworkInterface>& interfaces)
{
   for (const auto& interface : interfaces)
      if (Common::isDefaultMulticastInterface(interface))
         for (const auto& entry : interface.addressEntries())
            if (entry.ip().protocol() == QAbstractSocket::IPv6Protocol)
               return true;
   return false;
}

/**
  * Check the settings 'listen_address' and 'listen_any' against the current network configuration and correct them if needed.
  * It should be called once before (re)binding the sockets.
  * 'getCurrentAddressToListenTo()' doesn't depend on this function, it will always return a valid address.
  */
void Utils::sanitizeListenSettings(const QList<QNetworkInterface>& interfaces)
{
   const QString addressToListen = SETTINGS.get<QString>("listen_address");
   if (!addressToListen.isEmpty() && !Utils::addressExists(addressToListen, interfaces))
   {
      L_WARN(QString("The address to listen to (%1) doesn't exist anymore, listening to any address instead").arg(addressToListen));
      SETTINGS.set("listen_address", QString(""));
   }

   if (SETTINGS.get<QString>("listen_address").isEmpty() &&
       SETTINGS.get<quint32>("listen_any") == Protos::Common::Interface::Address::IPv6 && !Utils::hasIPv6(interfaces))
   {
      L_WARN("No usable IPv6 multicast LAN adapter, listening to any IPv4 address instead");
      SETTINGS.set("listen_any", static_cast<quint32>(Protos::Common::Interface::Address::IPv4));
   }
}

/**
  * @return The address to bind the sockets to. This function has no side effect on the settings, see 'sanitizeListenSettings()'.
  */
QHostAddress Utils::getCurrentAddressToListenTo(const QList<QNetworkInterface>& interfaces)
{
   const QString addressToListen = SETTINGS.get<QString>("listen_address");

   if (!addressToListen.isEmpty() && Utils::addressExists(addressToListen, interfaces))
      return QHostAddress(addressToListen);

   return
      SETTINGS.get<quint32>("listen_any") == Protos::Common::Interface::Address::IPv4 || !Utils::hasIPv6(interfaces) ?
           QHostAddress::AnyIPv4
         : QHostAddress::AnyIPv6;
}

/**
  * Return the multicast group. It can be an IPv6 or an IPv4 group, depending of the current address.
  * The group is stored in the setting variable 'multicast_group'.
  */
QHostAddress Utils::getMulticastGroup(QAbstractSocket::NetworkLayerProtocol protocol)
{
   static const quint32 group = SETTINGS.get<quint32>("multicast_group");

   if (protocol == QAbstractSocket::IPv4Protocol)
   {
      return QHostAddress(group);
   }
   else // Default is IPv6.
   {
      const QByteArray channelHash = Common::Hasher::hash(SETTINGS.get<QString>("channel")).getByteArray();

      Q_IPV6ADDR groupIPv6 {};
      // Scope: link-local, transient.
      groupIPv6[0] = 0xFF;
      groupIPv6[1] = 0x12;

      // Darwin embeds the interface scope in bytes 2..3 of link-local
      // addresses and clears them on output (XNU in6_setscope/in6_clearscope).
      // Keep that word zero on every platform. Preserve the remaining hash
      // bytes so the address matches what Darwin previously sent on the wire.
      for (int i = 2; i < 10 && i < channelHash.size(); ++i)
         groupIPv6[i+2] = channelHash[i];

      groupIPv6[12] = (group & 0xFF000000) >> 24;
      groupIPv6[13] = (group & 0x00FF0000) >> 16;
      groupIPv6[14] = (group & 0x0000FF00) >> 8;
      groupIPv6[15] = group & 0x000000FF;
      return QHostAddress(groupIPv6);
   }
}
