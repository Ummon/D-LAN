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

#include <Protos/common.pb.h>
#include <Protos/core_settings.pb.h>

#include <Common/Settings.h>
#include <Common/ProtoHelper.h>

#include <priv/Log.h>

QNetworkInterface Utils::getCurrentInterfaceToListenTo()
{
   const QString addressToListen = SETTINGS.get<QString>("listen_address");
   auto interfaces = QNetworkInterface::allInterfaces();

   if (interfaces.isEmpty() || addressToListen.isEmpty())
      return QNetworkInterface();

   // L_DEBU(QString("address to listen to: %1").arg(addressToListen));

   for (QList<QNetworkInterface>::const_iterator i = interfaces.begin(); i != interfaces.end(); ++i)
   {
      // L_DEBU(QString("Interface: %1").arg(i->name()));
      foreach (QNetworkAddressEntry entry, i->addressEntries())
      {
         // L_DEBU(QString("IP: %1").arg(entry.ip().toString()));
         if (entry.ip().toString() == addressToListen)
            return *i;
      }
   }

   return interfaces.first();
}

QHostAddress Utils::getCurrentAddressToListenTo()
{
   const QString addressToListen = SETTINGS.get<QString>("listen_address");

   if (!addressToListen.isEmpty())
   {
      // Check if the address exists.
      foreach (QHostAddress address, QNetworkInterface::allAddresses())
         if (address.toString() == addressToListen)
            return QHostAddress(addressToListen);

      SETTINGS.set("listen_address", QString(""));
   }

   // Check if IPv6 is available.
   bool hasAnyIPv6 = false;
   foreach (QHostAddress address, QNetworkInterface::allAddresses())
      if (address == QHostAddress::AnyIPv6 || address == QHostAddress::LocalHostIPv6)
      {
         hasAnyIPv6 = true;
         break;
      }

   if (!hasAnyIPv6 && SETTINGS.get<quint32>("listen_any") == Protos::Common::Interface::Address::IPv6)
      SETTINGS.set("listen_any", static_cast<quint32>(Protos::Common::Interface::Address::IPv4));

   return SETTINGS.get<quint32>("listen_any") == Protos::Common::Interface::Address::IPv4 ? QHostAddress::AnyIPv4 : QHostAddress::AnyIPv6;
}

/**
  * Return the multicast group. It can be an IPv6 or an IPv4 group, depending of the current address.
  * The group is stored in the setting variable 'multicast_group'.
  */
QHostAddress Utils::getMulticastGroup()
{
   const quint32 group = SETTINGS.get<quint32>("multicast_group");
   QHostAddress currentAddressToListenTo = Utils::getCurrentAddressToListenTo();

   QByteArray channelHash = Common::Hasher::hash(SETTINGS.get<QString>("channel")).getByteArray();

   if (currentAddressToListenTo.protocol() == QAbstractSocket::IPv4Protocol)
      return QHostAddress(group);
   else // IPv6.
   {
      Q_IPV6ADDR groupIPv6;
      groupIPv6[0] = 0xFF;
      groupIPv6[1] = 0x0E;

      for (int i = 0; i < 10; i++)
         groupIPv6[i+2] = channelHash[i];

      groupIPv6[12] = (group & 0xFF000000) >> 24;
      groupIPv6[13] = (group & 0x00FF0000) >> 16;
      groupIPv6[14] = (group & 0x0000FF00) >> 8;
      groupIPv6[15] = group & 0x000000FF;
      return QHostAddress(groupIPv6);
   }
}
