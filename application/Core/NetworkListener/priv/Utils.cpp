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

QHostAddress Utils::getCurrentAddressToListenTo()
{
   // Check if IPv6 is available.
   bool hasAnyIPv6 = false;
   foreach (QHostAddress address, QNetworkInterface::allAddresses())
      if (address == QHostAddress::AnyIPv6 || address == QHostAddress::LocalHostIPv6)
      {
         hasAnyIPv6 = true;
         break;
      }

   if (!hasAnyIPv6 && SETTINGS.get<quint32>("listen_any") == Protos::Common::Interface::Address::IPv6)
      SETTINGS.set("listen_any", static_cast<quint32>(Protos::Common::Interface::Address::IPv6));

   QString adressToListen = SETTINGS.get<QString>("listen_address");

   // Check if the address exists.
   foreach (QHostAddress address, QNetworkInterface::allAddresses())
      if (address.toString() == adressToListen)
         return QHostAddress(adressToListen);

   SETTINGS.set("listen_address", QString(""));

   return SETTINGS.get<quint32>("listen_any") == Protos::Common::Interface::Address::IPv4 ? QHostAddress::Any : QHostAddress::AnyIPv6;
}

/**
  * Return the multicast group. It can be an IPv6 or an IPv4 group, depending of the current address.
  * The group is stored in the setting variable 'multicast_group'.
  */
QHostAddress Utils::getMulticastGroup()
{
   const quint32 group = SETTINGS.get<quint32>("multicast_group");
   QHostAddress currentAddressToListentTo = Utils::getCurrentAddressToListenTo();

   if (currentAddressToListentTo.protocol() == QAbstractSocket::IPv4Protocol)
      return QHostAddress(group);
   else // IPv6.
   {
      Q_IPV6ADDR groupIPv6;
      groupIPv6[0] = 0xFF;
      groupIPv6[1] = 0x0e;
      groupIPv6[2] = (group & 0xFF000000) >> 24;
      groupIPv6[3] = (group & 0x00FF0000) >> 16;
      groupIPv6[4] = (group & 0x0000FF00) >> 8;
      groupIPv6[5] = group & 0x000000FF;
      groupIPv6[6] = 0;
      groupIPv6[7] = 0;
      groupIPv6[8] = 0;
      groupIPv6[9] = 0;
      groupIPv6[10] = 0;
      groupIPv6[11] = 0;
      groupIPv6[12] = 0;
      groupIPv6[13] = 0;
      groupIPv6[14] = 0;
      groupIPv6[15] = 0;
      return QHostAddress(groupIPv6);
   }
}
