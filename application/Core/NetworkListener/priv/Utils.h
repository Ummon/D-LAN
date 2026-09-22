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
  
#pragma once

#include <QNetworkInterface>
#include <QHostAddress>
#include <QStringList>

namespace NL
{
   class Utils
   {
   public:
      static QList<QNetworkInterface> getCurrentInterfacesToListenTo(const QList<QNetworkInterface>& interfaces = QNetworkInterface::allInterfaces());
      static void sanitizeListenSettings(const QList<QNetworkInterface>& interfaces = QNetworkInterface::allInterfaces());
      static QHostAddress getCurrentAddressToListenTo(const QList<QNetworkInterface>& interfaces = QNetworkInterface::allInterfaces());
      static QHostAddress getMulticastGroup(QAbstractSocket::NetworkLayerProtocol protocol);
      // Stable across enumeration order; only covers what the sockets depend on.
      static QStringList getNetworkConfiguration(const QList<QNetworkInterface>& interfaces = QNetworkInterface::allInterfaces());

   private:
      static bool addressExists(const QString& address, const QList<QNetworkInterface>& interfaces);
      static bool hasIPv6(const QList<QNetworkInterface>& interfaces);
   };
}
