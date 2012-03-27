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
  
#ifndef NETWORKLISTENER_TCPLISTENER_H
#define NETWORKLISTENER_TCPLISTENER_H

#include <QObject>
#include <QList>
#include <QTcpServer>
#include <QSharedPointer>

#include <Common/Uncopyable.h>

#include <Core/PeerManager/IPeerManager.h>

namespace NL
{
   class TCPListener : public QObject, Common::Uncopyable
   {
      Q_OBJECT
      static const int MAX_LISTEN_ATTEMPT;

   public:
      TCPListener(QSharedPointer<PM::IPeerManager> peerManager);
      quint16 getCurrentPort();
      void rebindSockets();

   private slots:
      void newConnection();

   private:
      QSharedPointer<PM::IPeerManager> peerManager;
      QTcpServer tcpServer;

      quint16 currentPort;

        // TODO: count the number of connection per ip per second and
        // banned temporarely an ip with too much attempt.
//      struct BannedIPs
//      {
//         QHostAddress address;
//         QDateTime time;
//      };
//      QList<BannedIPs> bannedIPs;
   };
}
#endif
