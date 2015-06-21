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
  
#include <priv/TCPListener.h>
using namespace NL;

#include <Common/Settings.h>

#include <priv/Log.h>
#include <priv/Utils.h>

/**
  * @class NL::TCPListener
  * @author mcuony
  * @author gburri
  */

const int TCPListener::MAX_LISTEN_ATTEMPT(10);

TCPListener::TCPListener(QSharedPointer<PM::IPeerManager> peerManager) :
   peerManager(peerManager)
{
   this->rebindSockets();
}

quint16 TCPListener::getCurrentPort()
{
   return this->currentPort;
}

void TCPListener::rebindSockets()
{
   this->tcpServer.close();
   this->tcpServer.disconnect(this);

   this->currentPort = SETTINGS.get<quint32>("unicast_base_port");

   int n = 0;
   while(!this->tcpServer.listen(Utils::getCurrentAddressToListenTo(), this->currentPort))
   {
      if (++n == MAX_LISTEN_ATTEMPT)
      {
         L_ERRO("Can't find a port to listen");
         break;
      }
      this->currentPort++;
   }
   connect(&this->tcpServer, &QTcpServer::newConnection, this, &TCPListener::newConnection);
}

void TCPListener::newConnection()
{
   QTcpSocket* socket = this->tcpServer.nextPendingConnection();
   this->peerManager->newConnection(socket);
}
