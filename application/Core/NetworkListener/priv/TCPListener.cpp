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

/**
  * @class NL::TCPListener
  * @author mcuony
  * @author gburri
  */

TCPListener::TCPListener(QSharedPointer<PM::IPeerManager> peerManager) :
   peerManager(peerManager)
{
   connect(&this->tcpServer, &QTcpServer::newConnection, this, &TCPListener::newConnection);
}

/**
  * @return The port currently listened to, 0 if the server isn't listening.
  */
quint16 TCPListener::getCurrentPort() const
{
   return this->tcpServer.serverPort();
}

bool TCPListener::listen(const QHostAddress& address, quint16 port)
{
   this->close();
   return this->tcpServer.listen(address, port);
}

void TCPListener::close()
{
   this->tcpServer.close();
}

void TCPListener::newConnection()
{
   QTcpSocket* socket = this->tcpServer.nextPendingConnection();
   this->peerManager->newConnection(socket);
}
