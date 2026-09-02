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

#include <limits>

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
   peerManager(peerManager), currentPort(0)
{
}

/**
  * @return The port currently listened to, 0 if the server isn't listening.
  */
quint16 TCPListener::getCurrentPort()
{
   return this->currentPort;
}

/**
  * Try to listen to 'unicast_base_port', if it's already taken the next ports are tried.
  * If none of them is available a port chosen by the OS is used, this port is transmitted to the peers by the 'IMAlive' message.
  */
void TCPListener::rebindSockets()
{
   this->tcpServer.close();
   this->tcpServer.disconnect(this);

   const QHostAddress address = Utils::getCurrentAddressToListenTo();
   const quint32 basePort = SETTINGS.get<quint32>("unicast_base_port");

   for (int n = 0; n < MAX_LISTEN_ATTEMPT && basePort + n <= std::numeric_limits<quint16>::max(); n++)
      if (this->tcpServer.listen(address, static_cast<quint16>(basePort + n)))
         break;

   if (!this->tcpServer.isListening())
   {
      if (this->tcpServer.listen(address, 0))
         L_WARN(
            QString("Unable to listen to the ports %1 to %2, listening to the port %3 instead")
               .arg(basePort).arg(basePort + MAX_LISTEN_ATTEMPT - 1).arg(this->tcpServer.serverPort())
         );
      else
         L_ERRO(QString("Unable to listen to any port on %1: %2").arg(address.toString(), this->tcpServer.errorString()));
   }

   this->currentPort = this->tcpServer.serverPort();

   connect(&this->tcpServer, &QTcpServer::newConnection, this, &TCPListener::newConnection);
}

void TCPListener::newConnection()
{
   QTcpSocket* socket = this->tcpServer.nextPendingConnection();
   this->peerManager->newConnection(socket);
}
