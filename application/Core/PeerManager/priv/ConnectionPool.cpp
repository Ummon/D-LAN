/**
  * Aybabtu - A decentralized LAN file sharing software.
  * Copyright (C) 2010-2011 Greg Burri <greg.burri@gmail.com>
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
  
#include <priv/ConnectionPool.h>
using namespace PM;

#include <Common/Constants.h>
#include <Common/Settings.h>

#include <priv/Log.h>
#include <priv/Constants.h>
#include <priv/Socket.h>
#include <priv/PeerManager.h>

ConnectionPool::ConnectionPool(PeerManager* peerManager, QSharedPointer<FM::IFileManager> fileManager, const Common::Hash& peerID)
   : peerManager(peerManager), fileManager(fileManager), peerID(peerID)
{
}

ConnectionPool::~ConnectionPool()
{
   foreach (QSharedPointer<Socket> s, this->sockets)
      s->close();
}

void ConnectionPool::setIP(const QHostAddress& IP, quint16 port)
{
   this->peerIP = IP;
   this->port = port;
}

/**
  * Add an already established connection.
  */
void ConnectionPool::newConnexion(QTcpSocket* tcpSocket)
{
   this->addNewSocket(QSharedPointer<Socket>(new Socket(this->peerManager, this->fileManager, this->peerID, tcpSocket)));
}

/**
  * Return an idle socket to the peer.
  * A new connection is made if there is no idle socket.
  */
QSharedPointer<Socket> ConnectionPool::getASocket()
{
   for (QListIterator< QSharedPointer<Socket> > i(this->sockets); i.hasNext();)
   {
      QSharedPointer<Socket> socket = i.next();
      if (socket->isIdle())
      {
         socket->setActive();
         return socket;
      }
   }

   if (!this->peerIP.isNull())
      return this->addNewSocket(QSharedPointer<Socket>(new Socket(this->peerManager, this->fileManager, this->peerID, this->peerIP, this->port)));

   L_ERRO("ConnectionPool::getASocket() : Unable to get a socket");
   return QSharedPointer<Socket>();
}

void ConnectionPool::closeAllSocket()
{
   for (QListIterator< QSharedPointer<Socket> > i(this->sockets); i.hasNext();)
      i.next()->close();
}

void ConnectionPool::socketGetIdle(Socket* socket)
{
   quint32 n = 0;
   QList< QSharedPointer<Socket> > socketsToClose;

   for (QMutableListIterator< QSharedPointer<Socket> > i(this->sockets); i.hasNext();)
   {
     QSharedPointer<Socket> currentSocket = i.next();
     if (currentSocket.data()->isIdle())
     {
        n += 1;
        if (n > SETTINGS.get<quint32>("max_number_idle_socket"))
           socketsToClose << currentSocket;
     }
   }

   for (QListIterator<QSharedPointer<Socket> > i(socketsToClose); i.hasNext();)
      i.next()->close();
}

void ConnectionPool::socketClosed(Socket* socket)
{
   for (QMutableListIterator< QSharedPointer<Socket> > i(this->sockets); i.hasNext();)
      if (i.next().data() == socket)
      {
         i.remove();
         break;
      }
}

void ConnectionPool::socketGetChunk(const Common::Hash& hash, int offset, Socket* socket)
{
   for (QListIterator< QSharedPointer<Socket> > i(this->sockets); i.hasNext();)
   {
      QSharedPointer<Socket> socketShared = i.next();
      if (socketShared.data() == socket)
      {
         this->peerManager->onGetChunk(hash, offset, socketShared);
         break;
      }
   }
}

/**
  * Add a newly created socket to the socket pool.
  */
QSharedPointer<Socket> ConnectionPool::addNewSocket(QSharedPointer<Socket> socket)
{
   this->sockets << socket;
   connect(socket.data(), SIGNAL(getIdle(Socket*)), this, SLOT(socketGetIdle(Socket*)));
   connect(socket.data(), SIGNAL(closed(Socket*)), this, SLOT(socketClosed(Socket*)));
   connect(socket.data(), SIGNAL(getChunk(const Common::Hash&, int, Socket*)), this, SLOT(socketGetChunk(const Common::Hash&, int, Socket*)), Qt::DirectConnection);
   socket->startListening();
   return socket;
}
