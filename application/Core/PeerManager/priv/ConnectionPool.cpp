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
  
#include <priv/ConnectionPool.h>
using namespace PM;

#include <Common/Constants.h>
#include <Common/Settings.h>

#include <priv/Log.h>
#include <priv/Constants.h>
#include <priv/PeerMessageSocket.h>
#include <priv/PeerManager.h>

/**
  * @class PM::ConnectionPool
  *
  * @brief Manage a set of opened sockets to a remote peer.
  *
  * There is two kind of socket.
  * 1) Socket opened by another peer.
  * 2) Socket opened by yourself.
  * When we want to send a message (for example 'GetHashes' to ask for some hashes) we must use the second kind.
  * This constraint exists to avoid sending two messages simultaneously, when one of the message (or both) is a 'GetChunk'.
  * The socket will be occupied for a moment to receive or send the stream of data and cannot handle others messages.
  *
  * The method 'getASocket()' may reuse a existing socket or create a new connection to the peer.
  */

ConnectionPool::ConnectionPool(PeerManager* peerManager, QSharedPointer<FM::IFileManager> fileManager, const Common::Hash& peerID) :
   peerManager(peerManager), fileManager(fileManager), peerID(peerID)
{
}

ConnectionPool::~ConnectionPool()
{
   this->closeAllSocket();
}

/**
  * Set the IP and the port of the remote peer.
  */
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
   this->addNewSocket(QSharedPointer<PeerMessageSocket>(new PeerMessageSocket(this->peerManager, this->fileManager, this->peerID, tcpSocket)), FROM_PEER);
}

/**
  * Return an idle socket to the peer.
  * A new connection is made if there is no idle socket.
  */
QSharedPointer<PeerMessageSocket> ConnectionPool::getASocket()
{
   for (QListIterator< QSharedPointer<PeerMessageSocket> > i(this->socketsToPeer); i.hasNext();)
   {
      QSharedPointer<PeerMessageSocket> socket = i.next();
      if (!socket->isActive())
      {
         socket->setActive();
         return socket;
      }
   }

   if (!this->peerIP.isNull())
      return this->addNewSocket(QSharedPointer<PeerMessageSocket>(new PeerMessageSocket(this->peerManager, this->fileManager, this->peerID, this->peerIP, this->port)), TO_PEER);

   L_ERRO("ConnectionPool::getASocket(): Unable to get a socket");
   return QSharedPointer<PeerMessageSocket>();
}

void ConnectionPool::closeAllSocket()
{
   for (QListIterator< QSharedPointer<PeerMessageSocket> > i(this->getAllSockets()); i.hasNext();)
   {
      QSharedPointer<PeerMessageSocket> socket = i.next();
      socket->close();
   }
}

void ConnectionPool::socketBecomeIdle(PeerMessageSocket* socket)
{
   quint32 n = 0;
   QList< QSharedPointer<PeerMessageSocket> > socketsToClose;

   for (QListIterator< QSharedPointer<PeerMessageSocket> > i(this->getAllSockets()); i.hasNext();)
   {
     QSharedPointer<PeerMessageSocket> currentSocket = i.next();
     if (!currentSocket.data()->isActive())
     {
        n += 1;
        if (n > SETTINGS.get<quint32>("max_number_idle_socket"))
           socketsToClose << currentSocket;
     }
   }

   for (QListIterator<QSharedPointer<PeerMessageSocket> > i(socketsToClose); i.hasNext();)
      i.next()->close();
}

void ConnectionPool::socketClosed(PeerMessageSocket* socket)
{
   for (int k = 0; k < 2; k++)
   {
      QList< QSharedPointer<PeerMessageSocket> >& list = k == 0 ? this->socketsToPeer : this->socketsFromPeer;

      for (QMutableListIterator< QSharedPointer<PeerMessageSocket> > i(list); i.hasNext();)
      {
         if (i.next().data() == socket)
         {
            socket->disconnect(this);
            i.remove();
            return;
         }
      }
   }
}

void ConnectionPool::socketGetChunk(QSharedPointer<FM::IChunk> chunk, int offset, PeerMessageSocket* socket)
{
   for (QListIterator< QSharedPointer<PeerMessageSocket> > i(this->socketsFromPeer); i.hasNext();)
   {
      QSharedPointer<PeerMessageSocket> socketShared = i.next();
      if (socketShared.data() == socket)
      {
         this->peerManager->onGetChunk(chunk, offset, socketShared);
         break;
      }
   }
}

/**
  * Add a newly created socket to the socket pool.
  */
QSharedPointer<PeerMessageSocket> ConnectionPool::addNewSocket(QSharedPointer<PeerMessageSocket> socket, Direction direction)
{
   switch (direction)
   {
   case TO_PEER:
      this->socketsToPeer << socket;
      break;
   case FROM_PEER:
      this->socketsFromPeer << socket;
      connect(socket.data(), SIGNAL(getChunk(QSharedPointer<FM::IChunk>, int, PeerMessageSocket*)), this, SLOT(socketGetChunk(QSharedPointer<FM::IChunk>, int, PeerMessageSocket*)), Qt::DirectConnection);
      break;
   }

   connect(socket.data(), SIGNAL(becomeIdle(PeerMessageSocket*)), this, SLOT(socketBecomeIdle(PeerMessageSocket*)));
   connect(socket.data(), SIGNAL(closed(PeerMessageSocket*)), this, SLOT(socketClosed(PeerMessageSocket*)) /*, Qt::QueuedConnection*/);
   socket->startListening();
   return socket;
}

QList< QSharedPointer<PeerMessageSocket> > ConnectionPool::getAllSockets() const
{
   QList< QSharedPointer<PeerMessageSocket> > allSockets;
   allSockets << this->socketsToPeer;
   allSockets << this->socketsFromPeer;
   return allSockets;
}
