#include <priv/ConnectionPool.h>
using namespace PM;

#include <Common/Constants.h>
#include <Common/Settings.h>

#include <priv/Log.h>
#include <priv/Constants.h>
#include <priv/Socket.h>

ConnectionPool::ConnectionPool(PeerManager* peerManager, QSharedPointer<FM::IFileManager> fileManager, const Common::Hash& peerID)
   : peerManager(peerManager), fileManager(fileManager), peerID(peerID)
{
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
   this->addNewSocket(new Socket(this->peerManager, this->fileManager, this->peerID, tcpSocket));
}

/**
  * Return an idle socket to the peer.
  * A new connection is made if there is no idle socket.
  */
Socket* ConnectionPool::getASocket()
{
   for (QListIterator<Socket*> i(this->sockets); i.hasNext();)
   {
      Socket* socket = i.next();
      if (socket->isIdle())
      {
         socket->setActive();
         return socket;
      }
   }

   if (!this->peerIP.isNull())
      return this->addNewSocket(new Socket(this->peerManager, this->fileManager, this->peerID, this->peerIP, this->port));

   return 0;
}

void ConnectionPool::closeAllSocket()
{
   for (QListIterator<Socket*> i(this->sockets); i.hasNext();)
      i.next()->close();

   this->sockets.clear();
}

void ConnectionPool::socketGetIdle(Socket* socket)
{
   int n = 0;
   for (QMutableListIterator<Socket*> i(this->sockets); i.hasNext();)
   {
     Socket* currentSocket = i.next();
     if (currentSocket->isIdle())
     {
        n += 1;
        if (n > SETTINGS.getUInt32("max_number_idle_socket"))
           i.remove();
     }
   }
}

void ConnectionPool::socketClosed(Socket* socket)
{
   this->sockets.removeAll(socket);
}

/**
  * Add a newly created socket to the socket pool.
  */
Socket* ConnectionPool::addNewSocket(Socket* socket)
{
   this->sockets << socket;
   connect(socket, SIGNAL(getIdle(Socket*)), this, SLOT(socketGetIdle(Socket*)));
   connect(socket, SIGNAL(closed(Socket*)), this, SLOT(socketClosed(Socket*)));
   socket->startListening();
   return socket;
}
