#include <priv/ConnectionPool.h>
using namespace PM;

#include <Common/Constants.h>

#include <priv/Log.h>
#include <priv/Constants.h>
#include <priv/Socket.h>

ConnectionPool::ConnectionPool(PeerManager* peerManager, QSharedPointer<FM::IFileManager> fileManager)
   : peerManager(peerManager), fileManager(fileManager)
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
   this->addNewSocket(new Socket(this->peerManager, this->fileManager, tcpSocket));
}

Socket* ConnectionPool::getASocket()
{
   for (QListIterator<Socket*> i(this->sockets); i.hasNext();)
   {
      Socket* socket = i.next();
      if (socket->isIdle())
         return socket;
   }

   if (!this->peerIP.isNull())
      return this->addNewSocket(new Socket(this->peerManager, this->fileManager, this->peerIP, this->port));

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
        if (n > MAX_NUMBER_IDLE_SOCKET)
           i.remove();
     }
   }
}

/**
  * Add a newly created socket to the socket pool.
  */
Socket* ConnectionPool::addNewSocket(Socket* socket)
{
   this->sockets << socket;
   connect(socket, SIGNAL(getIdle(Socket*)), this, SLOT(socketGetIdle(Socket*)));
   socket->startListening();
   return socket;
}
