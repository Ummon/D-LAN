#include <priv/ConnectionPool.h>
using namespace PM;

#include <Common/Constants.h>

#include <priv/Log.h>
#include <priv/Constants.h>
#include <priv/Socket.h>

ConnectionPool::ConnectionPool(PeerManager* peerManager, QSharedPointer<FM::IFileManager> fileManager)
   : peerManager(peerManager), fileManager(fileManager)
{
//   connect(&this->timer, SIGNAL(timeout()), this, SLOT(cleanIdleSockets()));
//   this->timer.start();
//   this->timer.setInterval(1000 * IDLE_SOCKET_TIMEOUT / 10);
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
   Socket* socket = new Socket(this->peerManager, this->fileManager, tcpSocket);
   this->sockets << socket;
   //connect(socket, SIGNAL(newMessage(quint32, const google::protobuf::Message&, Socket*)), this, SIGNAL(newMessage(quint32, const google::protobuf::Message&, Socket*)));
   connect(socket, SIGNAL(getIdle(Socket*)), this, SLOT(socketGetIdle(Socket*)));

   socket->startListening();
}

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
   {
      Socket* newSocket = new Socket(this->peerIP, this->port);
      this->sockets << newSocket;
      return newSocket;
   }

   return 0;
}

/**
  * Return a socket to communicate with the peer.
  * Choose an idle one or create a new one if there is no idle socket.
  */
//QSharedPointer<QTcpSocket> ConnectionPool::grabSocket()
//{
//   for (QListIterator<Socket> i(this->sockets); i.hasNext();)
//   {
//      Socket socket = i.next();
//      if (socket.idle)
//      {
//         socket.idle = false;
//         return socket.socket;
//      }
//   }

//   QSharedPointer<QTcpSocket> newSocket;

//   if (!this->peerIP.isNull())
//   {
//      newSocket = QSharedPointer<QTcpSocket>(new QTcpSocket());
//      newSocket->connectToHost(this->peerIP, this->port);
//      this->sockets << Socket(newSocket);
//   }

//   return newSocket;
//}

///**
//  * When a socket is no more used this method must be called.
//  */
//void ConnectionPool::releaseSocket(QSharedPointer<QTcpSocket> socket)
//{
//   for (QListIterator<Socket> i(this->sockets); i.hasNext();)
//   {
//      Socket existingSocket = i.next();
//      if (existingSocket.socket == socket)
//      {
//         existingSocket.idle = true;
//         existingSocket.lastReleaseTime = QDateTime::currentDateTime();
//         break;
//      }
//   }
//}

//void ConnectionPool::cleanIdleSockets()
//{
//   int n = 0;
//   for (QMutableListIterator<Socket> i(this->sockets); i.hasNext();)
//   {
//      Socket existingSocket = i.next();
//      if (existingSocket.idle)
//      {
//         if (existingSocket.lastReleaseTime.addSecs(IDLE_SOCKET_TIMEOUT) > QDateTime::currentDateTime())
//            i.remove();
//         else
//         {
//            n += 1;
//            if (n > MAX_NUMBER_IDLE_SOCKET)
//               i.remove();
//         }
//      }
//   }
//}


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
