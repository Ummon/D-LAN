#include <priv/ConnectionPool.h>
using namespace PM;

#include <Common/Constants.h>

#include <priv/Constants.h>

ConnectionPool::ConnectionPool()
{
   this->timer.setInterval(1000 * IDLE_SOCKET_TIMEOUT / 10);
   connect(&this->timer, SIGNAL(timeout()), this, SLOT(cleanIdleSockets()));
   this->timer.start();
}

void ConnectionPool::setIP(const QHostAddress& IP)
{
   this->peerIP = IP;
}

/**
  * Add an already established connection.
  */
void ConnectionPool::addSocket(QSharedPointer<QTcpSocket> socket)
{
   if (socket->isValid())
      this->sockets << Socket(socket);
}

/**
  * Return a socket to communicate with the peer.
  * Choose an idle one or create a new one if there is no idle socket.
  */
QSharedPointer<QTcpSocket> ConnectionPool::grabSocket()
{
   for (QListIterator<Socket> i(this->sockets); i.hasNext();)
   {
      Socket socket = i.next();
      if (socket.idle)
      {
         socket.idle = false;
         return socket.socket;
      }
   }

   QSharedPointer<QTcpSocket> newSocket;

   if (!this->peerIP.isNull())
   {
      newSocket = QSharedPointer<QTcpSocket>(new QTcpSocket());
      newSocket->connectToHost(this->peerIP, Common::BASE_PORT);
      this->sockets << Socket(newSocket);
   }

   return newSocket;
}

/**
  * When a socket is no more used this method must be called.
  */
void ConnectionPool::releaseSocket(QSharedPointer<QTcpSocket> socket)
{
   for (QListIterator<Socket> i(this->sockets); i.hasNext();)
   {
      Socket existingSocket = i.next();
      if (existingSocket.socket == socket)
      {
         existingSocket.idle = true;
         existingSocket.lastReleaseTime = QDateTime::currentDateTime();
         break;
      }
   }
}

void ConnectionPool::cleanIdleSockets()
{
   int n = 0;
   for (QMutableListIterator<Socket> i(this->sockets); i.hasNext();)
   {
      Socket existingSocket = i.next();
      if (existingSocket.idle)
      {
         if (existingSocket.lastReleaseTime.addSecs(IDLE_SOCKET_TIMEOUT) > QDateTime::currentDateTime())
            i.remove();
         else
         {
            n += 1;
            if (n > MAX_NUMBER_IDLE_SOCKET)
               i.remove();
         }
      }
   }
}
