#ifndef PEERMANAGER_CONNECTIONPOOL_H
#define PEERMANAGER_CONNECTIONPOOL_H

#include <QList>
#include <QSharedPointer>
#include <QTcpSocket>

namespace PM
{
   struct Socket
   {
      void setIdle(bool idle) { this->idle = idle; }
      bool isIdle() { return this->idle; }

      void setSocket(QSharedPointer<QTcpSocket> socket) { this->socket = socket; }
      QSharedPointer<QTcpSocket> getSocket() { return this->socket; }

   private:
      QSharedPointer<QTcpSocket> socket;
      bool idle;
   };

   template <typename T>
   class ConnectionPool
   {
   public:
      //ConnectionPool(host);

      void setIdleTimeout(int time);
      void setMaxIdleSocket(int n);

      void addSocket(QSharedPointer<QTcpSocket>, T value);
      QSharedPointer<QTcpSocket> grabSocket(T value);
      void releaseSocket(QSharedPointer<QTcpSocket>);

   private:
      int idleTimeout; // [ms].
      int maxIdleSocket;
      //host;
   };

   /***** Definition *****/
}

#endif
