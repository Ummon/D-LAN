#ifndef PEERMANAGER_CONNECTIONPOOL_H
#define PEERMANAGER_CONNECTIONPOOL_H

#include <QtNetwork>
#include <QList>
#include <QSharedPointer>
#include <QDateTime>
#include <QTimer>

namespace PM
{
   class ConnectionPool : public QObject
   {
      Q_OBJECT
      struct Socket
      {
         Socket(QSharedPointer<QTcpSocket> socket) : socket(socket), idle(false) {}

         QSharedPointer<QTcpSocket> socket;
         QDateTime lastReleaseTime;
         bool idle;
      };

   public:
      ConnectionPool();

      void setIP(const QHostAddress& IP);
      void addSocket(QSharedPointer<QTcpSocket> socket);
      QSharedPointer<QTcpSocket> grabSocket();
      void releaseSocket(QSharedPointer<QTcpSocket> socket);

   private slots:
      void cleanIdleSockets();

   private:
      QTimer timer;
      QList<Socket> sockets;
      QHostAddress peerIP;
   };
}

#endif
