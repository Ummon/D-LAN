#ifndef PEERMANAGER_CONNECTIONPOOL_H
#define PEERMANAGER_CONNECTIONPOOL_H

#include <QtNetwork>
#include <QList>
#include <QDateTime>
#include <QTimer>

#include <priv/Socket.h>

namespace PM
{
   class Socket;

   class ConnectionPool : public QObject
   {
      Q_OBJECT

   public:
      ConnectionPool();

      void setIP(const QHostAddress& IP, quint16 port);
      void newConnexion(QTcpSocket* socket);


      Socket* getASocket();
//      void releaseSocket(QSharedPointer<QTcpSocket> socket);

   signals:
      void newMessage(quint32 type, const google::protobuf::Message& message, Socket* socket);

      //void cleanIdleSockets();

   private slots:
      void socketGetIdle(Socket* socket);

   private:
      QTimer timer;
      QList<Socket*> sockets;
      QHostAddress peerIP;
      quint16 port;
   };
}

#endif
