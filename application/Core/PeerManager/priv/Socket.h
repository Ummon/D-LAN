#ifndef PEERMANAGER_SOCKET_H
#define PEERMANAGER_SOCKET_H

#include <QTcpSocket>
#include <QDateTime>
#include <QHostAddress>

#include <google/protobuf/message.h>

#include <Common/Hash.h>

namespace PM
{
   class Socket : public QObject
   {
      Q_OBJECT
   public:
      Socket(QTcpSocket* socket);
      Socket(const QHostAddress& address, quint16 port);
      ~Socket();

      void startListening();
      //QTcpSocket* getSocket();

      bool isIdle();
      void setActive();

      void send(quint32 type, const google::protobuf::Message& message, const Common::Hash& senderID);
      void finished();

   signals:
      void newMessage(quint32 type, const google::protobuf::Message& message, Socket* socket);
      void getIdle(Socket*);
      void close();

   private slots:
      void dataReceived();
      void disconnected();

   private:
      QTcpSocket* socket;
      QDateTime lastReleaseTime;
      bool idle;
   };
}

#endif
