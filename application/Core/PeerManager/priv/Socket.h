#ifndef PEERMANAGER_SOCKET_H
#define PEERMANAGER_SOCKET_H

#include <QTcpSocket>
#include <QDateTime>
#include <QHostAddress>

#include <google/protobuf/message.h>

#include <Core/FileManager/IFileManager.h>
#include <Common/Hash.h>

#include <ISocket.h>

namespace PM
{
   class PeerManager;

   class Socket : public ISocket
   {
      Q_OBJECT
   public:
      Socket(PeerManager* peerManager, QSharedPointer<FM::IFileManager> fileManager, QTcpSocket* socket);
      Socket(const QHostAddress& address, quint16 port);
      ~Socket();

      QIODevice* getDevice();

      void startListening();
      bool isIdle();
      void setActive();

      void send(quint32 type, const google::protobuf::Message& message);
      void finished();

   signals:
//    void newMessage(quint32 type, const google::protobuf::Message& message, Socket* socket);
      void getIdle(Socket*);
      void close();

   private slots:
      void dataReceived();
      void disconnected();

      void nextAskedHash(Common::Hash hash);

   private:
      PeerManager* peerManager;
      QSharedPointer<FM::IFileManager> fileManager;

      QTcpSocket* socket;
      QDateTime lastReleaseTime;
      bool idle;

      QSharedPointer<FM::IGetHashesResult> currentHashesResult;
      int nbHashToSend;
   };
}

#endif
