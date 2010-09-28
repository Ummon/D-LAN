#ifndef PEERMANAGER_SOCKET_H
#define PEERMANAGER_SOCKET_H

#include <QTcpSocket>
#include <QDateTime>
#include <QHostAddress>
#include <QTimer>

#include <google/protobuf/message.h>

#include <Common/Network.h>
#include <Common/Hash.h>
#include <Core/FileManager/IFileManager.h>

#include <ISocket.h>

namespace PM
{
   class PeerManager;

   class Socket : public ISocket
   {
      Q_OBJECT
   public:
      Socket(PeerManager* peerManager, QSharedPointer<FM::IFileManager> fileManager, QTcpSocket* socket);
      Socket(PeerManager* peerManager, QSharedPointer<FM::IFileManager> fileManager, const QHostAddress& address, quint16 port);
      ~Socket();

      QIODevice* getDevice();

      void startListening();
      void stopListening();
      bool isIdle();
      void setActive();

      void send(quint32 type, const google::protobuf::Message& message);
      void finished();

   public slots:
      void close();

   signals:
      void newMessage(quint32 type, const google::protobuf::Message& message);
      void getIdle(Socket*);
      void closed(Socket*);

   private slots:
      void dataReceived();
      void disconnected();

      void nextAskedHash(Common::Hash hash);

   private:
      bool readMessage();
      void initActivityTimer();

      PeerManager* peerManager;
      QSharedPointer<FM::IFileManager> fileManager;

      Common::MessageHeader currentHeader;

      QTcpSocket* socket;

      QTimer activityTimer; ///< When there is no activity during some time the socket is automatically closed.
      bool idle;
      bool listening;

      // Used when asking hashes to the fileManager.
      QSharedPointer<FM::IGetHashesResult> currentHashesResult;
      int nbHash;

#ifdef DEBUG
      int num;
      static int currentNum;
#endif
   };
}

#endif
