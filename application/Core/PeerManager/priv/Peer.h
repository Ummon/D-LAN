#ifndef PEERMANAGER_PEER_H
#define PEERMANAGER_PEER_H

#include <QDate>
#include <QTimer>
#include <QString>
#include <QTcpSocket>
#include <QHostAddress>
#include <QSharedPointer>

#include <google/protobuf/text_format.h>

#include <Common/Hash.h>
#include <Common/Network.h>

#include <Core/FileManager/IGetHashesResult.h>
#include <Core/FileManager/IFileManager.h>

#include <IPeer.h>
#include <priv/ConnectionPool.h>

namespace PM
{   
   class Socket;
   class PeerManager;

   class Peer : public IPeer
   {
      Q_OBJECT
   public:
      Peer(PeerManager* peerManager, QSharedPointer<FM::IFileManager> fileManager, Common::Hash ID);

      Common::Hash getID();
      QHostAddress getIP();
      QString getNick();
      quint64 getSharingAmount();

      bool isAlive();
      void update(const QHostAddress& IP, quint16 port, const QString& nick, const quint64& sharingAmount);

      //bool send(const QByteArray& data) ;
      QSharedPointer<IGetEntriesResult> getEntries(const Protos::Core::GetEntries& dir);
      QSharedPointer<IGetHashesResult> getHashes(const Protos::Common::Entry& file);
      QSharedPointer<IGetChunkResult> getChunk(const Protos::Core::GetChunk& chunk);

      void newConnexion(QTcpSocket* tcpSocket);

   private slots:

//      void sendGetEntriesRequest(QTcpSocket* socket = 0);
      void consideredDead();
//      void stateChanged(QAbstractSocket::SocketState socketState);
//      void dataReceived();

   private:
      QString toStr();

      PeerManager* peerManager;
      QSharedPointer<FM::IFileManager> fileManager;

      ConnectionPool connectionPool;


      Common::Hash ID;
      QHostAddress IP;
      quint16 port;
      QString nick;
      quint64 sharingAmount;

      bool alive;
      QTimer aliveTimer;

      quint32 averageSpeed;
      QDateTime lastUpdateAverageSpeed;
   };
}
#endif
