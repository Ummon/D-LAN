#ifndef PEERMANAGER_PEER_H
#define PEERMANAGER_PEER_H

#include <QDate>
#include <QTimer>
#include <QString>
#include <QTcpSocket>
#include <QHostAddress>

#include <Common/Hash.h>
#include <Common/Network.h>

#include <QSharedPointer>

#include <Core/FileManager/IFileManager.h>

#include <IPeer.h>

namespace PM
{
   class Peer : public IPeer
   {
      Q_OBJECT
   public:
      Peer(QSharedPointer<FM::IFileManager> fileManager, Common::Hash ID);

      Common::Hash getID();
      QHostAddress getIP();
      QString getNick();
      quint64 getSharingAmount();

      bool isAlive();
      void update(const QHostAddress& IP, const QString& nick, const quint64& sharingAmount);

      //bool send(const QByteArray& data) ;
      void getHashes(const Protos::Common::FileEntry& file) ;
      void getEntries(const Protos::Common::Entry& dir);

      void newConnexion(Common::MessageHeader header, QSharedPointer<QTcpSocket> socket);

   private:
      QSharedPointer<FM::IFileManager> fileManager;

      Common::Hash ID;
      QHostAddress IP;
      QString nick;
      quint64 sharingAmount;

      bool alive;
      QTimer aliveTimer;

      QList< QSharedPointer<QTcpSocket> > sockets;

      quint32 averageSpeed;
      QDateTime lastUpdateAverageSpeed;

      //QByteArray bufferToWrite;

      //static const int ttl = 15;
      //static const int port;

   /*signals:
      void receive(QByteArray& data);*/

   private slots:
      void consideredDead();

      void stateChanged(QAbstractSocket::SocketState socketState);
      void dataReceived();
   };
}
#endif
