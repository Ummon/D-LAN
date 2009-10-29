#ifndef PEERMANAGER_PEER_H
#define PEERMANAGER_PEER_H

#include <QDate>
#include <QString>
#include <QTcpSocket>
#include <QHostAddress>

#include <Common/Hash.h>

#include <QSharedPointer>

#include <IPeer.h>
#include <Common/LogManager/ILogger.h>


namespace PM
{
   class Peer : public IPeer
   {
   public:
      Peer(Common::Hash NewID);
      void justSeen(const QHostAddress& peerIP, const QString& peerNick, const quint64& peerAmount);
      bool haveYouToDie();
      bool isAlive();
      Common::Hash getId();
      bool send(const QByteArray& data) ;
      Common::Hashes* getHashes(const Protos::Common::FileEntry& file) ;
      IGetEntries* getEntries(const Protos::Common::DirEntry& dir)  ;
      QHostAddress getIp();
      void newSocket(QSharedPointer<QTcpSocket> newSocket);


   private:
      Common::Hash ID;
      QHostAddress IP;
      bool IisAlive;
      QDateTime lastUpdate;
      QString nick;
      quint64 amount;
      QSharedPointer<QTcpSocket> socket;
      quint32 averageSpeed;
      QDate lastUpdateAverageSpeed;
      QByteArray bufferToWrite;
      QSharedPointer<LM::ILogger> logger;
      static const int ttl = 15;
      static const int port;

   signals:
      void receive(QByteArray& data);
   public slots:
      void connected();
      void gotData();
   };
}
#endif
