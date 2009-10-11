#ifndef PEERMANAGER_PEER_H
#define PEERMANAGER_PEER_H

#include <QDate>
#include <QString>
#include <QAbstractSocket>

#include <Common/Hash.h>

#include <IPeer.h>

//Time before a peer is dead
#define TTL 15

namespace PeerManager
{
   class Peer : public IPeer
   {
       public:
          Peer(Common::Hash ID_);
          void justSeen();
          bool haveYouToDie();
          bool isAlive();
          Common::Hash* getId();
          void send(const QByteArray& data) ;
          Common::Hashes* getHashes(const Protos::Common::FileEntry& file) ;
          IGetEntries* getEntries(const Protos::Common::DirEntry& dir)  ;

      private:
          Common::Hash ID;
          quint32 IP;
          bool IisAlive;
          QDateTime lastUpdate;
          QString nick;
          quint64 amount;
          //?Erreur à la compilation?QAbstractSocket socket;
          quint32 averageSpeed;
          QDate lastUpdateAverageSpeed;
      signals:
          void receive(QByteArray& data);
   };
}
#endif
