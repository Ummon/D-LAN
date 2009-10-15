#ifndef PEERMANAGER_PEER_H
#define PEERMANAGER_PEER_H

#include <QDate>
#include <QString>
#include <QAbstractSocket>

#include <Common/Hash.h>

#include <IPeer.h>

namespace PeerManager
{
   class Peer : public IPeer
   {
   public:
      Peer(Common::Hash NewID);
      void justSeen();
      bool haveYouToDie();
      bool isAlive();
      Common::Hash getId();
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
      //?Erreur a la compilation?QAbstractSocket socket;
      quint32 averageSpeed;
      QDate lastUpdateAverageSpeed;
      const static int ttl = 15;

   signals:
      void receive(QByteArray& data);
   };
}
#endif
