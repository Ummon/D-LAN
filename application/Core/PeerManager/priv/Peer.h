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
   private:
      Common::Hash ID;
      quint32 IP;
      bool isAlive;
      QDate lastUpdate;
      QString nick;
      quint64 amount;
      QAbstractSocket socket;
      quint32 averageSpeed;
      QDate lastUpdateAverageSpeed;
   };
}
#endif
