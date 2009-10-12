#ifndef PEERMANAGER_PEERMANAGER_H
#define PEERMANAGER_PEERMANAGER_H

#include <QString>
#include <QList>
#include <QSharedPointer>
#include <QTimer>
#include <QObject>

#include <Common/Hash.h>

#include <IPeerManager.h>
#include <Common/LogManager/ILogger.h>
#include <priv/Peer.h>

//Clean up each 10s.
#define CleanUpFrequency 0.1

namespace PeerManager
{
   class Peer;

   class PeerManager : public IPeerManager
   {
       public:
          PeerManager();
          Common::Hash getMyId();
          void setNick(const QString& nick_);
          QString* getNick();
          void updatePeer(const Common::Hash& ID_, quint32 IP_, const QString& nick_, const quint64& amount_);

       private:
          QList<Peer*> peers;
          Common::Hash ID;
          QString nick;
          QTimer *timer;
          QSharedPointer<LogManager::ILogger> logger;
          Peer* fromIdToPeer(const Common::Hash& ID_);

       public slots:
          void cleanUp();
   };
}
#endif
