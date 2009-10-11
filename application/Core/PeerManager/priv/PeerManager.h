#ifndef PEERMANAGER_PEERMANAGER_H
#define PEERMANAGER_PEERMANAGER_H

#include <QString>
#include <QList>
#include <QSharedPointer>

#include <Common/Hash.h>

#include <IPeerManager.h>
#include <Common/LogManager/ILogger.h>

namespace PeerManager
{
   class Peer;

   class PeerManager : public IPeerManager
   {
       public:
          PeerManager();
          Common::Hash* getMyId();
          void setNick(const QString& nick_);
          QString* getNick();

       private:
          QList<Peer*> peers;
          Common::Hash ID;
          QString nick;
          QSharedPointer<LogManager::ILogger> logger;
   };
}
#endif
