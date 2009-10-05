#ifndef PEERMANAGER_PEERMANAGER_H
#define PEERMANAGER_PEERMANAGER_H

#include <QString>
#include <QList>

#include <Common/Hash.h>

#include <IPeerManager.h>

namespace PeerManager
{
   class Peer;

   class PeerManager : public IPeerManager
   {
   private:
      QList<Peer*> peers;
      Common::Hash ID;
      QString nick;
   };
}
#endif
