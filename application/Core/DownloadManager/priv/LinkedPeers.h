#ifndef DOWNLOADMANAGER_LINKEDPEERS_H
#define DOWNLOADMANAGER_LINKEDPEERS_H

#include <QMap>
#include <QList>

#include <Core/PeerManager/IPeer.h>

#include <priv/Log.h>

namespace DM
{
   /**
     * @class LinkedPeers
     * Count the number of chunks that each peer owns. If a peer has no chunk he is not referenced.
     */
   class LinkedPeers : private QMap<PM::IPeer*, quint32>
   {
   public:
      inline QList<PM::IPeer*> getPeers()
      {
         return this->keys();
      }

      inline void addLink(PM::IPeer* peer)
      {
         quint32& n = (*this)[peer];
         n++;
         L_DEBU(QString("addLink(..): peer: %1, n = %2").arg(peer->toStringLog()).arg(n));
      }

      inline void rmLink(PM::IPeer* peer)
      {
         quint32& n = (*this)[peer];
         n--;
         if (n == 0)
            this->remove(peer);
         L_DEBU(QString("rmLink(..): peer: %1, n = %2").arg(peer->toStringLog()).arg(n));
      }
   };
}

#endif
