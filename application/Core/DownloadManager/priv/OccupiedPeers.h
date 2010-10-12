#ifndef DOWNLOADMANAGER_OCCUPIEDPEERS_H
#define DOWNLOADMANAGER_OCCUPIEDPEERS_H

#include <QObject>
#include <QSet>
#include <QMutex>

namespace PM
{
   class IPeer;
}

namespace DM
{
   class OccupiedPeers : public QObject
   {
      Q_OBJECT
   public:
      OccupiedPeers();

      bool isPeerFree(PM::IPeer* peer) const;
      bool setPeerAsOccupied(PM::IPeer* peer);
      void setPeerAsFree(PM::IPeer* peer);
      void newPeer(PM::IPeer* peer);

   signals:
      void newFreePeer(PM::IPeer*);

   private:
      QSet<PM::IPeer*> occupiedPeers; // Peers currently occupied.
      mutable QMutex mutex;
   };
}

#endif
