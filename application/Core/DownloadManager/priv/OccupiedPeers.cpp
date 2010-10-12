#include <priv/OccupiedPeers.h>
using namespace DM;

#include <QMutexLocker>

OccupiedPeers::OccupiedPeers()
//   : mutex(QMutex::Recursive)
{
}

bool OccupiedPeers::isPeerFree(PM::IPeer* peer) const
{
   QMutexLocker lock(&this->mutex);
   return !this->occupiedPeers.contains(peer);
}

bool OccupiedPeers::setPeerAsOccupied(PM::IPeer* peer)
{
   QMutexLocker lock(&this->mutex);
   if (this->occupiedPeers.contains(peer))
      return false;

   this->occupiedPeers.insert(peer);
   return true;
}

void OccupiedPeers::setPeerAsFree(PM::IPeer* peer)
{
   {
      QMutexLocker lock(&this->mutex);
      this->occupiedPeers.remove(peer);
   }
   emit newFreePeer(peer);
}

void OccupiedPeers::newPeer(PM::IPeer* peer)
{
   this->mutex.lock();
   if (!this->occupiedPeers.contains(peer))
   {
      this->mutex.unlock();
      emit newFreePeer(peer);
   }
   else
      this->mutex.unlock();
}
