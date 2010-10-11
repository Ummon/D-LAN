#include <priv/OccupiedPeers.h>
using namespace DM;

bool OccupiedPeers::isPeerFree(PM::IPeer* peer)
{
   return !this->occupiedPeers.contains(peer);
}

bool OccupiedPeers::setPeerAsOccupied(PM::IPeer* peer)
{
   if (this->occupiedPeers.contains(peer))
      return false;

   this->occupiedPeers.insert(peer);
   return true;
}

void OccupiedPeers::setPeerAsFree(PM::IPeer* peer)
{
   this->occupiedPeers.remove(peer);
   emit newFreePeer(peer);
}

void OccupiedPeers::newPeer(PM::IPeer* peer)
{
   if (!this->occupiedPeers.contains(peer))
      emit newFreePeer(peer);
}
