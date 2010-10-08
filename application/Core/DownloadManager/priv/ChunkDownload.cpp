#include <priv/ChunkDownload.h>
using namespace DM;

#include <QMutexLocker>

#include <Core/PeerManager/IPeer.h>


ChunkDownload::ChunkDownload(QSharedPointer<PM::IPeerManager> peerManager, OccupiedPeers& occupiedPeersDownloadingChunk, Common::Hash chunkHash)
   : peerManager(peerManager), occupiedPeersDownloadingChunk(occupiedPeersDownloadingChunk), chunkHash(chunkHash), complete(false), downloading(false)
{
}

Common::Hash ChunkDownload::getHash()
{
   return this->chunkHash;
}

void ChunkDownload::setPeerIDs(const QList<Common::Hash>& peerIDs)
{
   this->peers.clear();
   for (QListIterator<Common::Hash> i(peerIDs); i.hasNext();)
   {
      PM::IPeer* peer = this->peerManager->getPeer(i.next());
      if (peer)
      {
         this->peers << peer;
         this->occupiedPeersDownloadingChunk.newPeer(peer);
      }
   }
}

void ChunkDownload::setPeerSource(PM::IPeer* peer)
{
   if (!this->peers.contains(peer))
   {
      this->peers << peer;
      this->occupiedPeersDownloadingChunk.newPeer(peer);
   }
}

bool ChunkDownload::downloadChunk()
{
   QMutexLocker lock(&this->mutex);

   if (this->downloading)
      return false;
   this->downloading = true;

   PM::IPeer* peer = this->getFastestPeer();
   if (!peer)
      goto end;

   /*QSharedPointer<IChunk> = chunkDownload

   Protos::Core::GetChunk getChunkMess;

   getChunkMess.chunk().mutable_hash()->
   peer->getChunk()*/

end:
   this->downloading = false;

return true;
}

PM::IPeer* ChunkDownload::getFastestPeer() const
{
   QMutexLocker lock(&this->mutex);

   PM::IPeer* current = 0;
   foreach (PM::IPeer* peer, this->peers)
   {
      if (!current || peer->getSpeed() > current->getSpeed())
         current = peer;
   }
   return current;
}
