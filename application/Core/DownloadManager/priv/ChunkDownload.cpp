#include <priv/ChunkDownload.h>
using namespace DM;

#include <QMutexLocker>

ChunkDownload::ChunkDownload(QSharedPointer<PM::IPeerManager> peerManager, QSharedPointer<FM::IChunk> chunk, PM::IPeer* peerSource)
   : peerManager(peerManager), chunk(chunk), complete(false), downloading(false)
{
   this->peers << peerSource;
}

Common::Hash ChunkDownload::getHash()
{
   return this->chunk->getHash();
}

void ChunkDownload::setPeerIDs(const QList<Common::Hash>& peerIDs)
{
   for (QListIterator<Common::Hash> i(peerIDs); i.hasNext();)
   {
      PM::IPeer* peer = this->peerManager->getPeer(i.next());
      if (peer && !this->peers.contains(peer))
      {
         this->peers << peer;
         if (this->peers.size() == 1)
            emit chunkReadyToDownload(this);
      }
   }
}

/**
  * When a ChunkDownloader want to download a chunk it must be first call this method.
  * @return true if the ChunkDownloader can begin the download.
  */
bool ChunkDownload::setAsDownloading()
{
   QMutexLocker lock(&this->mutex);

   if (this->downloading)
      return false;

   this->downloading = true;
   return true;
}

void ChunkDownload::releaseDownloading()
{
   QMutexLocker lock(&this->mutex);

   this->downloading = false;
}

