#include <priv/FileDownload.h>
using namespace DM;

#include <Common/Settings.h>

#include <priv/Log.h>
#include <priv/Constants.h>

FileDownload::FileDownload(
   QSharedPointer<FM::IFileManager> fileManager,
   QSharedPointer<PM::IPeerManager> peerManager,
   OccupiedPeers& occupiedPeersAskingForHashes,
   OccupiedPeers& occupiedPeersDownloadingChunk,
   Common::Hash peerSourceID, const Protos::Common::Entry& entry
)
   : Download(fileManager, peerManager, peerSourceID, entry),
   NB_CHUNK(this->entry.size() / SETTINGS.get<quint32>("chunk_size") + (this->entry.size() % SETTINGS.get<quint32>("chunk_size") == 0 ? 0 : 1)),
   occupiedPeersAskingForHashes(occupiedPeersAskingForHashes),
   occupiedPeersDownloadingChunk(occupiedPeersDownloadingChunk),
   nbHashesKnown(0),
   fileCreated(false)
{   
   L_DEBU(QString("New FileDownload : source = %1, entry : \n%2").arg(this->peerSourceID.toStr()).arg(QString::fromStdString(this->entry.DebugString())));

   this->timer.setInterval(CHECK_ENTRY_PERIOD);
   this->timer.setSingleShot(true);
   connect(&this->timer, SIGNAL(timeout()), this, SLOT(retreiveHashes()));

   for (int i = 0; i < entry.chunk_size(); i++)
   {
      QSharedPointer<ChunkDownload> chunkDownload = QSharedPointer<ChunkDownload>(new ChunkDownload(this->peerManager, this->occupiedPeersDownloadingChunk, Common::Hash(entry.chunk(i).hash().data())));
      this->chunkDownloads << chunkDownload;
      this->connectChunkDownloadSignals(this->chunkDownloads.last());
   }
   this->nbHashesKnown = this->chunkDownloads.size();

   if (this->hasAValidPeer())
      for (QListIterator< QSharedPointer<ChunkDownload> > i(this->chunkDownloads); i.hasNext();)
         i.next()->setPeerSource(this->peerSource);

   this->retreiveHashes();
}

int FileDownload::getDownloadRate() const
{
   int downloadRate = 0;
   for (QListIterator< QSharedPointer<ChunkDownload> > i(this->chunkDownloads); i.hasNext();)
      downloadRate += i.next()->getDownloadRate();

   return downloadRate;
}

int FileDownload::getProgress() const
{
   quint64 knownBytes = 0;
   for (QListIterator< QSharedPointer<ChunkDownload> > i(this->chunkDownloads); i.hasNext();)
   {
      knownBytes += i.next()->getDownloadedBytes();
   }

   return 100LL * knownBytes / this->entry.size();
}

/**
  * If there is a ChunkDownload with a free peer (we do not already download from this peer) the return the chunk.
  * The file is created on the fly with IFileManager::newFile(..) if we don't have the IChunks.
  */
QSharedPointer<ChunkDownload> FileDownload::getAChunkToDownload()
{
   for (QListIterator< QSharedPointer<ChunkDownload> > i(this->chunkDownloads); i.hasNext();)
   {
      QSharedPointer<ChunkDownload> chunkDownload = i.next();
      if (chunkDownload->isReadyToDownload())
      {
         if (!this->fileCreated)
         {
            this->chunksWithoutDownload = this->fileManager->newFile(this->entry);
            for (int i = 0; !this->chunksWithoutDownload.isEmpty() && i < this->chunkDownloads.size(); i++)
            {
               this->chunkDownloads[i]->setChunk(this->chunksWithoutDownload.takeFirst());
            }
            this->fileCreated = true;
         }

         return chunkDownload;
      }
   }

   return QSharedPointer<ChunkDownload>();
}

/**
  * Return true if a GetHashes request has been sent to the peer.
  */
bool FileDownload::retreiveHashes()
{
   // If we've already got all the chunk hashes it's unecessary to re-ask them.
   // Or if we'v got anyone to ask the chunk hashes..
   if (this->nbHashesKnown == this->NB_CHUNK || !this->hasAValidPeer())
      return false;

   if (!this->occupiedPeersAskingForHashes.setPeerAsOccupied(this->peerSource))
      return false;

   this->status = INITIALIZING;

   this->getHashesResult = this->peerSource->getHashes(this->entry);
   connect(this->getHashesResult.data(), SIGNAL(result(const Protos::Core::GetHashesResult&)), this, SLOT(result(const Protos::Core::GetHashesResult&)));
   connect(this->getHashesResult.data(), SIGNAL(nextHash(const Common::Hash&)), this, SLOT(nextHash(const Common::Hash&)));
   this->getHashesResult->start();
   return true;
}

void FileDownload::retrievePeer()
{
   Download::retrievePeer();

   // Right after we got the peer we can ask him the hashes.
   this->retreiveHashes();
}

void FileDownload::result(const Protos::Core::GetHashesResult& result)
{
   if (result.status() == Protos::Core::GetHashesResult_Status_OK)
   {
      if (this->nbHashesKnown + static_cast<int>(result.nb_hash()) != this->NB_CHUNK)
         L_ERRO(QString("The received hashes (%1) plus the known hashes (%2) is not equal to the number of chunks (%3)").arg(result.nb_hash()).arg(this->nbHashesKnown).arg(this->NB_CHUNK));
   }
   else
   {
      this->status = ENTRY_NOT_FOUND;
      this->getHashesResult.clear();
      this->occupiedPeersAskingForHashes.setPeerAsFree(this->peerSource);
      this->timer.start(); // Retry later.
   }
}

void FileDownload::nextHash(const Common::Hash& hash)
{
   if (++this->nbHashesKnown == this->NB_CHUNK)
   {
      this->getHashesResult.clear();
      this->occupiedPeersAskingForHashes.setPeerAsFree(this->peerSource);
   }

   if (this->chunkDownloads.size() >= this->nbHashesKnown && this->chunkDownloads[this->nbHashesKnown-1]->getHash() != hash)
   {
      L_ERRO(
         QString("The hash (%1) num %2 received doesn't match the hash (%3) in the entry").
            arg(hash.toStr()).
            arg(this->nbHashesKnown-1).
            arg(this->chunkDownloads[this->nbHashesKnown-1]->getHash().toStr())
      );
   }
   else
   {
      QSharedPointer<ChunkDownload> chunkDownload = QSharedPointer<ChunkDownload>(new ChunkDownload(this->peerManager, this->occupiedPeersDownloadingChunk, hash));

      // If the file has alredy been created the chunks are known.
      if (!this->chunksWithoutDownload.isEmpty())
         chunkDownload->setChunk(this->chunksWithoutDownload.takeFirst());

      this->chunkDownloads << chunkDownload;
      this->connectChunkDownloadSignals(chunkDownload);
      chunkDownload->setPeerSource(this->peerSource);
   }
}

void FileDownload::downloadStarted()
{
   this->status = DOWNLOADING;
}

void FileDownload::downloadFinished()
{
   this->status = COMPLETE;
   for (QListIterator< QSharedPointer<ChunkDownload> > i(this->chunkDownloads); i.hasNext();)
   {
      QSharedPointer<ChunkDownload> chunkDownload = i.next();

      if (chunkDownload->isDownloading())
      {
         this->status = DOWNLOADING;
         return;
      }
      else if (!chunkDownload->isComplete())
      {
         if (!chunkDownload->hasAtLeastAPeer())
            this->status = NO_SOURCE;
         else if (!this->getHashesResult.isNull())
            this->status = INITIALIZING;
         else
            this->status = QUEUED;
      }
   }
}

void FileDownload::connectChunkDownloadSignals(QSharedPointer<ChunkDownload> chunkDownload)
{
   connect(chunkDownload.data(), SIGNAL(downloadStarted()), this, SLOT(downloadStarted()), Qt::DirectConnection);
   connect(chunkDownload.data(), SIGNAL(downloadFinished()), this, SLOT(downloadFinished()), Qt::DirectConnection);
}
