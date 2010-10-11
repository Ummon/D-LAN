#include <priv/FileDownload.h>
using namespace DM;

#include <Common/Settings.h>

#include <priv/Log.h>

FileDownload::FileDownload(
   QSharedPointer<FM::IFileManager> fileManager,
   QSharedPointer<PM::IPeerManager> peerManager,
   OccupiedPeers& occupiedPeersAskingForHashes,
   OccupiedPeers& occupiedPeersDownloadingChunk,
   Common::Hash peerSourceID, const Protos::Common::Entry& entry
)
   : Download(fileManager, peerManager, peerSourceID, entry),
   NB_CHUNK(this->entry.size() / SETTINGS.getUInt32("chunk_size") + (this->entry.size() % SETTINGS.getUInt32("chunk_size") == 0 ? 0 : 1)),
   occupiedPeersAskingForHashes(occupiedPeersAskingForHashes),
   occupiedPeersDownloadingChunk(occupiedPeersDownloadingChunk),
   nbHashes(0),
   fileCreated(false)
{   
   L_DEBU(QString("New FileDownload : path = %1/%2 source = %3").arg(QString::fromStdString(entry.path())).arg(QString::fromStdString(entry.name())).arg(this->peerSourceID.toStr()));

   for (int i = 0; i < entry.chunk_size(); i++)
      this->chunkDownloads << QSharedPointer<ChunkDownload>(new ChunkDownload(this->peerManager, this->occupiedPeersDownloadingChunk, Common::Hash(entry.chunk(i).hash().data())));

   if (this->hasAValidPeer())
      for (QListIterator< QSharedPointer<ChunkDownload> > i(this->chunkDownloads); i.hasNext();)
         i.next()->setPeerSource(this->peerSource);

   this->retreiveHashes();
}

/**
  * Return true if a GetHashes request has been sent to the peer.
  */
bool FileDownload::retreiveHashes()
{
   // If we've already got all the chunk hashes it's unecessary to re-ask them.
   // Or if we'v got anyone to ask the chunk hashes..
   if (this->nbHashes != 0 || !this->hasAValidPeer())
      return false;

   if (!this->occupiedPeersAskingForHashes.setPeerAsOccupied(this->peerSource))
      return false;

   this->status |= ASKING_FOR_HASHES;

   this->getHashesResult = this->peerSource->getHashes(this->entry);
   connect(this->getHashesResult.data(), SIGNAL(result(const Protos::Core::GetHashesResult&)), this, SLOT(result(const Protos::Core::GetHashesResult&)));
   connect(this->getHashesResult.data(), SIGNAL(nextHash(const Common::Hash&)), this, SLOT(nextHash(const Common::Hash&)));
   this->getHashesResult->start();
   return true;
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
            for (int i = 0; i < this->chunksWithoutDownload.size() && i < this->chunkDownloads.size(); i++)
               this->chunkDownloads[i]->setChunk(this->chunksWithoutDownload.takeFirst());
            this->fileCreated = true;
         }

         return chunkDownload;
      }
   }

   return QSharedPointer<ChunkDownload>();
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
      this->status &= !ENTRY_NOT_FOUND;
      this->nbHashes = result.nb_hash();
      this->nbHashesReceived = 0;
   }
   else
   {
      this->status &= !ASKING_FOR_HASHES;
      this->status |= ENTRY_NOT_FOUND;
      this->getHashesResult.clear();
      this->occupiedPeersAskingForHashes.setPeerAsFree(this->peerSource);
   }
}

void FileDownload::nextHash(const Common::Hash& hash)
{
   if (++this->nbHashesReceived == this->nbHashes)
   {
      this->getHashesResult.clear();
      this->occupiedPeersAskingForHashes.setPeerAsFree(this->peerSource);
   }

   if (this->chunkDownloads.size() >= this->nbHashesReceived && this->chunkDownloads[this->nbHashesReceived-1]->getHash() != hash)
   {
      L_ERRO(
         QString("The hash (%1) num %2 received doesn't match the hash (%3) in the entry").
            arg(hash.toStr()).
            arg(this->nbHashesReceived-1).
            arg(this->chunkDownloads[this->nbHashesReceived-1]->getHash().toStr())
      );
   }
   else
   {
      QSharedPointer<ChunkDownload> chunkDownload = QSharedPointer<ChunkDownload>(new ChunkDownload(this->peerManager, this->occupiedPeersDownloadingChunk, hash));

      // If the file has alredy been created the chunks are known.
      if (!this->chunksWithoutDownload.isEmpty())
         chunkDownload->setChunk(this->chunksWithoutDownload.takeFirst());

      this->chunkDownloads << chunkDownload;
      chunkDownload->setPeerSource(this->peerSource);
   }
}
