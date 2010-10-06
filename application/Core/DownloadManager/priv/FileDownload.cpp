#include <priv/FileDownload.h>
using namespace DM;

FileDownload::FileDownload(QSharedPointer<FM::IFileManager> fileManager, QSharedPointer<PM::IPeerManager>, Common::Hash peerSourceID, const Protos::Common::Entry& entry)
   : Download(fileManager, peerManager, peerSourceID, entry)
{

}

void FileDownload::retreiveHashes()
{
   if (!this->hasAValidPeer())
      return;

   this->status |= ASKING_FOR_HASHES;

   this->getHashesResult = this->peerSource->getHashes(this->entry);
   connect(this->getHashesResult.data(), SIGNAL(result(const Protos::Core::GetHashesResult&)), this, SLOT(result(const Protos::Core::GetHashesResult&)));
   connect(this->getHashesResult.data(), SIGNAL(nextHash(const Common::Hash&)), this, SLOT(nextHash(const Common::Hash&)));
   this->getHashesResult->start();
}

void FileDownload::chunkReadyToDownload(ChunkDownload* chunkDownload)
{
   for (QListIterator< QSharedPointer<ChunkDownload> > i(this->chunkDownloads); i.hasNext(); i.next())
   {
      if (i.peekNext().data() == chunkDownload)
      {
         emit chunkReadyToDownload(i.peekNext());
         return;
      }
   }
}

void FileDownload::result(const Protos::Core::GetHashesResult& result)
{
   if (result.status() == Protos::Core::GetHashesResult_Status_OK)
   {
      this->status &= !ENTRY_NOT_FOUND;
      this->nbHashes = result.nb_hash();
   }
   else
   {
      this->status &= !ASKING_FOR_HASHES;
      this->status |= ENTRY_NOT_FOUND;
      this->getHashesResult.clear();
   }
}

void FileDownload::nextHash(const Common::Hash& hash)
{
   if (--this->nbHashes == 0)
      this->getHashesResult.clear(); // TODO : Must we disconnect the signals ?

   // The file is creating on the fly when the first hash is received.
   if (this->chunkDownloads.isEmpty())
      this->chunksWithoutDownload = this->fileManager->newFile(this->entry);

   QSharedPointer<FM::IChunk> chunk = this->chunksWithoutDownload.takeFirst();
   chunk->setHash(hash);
   this->chunkDownloads << QSharedPointer<ChunkDownload>(new ChunkDownload(this->peerManager, chunk, this->peerSource));
   emit chunkReadyToDownload(this->chunkDownloads.last()); // We assume the sourcePeer has the chunk..
}
