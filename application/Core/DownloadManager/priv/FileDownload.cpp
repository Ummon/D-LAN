#include <priv/FileDownload.h>
using namespace DM;

#include <Common/Settings.h>
#include <Common/ProtoHelper.h>

#include <Core/FileManager/Exceptions.h>

#include <priv/Log.h>
#include <priv/Constants.h>

FileDownload::FileDownload(
   QSharedPointer<FM::IFileManager> fileManager,
   QSharedPointer<PM::IPeerManager> peerManager,
   OccupiedPeers& occupiedPeersAskingForHashes,
   OccupiedPeers& occupiedPeersDownloadingChunk,
   Common::Hash peerSourceID,
   const Protos::Common::Entry& entry,
   bool complete
)
   : Download(fileManager, peerManager, peerSourceID, entry),
   NB_CHUNK(this->entry.size() / SETTINGS.get<quint32>("chunk_size") + (this->entry.size() % SETTINGS.get<quint32>("chunk_size") == 0 ? 0 : 1)),
   occupiedPeersAskingForHashes(occupiedPeersAskingForHashes),
   occupiedPeersDownloadingChunk(occupiedPeersDownloadingChunk),
   nbHashesKnown(0),
   fileCreated(false)
{
   L_DEBU(QString("New FileDownload : source = %1, entry : \n%2").
      arg(this->peerSourceID.toStr()).
      arg(Common::ProtoHelper::getDebugStr(this->entry))
   );

   if (complete)
      this->status = COMPLETE;

   this->timer.setInterval(CHECK_ENTRY_PERIOD);
   this->timer.setSingleShot(true);
   connect(&this->timer, SIGNAL(timeout()), this, SLOT(retreiveHashes()));

   // We create a ChunkDownload for each known hash in the entry.
   for (int i = 0; i < entry.chunk_size(); i++)
   {
      Common::Hash chunkHash(entry.chunk(i).hash().data());
      QSharedPointer<ChunkDownload> chunkDownload = QSharedPointer<ChunkDownload>(new ChunkDownload(this->peerManager, this->occupiedPeersDownloadingChunk, chunkHash));

      this->chunkDownloads << chunkDownload;
      this->connectChunkDownloadSignals(this->chunkDownloads.last());
   }
   this->nbHashesKnown = this->chunkDownloads.size();
}

FileDownload::~FileDownload()
{
   this->status = DELETED;
   this->timer.stop();
   this->getHashesResult.clear();
   this->chunksWithoutDownload.clear();
   this->chunkDownloads.clear();
}

void FileDownload::start()
{
   if (this->hasAValidPeer())
   {
      for (QListIterator< QSharedPointer<ChunkDownload> > i(this->chunkDownloads); i.hasNext();)
         i.next()->setPeerSource(this->peerSource, false); // 'false' : to avoid to send unnecessary 'newFreePeer'.

      this->occupiedPeersDownloadingChunk.newPeer(this->peerSource);
   }

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

QSet<Common::Hash> FileDownload::getPeers() const
{
   QSet<Common::Hash> peerIDs;
   for (QListIterator< QSharedPointer<ChunkDownload> > i(this->chunkDownloads); i.hasNext();)
      for (QListIterator<Common::Hash> j(i.next()->getPeers()); j.hasNext();)
         peerIDs << j.next();
   return peerIDs;
}

/**
  * If there is a ChunkDownload with a free peer (we do not already download from this peer) the return the chunk.
  * The file is created on the fly with IFileManager::newFile(..) if we don't have the IChunks.
  * @return The chunk to download, can return a null pointer if an error occurs.
  */
QSharedPointer<ChunkDownload> FileDownload::getAChunkToDownload()
{
   if (this->status == COMPLETE || this->status == DELETED)
      return QSharedPointer<ChunkDownload>();

   try
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

               // If we got all the chunks, remote entry becomes a local entry.
               if (!this->chunksWithoutDownload.isEmpty() && this->nbHashesKnown == this->NB_CHUNK)
                  this->chunksWithoutDownload.first()->populateEntry(&this->entry);

               this->fileCreated = true;
            }

            return chunkDownload;
         }
      }
   }
   catch(FM::NoReadWriteSharedDirectoryException&)
   {
      L_DEBU(QString("There is no shared directory with writting rights for this download : %1").arg(Common::ProtoHelper::getStr(this->entry, &Protos::Common::Entry::name)));
      this->status = NO_SHARED_DIRECTORY_TO_WRITE;
   }
   catch(FM::InsufficientStorageSpaceException&)
   {
      L_DEBU(QString("There is no enough space storage available for this download : %1").arg(Common::ProtoHelper::getStr(this->entry, &Protos::Common::Entry::name)));
      this->status = NO_ENOUGH_FREE_SPACE;
   }
   catch(FM::FilePhysicallyAlreadyExistsException)
   {
      L_DEBU(QString("The file already exists, download : %1").arg(Common::ProtoHelper::getStr(this->entry, &Protos::Common::Entry::name)));
      this->status = THE_FILE_ALREADY_EXISTS;
   }
   catch(FM::UnableToCreateNewFileException&)
   {
      L_DEBU(QString("Unable to create the file, download : %1").arg(Common::ProtoHelper::getStr(this->entry, &Protos::Common::Entry::name)));
      this->status = UNABLE_TO_CREATE_THE_FILE;
   }

   return QSharedPointer<ChunkDownload>();
}

void FileDownload::getUnfinishedChunks(QList< QSharedPointer<IChunkDownload> >& chunks, int n)
{
   for (QListIterator< QSharedPointer<ChunkDownload> > i(this->chunkDownloads); i.hasNext() && this->status != COMPLETE;)
   {
      const QSharedPointer<ChunkDownload>& chunkDownload = i.next();
      if (!chunkDownload->isComplete())
         chunks << chunkDownload;
   }
}

/**
  * Return true if a GetHashes request has been sent to the peer.
  */
bool FileDownload::retreiveHashes()
{
   // If we've already got all the chunk hashes it's unecessary to re-ask them.
   // Or if we'v got anyone to ask the chunk hashes..
   if (this->nbHashesKnown == this->NB_CHUNK || !this->hasAValidPeer() || this->status == COMPLETE)
      return false;

   if (!this->occupiedPeersAskingForHashes.setPeerAsOccupied(this->peerSource))
      return false;

   if (!this->sender() && this->status == UNABLE_TO_RETRIEVE_THE_HASHES) // Not called by the timer.
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
      L_DEBU(QString("Unable to retrieve the hashes, error = %1").arg(result.status()));
      this->status = ENTRY_NOT_FOUND;
      this->getHashesResult.clear();
      this->occupiedPeersAskingForHashes.setPeerAsFree(this->peerSource);
      this->status = UNABLE_TO_RETRIEVE_THE_HASHES;
      this->timer.start(); // Retry later.
   }
}

void FileDownload::nextHash(const Common::Hash& hash)
{
   L_DEBU(QString("New Hash received : %1").arg(hash.toStr()));

   if (++this->nbHashesKnown == this->NB_CHUNK)
   {
      this->getHashesResult.clear();
      this->occupiedPeersAskingForHashes.setPeerAsFree(this->peerSource);

      // If we got all the chunks, remote entry becomes a local entry.
      if (this->fileCreated && !this->chunkDownloads.isEmpty())
         this->chunkDownloads.first()->getChunk()->populateEntry(&this->entry);
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

      // If the file has already been created, the chunks are known.
      if (!this->chunksWithoutDownload.isEmpty())
         chunkDownload->setChunk(this->chunksWithoutDownload.takeFirst());

      this->chunkDownloads << chunkDownload;
      this->connectChunkDownloadSignals(chunkDownload);
      chunkDownload->setPeerSource(this->peerSource); // May start a download.
      this->entry.add_chunk()->set_hash(hash.getData(), Common::Hash::HASH_SIZE); // Used during the saving of the queue, see Download::populateEntry(..).
   }
}

void FileDownload::chunkDownloadStarted()
{
   this->status = DOWNLOADING;
}

void FileDownload::chunkDownloadFinished()
{
   if (this->status == DELETED)
      return;

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

   if (this->chunkDownloads.size() != this->NB_CHUNK)
      this->status = INITIALIZING;
}

void FileDownload::connectChunkDownloadSignals(QSharedPointer<ChunkDownload> chunkDownload)
{
   connect(chunkDownload.data(), SIGNAL(downloadStarted()), this, SLOT(chunkDownloadStarted()), Qt::DirectConnection);
   connect(chunkDownload.data(), SIGNAL(downloadFinished()), this, SLOT(chunkDownloadFinished()), Qt::DirectConnection);
}
