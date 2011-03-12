/**
  * Aybabtu - A decentralized LAN file sharing software.
  * Copyright (C) 2010-2011 Greg Burri <greg.burri@gmail.com>
  *
  * This program is free software: you can redistribute it and/or modify
  * it under the terms of the GNU General Public License as published by
  * the Free Software Foundation, either version 3 of the License, or
  * (at your option) any later version.
  *
  * This program is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  *
  * You should have received a copy of the GNU General Public License
  * along with this program.  If not, see <http://www.gnu.org/licenses/>.
  */
  
#include <priv/FileDownload.h>
using namespace DM;

#include <QTimer>

#include <limits>

#include <Common/Settings.h>
#include <Common/ProtoHelper.h>
#include <Common/Hashes.h>

#include <Core/FileManager/Exceptions.h>

#include <priv/Log.h>
#include <priv/Constants.h>

MTRand FileDownload::mtrand;

FileDownload::FileDownload(
   QSharedPointer<FM::IFileManager> fileManager,
   QSharedPointer<PM::IPeerManager> peerManager,
   OccupiedPeers& occupiedPeersAskingForHashes,
   OccupiedPeers& occupiedPeersDownloadingChunk,
   Common::Hash peerSourceID,
   const Protos::Common::Entry& remoteEntry,
   const Protos::Common::Entry& localEntry,
   Common::TransferRateCalculator& transferRateCalculator,
   bool complete
) :
   Download(peerSourceID, remoteEntry, localEntry),
   fileManager(fileManager),
   peerManager(peerManager),
   NB_CHUNK(this->remoteEntry.size() / SETTINGS.get<quint32>("chunk_size") + (this->remoteEntry.size() % SETTINGS.get<quint32>("chunk_size") == 0 ? 0 : 1)),
   occupiedPeersAskingForHashes(occupiedPeersAskingForHashes),
   occupiedPeersDownloadingChunk(occupiedPeersDownloadingChunk),
   nbHashesKnown(0),
   fileCreated(false),
   transferRateCalculator(transferRateCalculator)
{
   L_DEBU(QString("New FileDownload : peer source = %1, remoteEntry : \n%2\nlocalEntry : \n%3").
      arg(this->peerSourceID.toStr()).
      arg(Common::ProtoHelper::getDebugStr(this->remoteEntry)).
      arg(Common::ProtoHelper::getDebugStr(this->localEntry))
   );

   if (complete)
      this->status = COMPLETE;

   // We create a ChunkDownload for each known hash in the entry.
   for (int i = 0; i < this->remoteEntry.chunk_size(); i++)
   {
      Common::Hash chunkHash(this->remoteEntry.chunk(i).hash().data());
      QSharedPointer<ChunkDownload> chunkDownload = QSharedPointer<ChunkDownload>(new ChunkDownload(this->peerManager, this->occupiedPeersDownloadingChunk, chunkHash, this->transferRateCalculator));

      this->chunkDownloads << chunkDownload;
      this->connectChunkDownloadSignals(this->chunkDownloads.last());
   }
   this->nbHashesKnown = this->chunkDownloads.size();
}

FileDownload::~FileDownload()
{
   this->status = DELETED;

   this->occupiedPeersAskingForHashes.setPeerAsFree(this->peerSource);

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

   this->retrieveHashes();
}

/**
  * Add the known hashes.
  */
void FileDownload::populateRemoteEntry(Protos::Queue::Queue_Entry* entry) const
{
   Download::populateRemoteEntry(entry);

   for (int i = entry->remote_entry().chunk_size(); i < this->chunkDownloads.size(); i++)
   {
      Protos::Common::Hash* hash = entry->mutable_remote_entry()->add_chunk();
      hash->set_hash(this->chunkDownloads[i]->getHash().getData(), Common::Hash::HASH_SIZE);
   }
}

int FileDownload::getProgress() const
{
   if (this->status == COMPLETE)
      return 100;

   quint64 knownBytes = 0;
   for (QListIterator< QSharedPointer<ChunkDownload> > i(this->chunkDownloads); i.hasNext();)
   {
      knownBytes += i.next()->getDownloadedBytes();
   }

   return 100LL * knownBytes / this->remoteEntry.size();
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
      // Choose a chunk with the less number of peer. (rarest first).
      QList< QSharedPointer<ChunkDownload> > chunksReadyToDownload;
      int bestNbPeer = std::numeric_limits<int>::max();
      for (QListIterator< QSharedPointer<ChunkDownload> > i(this->chunkDownloads); i.hasNext();)
      {
         QSharedPointer<ChunkDownload> chunkDownload = i.next();
         int nbPeer = chunkDownload->isReadyToDownload();
         if (nbPeer == 0)
            continue;
         else if (nbPeer == bestNbPeer)
            chunksReadyToDownload << chunkDownload;
         else if (nbPeer < bestNbPeer)
         {
            chunksReadyToDownload.clear();
            chunksReadyToDownload << chunkDownload;
            bestNbPeer = nbPeer;
         }
      }

      // If there is many chunk with the same best speed we choose randomly one of them.
      QSharedPointer<ChunkDownload> chunkDownload;
      switch (chunksReadyToDownload.size())
      {
      case 0:
         break;
      case 1:
         chunkDownload = chunksReadyToDownload.first();
         break;
      default:
         chunkDownload = chunksReadyToDownload[mtrand.randInt(chunksReadyToDownload.size() - 1)];
      }

      if (!chunkDownload.isNull() && !this->fileCreated)
      {
         // First, try to get the chunks from an existing file, it's useful when a download is taken from the saved queue.
         if (!this->chunkDownloads.isEmpty())
         {
            Common::Hashes hashes;
            for (QListIterator< QSharedPointer<ChunkDownload> > i(this->chunkDownloads); i.hasNext();)
               hashes << i.next()->getHash();
            this->chunksWithoutDownload = this->fileManager->getAllChunks(this->localEntry, hashes);
         }

         // If the file doesn't exist we create it.
         if (this->chunksWithoutDownload.isEmpty())
            this->chunksWithoutDownload = this->fileManager->newFile(this->localEntry);

         for (int i = 0; !this->chunksWithoutDownload.isEmpty() && i < this->chunkDownloads.size(); i++)
            this->chunkDownloads[i]->setChunk(this->chunksWithoutDownload.takeFirst());

         this->fileCreated = true;

         // 'getAllChunks(..)' above can return some completed chunks.
         if (!chunkDownload->getChunk().isNull() && chunkDownload->getChunk()->isComplete())
         {
            this->updateStatus(); // Maybe all the file is complete, so we update the status.
            return QSharedPointer<ChunkDownload>();
         }
      }

      return chunkDownload;

   }
   catch(FM::NoWriteableDirectoryException&)
   {
      L_DEBU(QString("There is no shared directory with writting rights for this download : %1").arg(Common::ProtoHelper::getStr(this->remoteEntry, &Protos::Common::Entry::name)));
      this->status = NO_SHARED_DIRECTORY_TO_WRITE;
   }
   catch(FM::InsufficientStorageSpaceException&)
   {
      L_DEBU(QString("There is no enough space storage available for this download : %1").arg(Common::ProtoHelper::getStr(this->remoteEntry, &Protos::Common::Entry::name)));
      this->status = NO_ENOUGH_FREE_SPACE;
   }
   catch(FM::UnableToCreateNewFileException&)
   {
      L_DEBU(QString("Unable to create the file, download : %1").arg(Common::ProtoHelper::getStr(this->remoteEntry, &Protos::Common::Entry::name)));
      this->status = UNABLE_TO_CREATE_THE_FILE;
   }

   return QSharedPointer<ChunkDownload>();
}

/**
  * Fills 'chunks' with the unfinished chunk of the file. Do not add more than 'nMax' chunk to chunks.
  */
void FileDownload::getUnfinishedChunks(QList< QSharedPointer<IChunkDownload> >& chunks, int nMax) const
{
   if (this->status == COMPLETE || this->status == DELETED)
      return;

   for (int i = 0; i < this->chunkDownloads.size() && i < nMax; i++)
   {
      if (!this->chunkDownloads[i]->isComplete())
         chunks << this->chunkDownloads[i];
   }
}

/**
  * Return true if a GetHashes request has been sent to the peer.
  */
bool FileDownload::retrieveHashes()
{
   // If we've already got all the chunk hashes it's unecessary to re-ask them.
   // Or if we'v got anyone to ask the chunk hashes..
   if (
      this->nbHashesKnown == this->NB_CHUNK ||
      !this->hasAValidPeer() ||
      this->status == COMPLETE || this->status == DELETED || this->status == UNABLE_TO_RETRIEVE_THE_HASHES ||
      !this->occupiedPeersAskingForHashes.setPeerAsOccupied(this->peerSource)
   )
      return false;

   this->status = INITIALIZING;

   this->getHashesResult = this->peerSource->getHashes(this->remoteEntry);
   connect(this->getHashesResult.data(), SIGNAL(result(const Protos::Core::GetHashesResult&)), this, SLOT(result(const Protos::Core::GetHashesResult&)));
   connect(this->getHashesResult.data(), SIGNAL(nextHash(const Common::Hash&)), this, SLOT(nextHash(const Common::Hash&)));
   connect(this->getHashesResult.data(), SIGNAL(timeout()), this, SLOT(getHashTimeout()));
   this->getHashesResult->start();

   return true;
}

void FileDownload::retryToRetrieveHashes()
{
   this->status = QUEUED;
   if (!this->retrieveHashes())
      this->updateStatus();
}

void FileDownload::result(const Protos::Core::GetHashesResult& result)
{
   if (result.status() == Protos::Core::GetHashesResult_Status_OK)
   {
      if (this->nbHashesKnown + static_cast<int>(result.nb_hash()) != this->NB_CHUNK)
         L_WARN(QString("The received hashes (%1) plus the known hashes (%2) is not equal to the number of chunks (%3)").arg(result.nb_hash()).arg(this->nbHashesKnown).arg(this->NB_CHUNK));
   }
   else
   {
      L_DEBU(QString("Unable to retrieve the hashes, error = %1").arg(result.status()));
      this->getHashesResult.clear();
      this->status = UNABLE_TO_RETRIEVE_THE_HASHES;
      this->occupiedPeersAskingForHashes.setPeerAsFree(this->peerSource);
      QTimer::singleShot(RETRY_PEER_GET_HASHES_PERIOD, this, SLOT(retryToRetrieveHashes()));
   }
}

void FileDownload::nextHash(const Common::Hash& hash)
{
   L_DEBU(QString("New Hash received : %1").arg(hash.toStr()));

   if (++this->nbHashesKnown == this->NB_CHUNK)
   {
      this->getHashesResult.clear();
      this->occupiedPeersAskingForHashes.setPeerAsFree(this->peerSource);

      this->updateStatus();
   }

   if (this->chunkDownloads.size() >= this->nbHashesKnown && this->chunkDownloads[this->nbHashesKnown-1]->getHash() != hash)
   {
      L_WARN(
         QString("The hash (%1) num %2 received doesn't match the hash (%3) in the entry").
            arg(hash.toStr()).
            arg(this->nbHashesKnown-1).
            arg(this->chunkDownloads[this->nbHashesKnown-1]->getHash().toStr())
      );
   }
   else
   {
      QSharedPointer<ChunkDownload> chunkDownload = QSharedPointer<ChunkDownload>(new ChunkDownload(this->peerManager, this->occupiedPeersDownloadingChunk, hash, transferRateCalculator));

      // If the file has already been created, the chunks are known.
      if (!this->chunksWithoutDownload.isEmpty())
         chunkDownload->setChunk(this->chunksWithoutDownload.takeFirst());

      this->chunkDownloads << chunkDownload;
      this->connectChunkDownloadSignals(chunkDownload);
      chunkDownload->setPeerSource(this->peerSource); // May start a download.
      this->remoteEntry.add_chunk()->set_hash(hash.getData(), Common::Hash::HASH_SIZE); // Used during the saving of the queue, see Download::populateEntry(..).
      emit newHashKnown();
   }
}

void FileDownload::getHashTimeout()
{
   L_DEBU("Unable to retrieve the hashes : timeout");
   this->getHashesResult.clear();
   this->status = UNABLE_TO_RETRIEVE_THE_HASHES;
   this->occupiedPeersAskingForHashes.setPeerAsFree(this->peerSource);
   QTimer::singleShot(RETRY_PEER_GET_HASHES_PERIOD, this, SLOT(retryToRetrieveHashes()));
}

void FileDownload::chunkDownloadStarted()
{
   this->status = DOWNLOADING;
}

void FileDownload::chunkDownloadFinished()
{
   this->updateStatus();
}

void FileDownload::setStatus(Status newStatus)
{
   if (this->status == COMPLETE)
      return;

   // We don't care about the source peer if we have all the hashes.
   if (newStatus == UNKNOWN_PEER && nbHashesKnown == this->NB_CHUNK)
      return;

   Download::setStatus(newStatus);
}

void FileDownload::updateStatus()
{
   if (this->status == DELETED || this->status == COMPLETE)
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
         else
            this->status = QUEUED;
      }
   }

   if (!this->getHashesResult.isNull())
      this->status = INITIALIZING;

   if (this->status == COMPLETE)
   {
      const QString sharedDir = this->fileManager->getSharedDir(this->localEntry.shared_dir().id().hash().data());
      L_USER(QString("File completed : %1%2%3")
         .arg(sharedDir.left(sharedDir.size() - 1)) // remove the ending '/'.
         .arg(Common::ProtoHelper::getStr(this->localEntry, &Protos::Common::Entry::path))
         .arg(Common::ProtoHelper::getStr(this->localEntry, &Protos::Common::Entry::name))
      );
   }
}

void FileDownload::connectChunkDownloadSignals(QSharedPointer<ChunkDownload> chunkDownload)
{
   connect(chunkDownload.data(), SIGNAL(downloadStarted()), this, SLOT(chunkDownloadStarted()), Qt::DirectConnection);
   connect(chunkDownload.data(), SIGNAL(downloadFinished()), this, SLOT(chunkDownloadFinished()), Qt::DirectConnection);
}
