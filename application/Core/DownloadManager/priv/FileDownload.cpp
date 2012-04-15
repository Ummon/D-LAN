/**
  * D-LAN - A decentralized LAN file sharing software.
  * Copyright (C) 2010-2012 Greg Burri <greg.burri@gmail.com>
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
   LinkedPeers& linkedPeers,
   OccupiedPeers& occupiedPeersAskingForHashes,
   OccupiedPeers& occupiedPeersDownloadingChunk,
   Common::ThreadPool& threadPool,
   PM::IPeer* peerSource,
   const Protos::Common::Entry& remoteEntry,
   const Protos::Common::Entry& localEntry,
   Common::TransferRateCalculator& transferRateCalculator,
   Protos::Queue::Queue::Entry::Status status
) :
   Download(peerSource, remoteEntry, localEntry),
   fileManager(fileManager),
   linkedPeers(linkedPeers),
   NB_CHUNK(this->remoteEntry.size() / SETTINGS.get<quint32>("chunk_size") + (this->remoteEntry.size() % SETTINGS.get<quint32>("chunk_size") == 0 ? 0 : 1)),
   occupiedPeersAskingForHashes(occupiedPeersAskingForHashes),
   occupiedPeersDownloadingChunk(occupiedPeersDownloadingChunk),
   threadPool(threadPool),
   nbHashesKnown(0),
   transferRateCalculator(transferRateCalculator)
{
   L_DEBU(QString("New FileDownload : peer source = %1, remoteEntry : \n%2\nlocalEntry : \n%3").
      arg(this->peerSource->toStringLog()).
      arg(Common::ProtoHelper::getDebugStr(this->remoteEntry)).
      arg(Common::ProtoHelper::getDebugStr(this->localEntry))
   );

   this->setStatus(static_cast<Status>(status));

   // We create a ChunkDownload for each known hash in the entry.
   for (int i = 0; i < this->remoteEntry.chunk_size(); i++)
   {
      Common::Hash chunkHash(this->remoteEntry.chunk(i).hash());
      QSharedPointer<ChunkDownload> chunkDownload = QSharedPointer<ChunkDownload>(new ChunkDownload(this->linkedPeers, this->occupiedPeersDownloadingChunk, this->transferRateCalculator, this->threadPool, chunkHash));

      this->chunkDownloads << chunkDownload;
      this->connectChunkDownloadSignals(this->chunkDownloads.last());
   }
   this->nbHashesKnown = this->chunkDownloads.size();
}

FileDownload::~FileDownload()
{
   this->setStatus(DELETED);

   if (!this->getHashesResult.isNull())
   {
      this->getHashesResult.clear();
      this->occupiedPeersAskingForHashes.setPeerAsFree(this->peerSource);
   }

   this->chunksWithoutDownload.clear();
   this->chunkDownloads.clear();
}

void FileDownload::start()
{
   this->tryToLinkToAnExistingFile();

   if (this->hasAValidPeer())
      this->peerSourceBecomesAvailable();

   this->updateStatus();

   if (this->hasAValidPeer())
      this->occupiedPeersDownloadingChunk.newPeer(this->peerSource);

   this->retrieveHashes();
}

void FileDownload::stop()
{
   if (!this->getHashesResult.isNull())
   {
      this->getHashesResult.clear();
      this->occupiedPeersAskingForHashes.setPeerAsFree(this->peerSource);
   }

   for (QListIterator< QSharedPointer<ChunkDownload> > i(this->chunkDownloads); i.hasNext();)
      i.next()->stop();
}

bool FileDownload::pause(bool pause)
{
   if (this->status == COMPLETE || this->status == DELETED)
      return false;

   if (pause && this->status != PAUSED)
   {
      this->setStatus(PAUSED);
      this->stop();
      return true;
   }
   else if (!pause && this->status == PAUSED)
   {
      this->setStatus(QUEUED);
      this->retrieveHashes();
      return true;
   }

   return false;
}

void FileDownload::peerSourceBecomesAvailable()
{
   if (this->status == UNKNOWN_PEER_SOURCE)
      this->setStatus(QUEUED);

   for (QListIterator< QSharedPointer<ChunkDownload> > i(this->chunkDownloads); i.hasNext();)
      i.next()->setPeerSource(this->peerSource, false); // 'false' : to avoid to send unnecessary 'newFreePeer'.
}

/**
  * Add the known hashes.
  */
void FileDownload::populateQueueEntry(Protos::Queue::Queue::Entry* entry) const
{
   Download::populateQueueEntry(entry);

   for (int i = entry->remote_entry().chunk_size(); i < this->chunkDownloads.size(); i++)
   {
      Protos::Common::Hash* hash = entry->mutable_remote_entry()->add_chunk();
      hash->set_hash(this->chunkDownloads[i]->getHash().getData(), Common::Hash::HASH_SIZE);
   }
}

quint64 FileDownload::getDownloadedBytes() const
{
   if (this->status == COMPLETE)
      return this->remoteEntry.size();

   quint64 knownBytes = 0;
   for (QListIterator< QSharedPointer<ChunkDownload> > i(this->chunkDownloads); i.hasNext();)
      knownBytes += i.next()->getDownloadedBytes();
   return knownBytes;
}

QSet<Common::Hash> FileDownload::getPeers() const
{
   QSet<Common::Hash> peerIDs;
   for (QListIterator< QSharedPointer<ChunkDownload> > i(this->chunkDownloads); i.hasNext();)
      peerIDs += i.next()->getPeers().toSet();
   return peerIDs;
}

/**
  * If there is a ChunkDownload with a free peer (we do not already download from this peer) the return the chunk.
  * The file is created on the fly with IFileManager::newFile(..) if we don't have the IChunks.
  * @return The chunk to download, can return a null pointer if an error occurs.
  */
QSharedPointer<ChunkDownload> FileDownload::getAChunkToDownload()
{
   if (this->status == COMPLETE || this->status == DELETED || this->status == PAUSED)
      return QSharedPointer<ChunkDownload>();

   // Choose a chunk with the less number of peer. (rarest first).
   // Choose first a partially downloaded chunk.
   QList< QSharedPointer<ChunkDownload> > chunksReadyToDownload;
   int bestNbPeer = std::numeric_limits<int>::max();
   for (QListIterator< QSharedPointer<ChunkDownload> > i(this->chunkDownloads); i.hasNext();)
   {
      const QSharedPointer<ChunkDownload>& chunkDownload = i.next();

      const int nbPeer = chunkDownload->isReadyToDownload();

      if (nbPeer == 0)
      {
         continue;
      }
      else
      {
         if (chunkDownload->isPartiallyDownloaded())
         {
            chunksReadyToDownload.clear();
            chunksReadyToDownload << chunkDownload;
            break;
         }
         else if (nbPeer == bestNbPeer)
            chunksReadyToDownload << chunkDownload;
         else if (nbPeer < bestNbPeer)
         {
            chunksReadyToDownload.clear();
            chunksReadyToDownload << chunkDownload;
            bestNbPeer = nbPeer;
         }
      }
   }

   if (chunksReadyToDownload.isEmpty())
      return QSharedPointer<ChunkDownload>();

   // If there is many chunk with the same best speed we choose randomly one of them.
   QSharedPointer<ChunkDownload> chunkDownload =
      chunksReadyToDownload.size() == 1 ? chunksReadyToDownload.first() : chunksReadyToDownload[mtrand.randInt(chunksReadyToDownload.size() - 1)];

   if (!this->localEntry.exists())
   {
      try
      {
         this->chunksWithoutDownload = this->fileManager->newFile(this->localEntry);

         for (int i = 0; !this->chunksWithoutDownload.isEmpty() && i < this->chunkDownloads.size(); i++)
            this->chunkDownloads[i]->setChunk(this->chunksWithoutDownload.takeFirst());
      }
      catch(FM::NoWriteableDirectoryException&)
      {
         L_DEBU(QString("There is no shared directory with writting rights for this download : %1").arg(Common::ProtoHelper::getStr(this->remoteEntry, &Protos::Common::Entry::name)));
         this->setStatus(NO_SHARED_DIRECTORY_TO_WRITE);
         return QSharedPointer<ChunkDownload>();
      }
      catch(FM::InsufficientStorageSpaceException&)
      {
         L_DEBU(QString("There is no enough space storage available for this download : %1").arg(Common::ProtoHelper::getStr(this->remoteEntry, &Protos::Common::Entry::name)));
         this->setStatus(NO_ENOUGH_FREE_SPACE);
         return QSharedPointer<ChunkDownload>();
      }
      catch(FM::UnableToCreateNewFileException&)
      {
         L_DEBU(QString("Unable to create the file, download : %1").arg(Common::ProtoHelper::getStr(this->remoteEntry, &Protos::Common::Entry::name)));
         this->setStatus(UNABLE_TO_CREATE_THE_FILE);
         return QSharedPointer<ChunkDownload>();
      }

      // 'newFile(..)' above can return some completed chunks.
      if (!chunkDownload->getChunk().isNull() && chunkDownload->getChunk()->isComplete())
      {
         this->updateStatus(); // Maybe all the file is complete, so we update the status.
         return QSharedPointer<ChunkDownload>();
      }
   }

   return chunkDownload;
}

/**
  * Fills 'chunks' with the unfinished chunk of the file. Do not add more than 'nMax' chunk to chunks.
  */
void FileDownload::getUnfinishedChunks(QList< QSharedPointer<IChunkDownload> >& chunks, int nMax)
{
   if (this->status == COMPLETE || this->status == DELETED || this->status == PAUSED)
      return;

   for (int i = 0; i < this->chunkDownloads.size() && i < nMax; i++)
   {
      if (!this->chunkDownloads[i]->isComplete())
         chunks << this->chunkDownloads[i];
   }

   if (chunks.size() == this->chunkDownloads.size())
   {
      const QTime oldTime = this->lastTimeGetAllUnfinishedChunks;
      this->lastTimeGetAllUnfinishedChunks = QTime::currentTime();
      emit lastTimeGetAllUnfinishedChunksChanged(oldTime);
   }
}

QTime FileDownload::getLastTimeGetAllUnfinishedChunks() const
{
   return this->lastTimeGetAllUnfinishedChunks;
}

/**
  * When we explicitly remove a download, we must remove all unfinished files.
  */
void FileDownload::remove()
{
   this->setStatus(DELETED); // To avoid the call to 'stop()' to relaunch a download (via occupiedPeersDownloadingChunk::setPeerAsFree(..) -> DownloadManager::scanTheQueue()).
   this->stop();

   for (QListIterator< QSharedPointer<ChunkDownload> > i(this->chunkDownloads); i.hasNext();)
      i.next()->tryToRemoveItsIncompleteFile();

   Download::remove();
}

/**
  * Update the file status depending of the states of its chunks.
  * If all chunks are complete -> COMPLETE
  * If there is downloading chunk -> DOWNLOADING
  * If a GET_HASHES request is running -> INITIALIZING
  * If all the incomplete chunks have no peer -> NO_SOURCE
  * Else -> QUEUED
  * @return
  */
bool FileDownload::updateStatus()
{
   if (Download::updateStatus())
      return true;

   Status newStatus = this->status;

   if (this->chunkDownloads.size() == NB_CHUNK)
      newStatus = COMPLETE;

   bool hasAtLeastAPeer = false;
   for (QListIterator< QSharedPointer<ChunkDownload> > i(this->chunkDownloads); i.hasNext();)
   {
      QSharedPointer<ChunkDownload> chunkDownload = i.next();

      if (chunkDownload->isDownloading())
      {
         this->setStatus(DOWNLOADING);
         return false;
      }
      else if (chunkDownload->getLastTransfertStatus() >= 0x20)
      {
         newStatus = chunkDownload->getLastTransfertStatus();
         chunkDownload->resetLastTransfertStatus();

         // If the local file disappear we reset the download.
         if (newStatus == FILE_NON_EXISTENT)
         {
            this->reset();
            this->setStatus(newStatus);
            return false;
         }
      }
      else if (!hasAtLeastAPeer && !chunkDownload->isComplete())
      {
         if (chunkDownload->hasAtLeastAPeer())
         {
            hasAtLeastAPeer = true;
            newStatus = QUEUED;
         }
         else
         {
            newStatus = NO_SOURCE;
         }
      }
   }

   if (newStatus == COMPLETE)
   {
      const QString sharedDir = this->fileManager->getSharedDir(this->localEntry.shared_dir().id().hash());
      L_USER(QString(tr("File completed: %1%2%3"))
         .arg(sharedDir.left(sharedDir.size() - 1)) // remove the ending '/'.
         .arg(Common::ProtoHelper::getStr(this->localEntry, &Protos::Common::Entry::path))
         .arg(Common::ProtoHelper::getStr(this->localEntry, &Protos::Common::Entry::name))
      );
   }
   else if(!this->getHashesResult.isNull())
   {
      newStatus = GETTING_THE_HASHES;
   }

   this->setStatus(newStatus);

   return false;
}

/**
  * Return true if a GetHashes request has been sent to the peer.
  */
bool FileDownload::retrieveHashes()
{
   // If we've already got all the chunk hashes it's unecessary to re-ask them.
   // Or if we'v got anyone to ask the chunk hashes..
   if (this->nbHashesKnown == this->NB_CHUNK || this->status == COMPLETE || this->status == DELETED || this->status == PAUSED || this->status == UNABLE_TO_RETRIEVE_THE_HASHES)
      return false;

   if (!this->hasAValidPeer())
   {
      this->setStatus(UNKNOWN_PEER_SOURCE);
      return false;
   }

   if (!this->occupiedPeersAskingForHashes.setPeerAsOccupied(this->peerSource))
      return false;

   this->setStatus(GETTING_THE_HASHES);

   this->getHashesResult = this->peerSource->getHashes(this->remoteEntry);
   connect(this->getHashesResult.data(), SIGNAL(result(const Protos::Core::GetHashesResult&)), this, SLOT(result(const Protos::Core::GetHashesResult&)));
   connect(this->getHashesResult.data(), SIGNAL(nextHash(const Common::Hash&)), this, SLOT(nextHash(const Common::Hash&)));
   connect(this->getHashesResult.data(), SIGNAL(timeout()), this, SLOT(getHashTimeout()));
   this->getHashesResult->start();

   return true;
}

void FileDownload::retryToRetrieveHashes()
{
   this->setStatus(QUEUED);
   if (!this->retrieveHashes())
      this->updateStatus();
}

void FileDownload::result(const Protos::Core::GetHashesResult& result)
{
   if (result.status() == Protos::Core::GetHashesResult::OK)
   {
      if (this->nbHashesKnown + static_cast<int>(result.nb_hash()) != this->NB_CHUNK)
         L_WARN(QString("The received hashes (%1) plus the known hashes (%2) is not equal to the number of chunks (%3)").arg(result.nb_hash()).arg(this->nbHashesKnown).arg(this->NB_CHUNK));
   }
   else
   {
      if (result.status() == Protos::Core::GetHashesResult::DONT_HAVE)
      {
         L_DEBU("Unable to retrieve the hashes: DONT_HAVE");
         this->setStatus(ENTRY_NOT_FOUND);
      }
      else
      {
         L_DEBU("Unable to retrieve the hashes: ERROR_UNKNOWN");
         this->setStatus(UNABLE_TO_RETRIEVE_THE_HASHES);
      }

      this->getHashesResult.clear();
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
      QSharedPointer<ChunkDownload> chunkDownload = QSharedPointer<ChunkDownload>(new ChunkDownload(this->linkedPeers, this->occupiedPeersDownloadingChunk, this->transferRateCalculator, this->threadPool, hash));

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
   this->setStatus(UNABLE_TO_RETRIEVE_THE_HASHES);
   this->occupiedPeersAskingForHashes.setPeerAsFree(this->peerSource);
   QTimer::singleShot(RETRY_PEER_GET_HASHES_PERIOD, this, SLOT(retryToRetrieveHashes()));
}

void FileDownload::chunkDownloadStarted()
{
   this->setStatus(DOWNLOADING);
}

void FileDownload::chunkDownloadFinished()
{
   this->updateStatus();
}

/**
  * Look if a file in the cache ('FM::IFileManager') owns the known hashes. If so, the chunks ('FM:IChunk') are given to each 'ChunkDownload' and
  * 'this->local_entry().exists' is set to true.
  * @return 'true' is the file exists.
  */
bool FileDownload::tryToLinkToAnExistingFile()
{
   this->localEntry.set_exists(false);

   if (!this->chunkDownloads.isEmpty())
   {
      Common::Hashes hashes;
      for (QListIterator< QSharedPointer<ChunkDownload> > i(this->chunkDownloads); i.hasNext();)
         hashes << i.next()->getHash();
      this->chunksWithoutDownload = this->fileManager->getAllChunks(this->localEntry, hashes);

      if (!this->chunksWithoutDownload.isEmpty())
         this->localEntry.set_exists(true);

      for (int i = 0; !this->chunksWithoutDownload.isEmpty() && i < this->chunkDownloads.size(); i++)
         this->chunkDownloads[i]->setChunk(this->chunksWithoutDownload.takeFirst());
   }

   return this->localEntry.exists();
}

void FileDownload::connectChunkDownloadSignals(QSharedPointer<ChunkDownload> chunkDownload)
{
   connect(chunkDownload.data(), SIGNAL(downloadStarted()), this, SLOT(chunkDownloadStarted()), Qt::DirectConnection);
   connect(chunkDownload.data(), SIGNAL(downloadFinished()), this, SLOT(chunkDownloadFinished()), Qt::DirectConnection);
   connect(chunkDownload.data(), SIGNAL(numberOfPeersChanged()), this, SLOT(updateStatus()), Qt::DirectConnection);
}

/**
  * Reset all download chunk and set the local file as non-existent.
  */
void FileDownload::reset()
{
   this->chunksWithoutDownload.clear();
   for (QListIterator< QSharedPointer<ChunkDownload> > i(this->chunkDownloads); i.hasNext();)
      i.next()->reset();
   this->localEntry.set_exists(false);
   this->localEntry.clear_shared_dir();
}
