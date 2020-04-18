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
#include <QSet>
#include <QRandomGenerator64>

#include <limits>

#include <Common/Settings.h>
#include <Common/ProtoHelper.h>
#include <Common/Hashes.h>
#include <Common/Constants.h>

#include <Core/FileManager/Exceptions.h>

#include <priv/Log.h>
#include <priv/Constants.h>

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
   Download(fileManager, peerSource, remoteEntry, localEntry),
   linkedPeers(linkedPeers),
   NB_CHUNK(this->remoteEntry.size() / Common::Constants::CHUNK_SIZE + (this->remoteEntry.size() % Common::Constants::CHUNK_SIZE == 0 ? 0 : 1)),
   nbChunkAsked(0),
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

   // We create a 'ChunkDownloader' for each known chunk in the entry.
   for (int i = 0; i < this->NB_CHUNK; i++)
   {
      QSharedPointer<ChunkDownloader> chunkDownloader = (i < this->remoteEntry.chunk_size() && this->remoteEntry.chunk(i).hash().size() > 0) ?
         (new ChunkDownloader(this->linkedPeers, this->occupiedPeersDownloadingChunk, this->transferRateCalculator, this->threadPool, Common::Hash(this->remoteEntry.chunk(i).hash())))->grabStrongRef()
         : QSharedPointer<ChunkDownloader>();

      this->chunkDownloaders << chunkDownloader;

      if (!chunkDownloader.isNull())
      {
         this->nbHashesKnown++;
         this->connectChunkDownloaderSignals(this->chunkDownloaders.last());
      }
   }
}

FileDownload::~FileDownload()
{
   this->setStatus(DELETED);

   if (!this->getHashesResult.isNull())
   {
      this->getHashesResult.clear();
      this->occupiedPeersAskingForHashes.setPeerAsFree(this->peerSource);
   }

   this->chunksWithoutDownloader.clear();
   this->chunkDownloaders.clear();
}

void FileDownload::start()
{
   this->tryToLinkToAnExistingFile();

   if (this->hasAValidPeerSource())
      this->peerSourceBecomesAvailable();

   this->updateStatus();

   // If the file is empty we create it now.
   if (this->localEntry.size() == 0 && !this->localEntry.exists())
      this->createFile();

   if (!this->retrieveHashes() && this->hasAValidPeerSource())
      this->occupiedPeersDownloadingChunk.newPeer(this->peerSource);
}

void FileDownload::stop()
{
   if (!this->getHashesResult.isNull())
   {
      this->getHashesResult.clear();
      this->occupiedPeersAskingForHashes.setPeerAsFree(this->peerSource);
   }

   for (QListIterator<QSharedPointer<ChunkDownloader>> i(this->chunkDownloaders); i.hasNext();)
   {
      auto chunkDownloader = i.next();
      if (!chunkDownloader.isNull())
         chunkDownloader->stop();
   }
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

   for (QListIterator<QSharedPointer<ChunkDownloader>> i(this->chunkDownloaders); i.hasNext();)
   {
      auto chunkDownloader = i.next();
      if (!chunkDownloader.isNull())
         chunkDownloader->setPeerSource(this->peerSource, false); // 'false' : to avoid to send unnecessary 'newFreePeer'.
   }
}

/**
  * Add the known hashes.
  */
void FileDownload::populateQueueEntry(Protos::Queue::Queue::Entry* entry) const
{
   Download::populateQueueEntry(entry);

   for (int i = 0; i < this->chunkDownloaders.size() && i < entry->remote_entry().chunk_size(); i++)
      if (!entry->remote_entry().chunk(i).hash().size() > 0 && !this->chunkDownloaders[i].isNull())
         entry->mutable_remote_entry()->mutable_chunk(i)->set_hash(this->chunkDownloaders[i]->getHash().getData(), Common::Hash::HASH_SIZE);
}

quint64 FileDownload::getDownloadedBytes() const
{
   if (this->status == COMPLETE)
      return this->remoteEntry.size();

   quint64 knownBytes = 0;
   for (QListIterator<QSharedPointer<ChunkDownloader>> i(this->chunkDownloaders); i.hasNext();)
   {
      auto chunkDownloader = i.next();
      if (!chunkDownloader.isNull())
         knownBytes += chunkDownloader->getDownloadedBytes();
   }
   return knownBytes;
}

QSet<PM::IPeer*> FileDownload::getPeers() const
{
   QSet<PM::IPeer*> peers;
   for (QListIterator<QSharedPointer<ChunkDownloader>> i(this->chunkDownloaders); i.hasNext();)
   {
      auto chunkDownloader = i.next();
      if (!chunkDownloader.isNull())
      {
         auto peersFromChunkDownloader = chunkDownloader->getPeers();
         peers += QSet(peersFromChunkDownloader.begin(), peersFromChunkDownloader.end());
      }
   }
   return peers;
}

/**
  * If there is a ChunkDownloader with a free peer (we do not already download from this peer) the return the chunk.
  * The file is created on the fly with IFileManager::newFile(..) if we don't have the IChunks.
  * @return The chunk to download, can return a null pointer if an error occurs.
  */
QSharedPointer<ChunkDownloader> FileDownload::getAChunkToDownload()
{
   if (this->status == COMPLETE || this->status == DELETED || this->status == PAUSED)
      return QSharedPointer<ChunkDownloader>();

   // Choose a chunk with the less number of peer. (rarest first).
   // Choose first a partially downloaded chunk.
   QList<QSharedPointer<ChunkDownloader>> chunksReadyToDownload;
   int bestNbPeer = std::numeric_limits<int>::max();
   for (QListIterator<QSharedPointer<ChunkDownloader>> i(this->chunkDownloaders); i.hasNext();)
   {
      auto chunkDownloader = i.next();
      if (chunkDownloader.isNull())
         continue;

      const int nbPeer = chunkDownloader->isReadyToDownload();

      if (nbPeer == 0)
      {
         continue;
      }
      else
      {
         if (chunkDownloader->isPartiallyDownloaded())
         {
            chunksReadyToDownload.clear();
            chunksReadyToDownload << chunkDownloader;
            break;
         }
         else if (nbPeer == bestNbPeer)
            chunksReadyToDownload << chunkDownloader;
         else if (nbPeer < bestNbPeer)
         {
            chunksReadyToDownload.clear();
            chunksReadyToDownload << chunkDownloader;
            bestNbPeer = nbPeer;
         }
      }
   }

   if (chunksReadyToDownload.isEmpty())
      return QSharedPointer<ChunkDownloader>();

   // If there is many chunk with the same number of peer we choose randomly one of them.
   QSharedPointer<ChunkDownloader> chunkDownloader =
         chunksReadyToDownload.size() == 1
            ? chunksReadyToDownload.first()
            : chunksReadyToDownload[QRandomGenerator64::global()->bounded(chunksReadyToDownload.size())];

   if (!this->localEntry.exists())
   {
      if (!this->createFile())
         return QSharedPointer<ChunkDownloader>();

      // 'newFile(..)' above can return some completed chunks.
      if (!chunkDownloader->getChunk().isNull() && chunkDownloader->getChunk()->isComplete())
      {
         this->updateStatus(); // Maybe all the file is complete, so we update the status.
         return QSharedPointer<ChunkDownloader>();
      }
   }

   return chunkDownloader;
}

/**
  * Fills 'chunks' with the unfinished chunk of the file. Do not add more than 'nMax' chunk to chunks.
  */
void FileDownload::getUnfinishedChunks(QList<QSharedPointer<IChunkDownloader>>& chunks, int nMax, bool notAlreadyAsked)
{
   if (this->status == COMPLETE || this->status == DELETED || this->status == PAUSED)
      return;

   int n = 0;
   for (int i = notAlreadyAsked ? this->nbChunkAsked : 0; i < this->chunkDownloaders.size() && n < nMax; i++)
   {
      if (this->chunkDownloaders[i].isNull())
         continue;

      if (!this->chunkDownloaders[i]->isComplete())
      {
         chunks << this->chunkDownloaders[i];
         n++;
      }
      if (i >= this->nbChunkAsked)
         this->nbChunkAsked++;
   }

   if (this->nbChunkAsked == this->nbHashesKnown)
   {
      const QTime oldTime = this->lastTimeGetAllUnfinishedChunks;
      this->lastTimeGetAllUnfinishedChunks = QTime::currentTime();
      this->nbChunkAsked = 0;
      emit lastTimeGetAllUnfinishedChunksChanged(oldTime);
   }
}

/**
  * When we explicitly remove a download, we must remove all unfinished files.
  */
void FileDownload::remove()
{
   this->setStatus(DELETED); // To avoid the call to 'stop()' to relaunch a download (via occupiedPeersDownloadingChunk::setPeerAsFree(..) -> DownloadManager::scanTheQueue()).
   this->stop();

   for (QListIterator<QSharedPointer<ChunkDownloader>> i(this->chunkDownloaders); i.hasNext();)
   {
      auto chunkDownloader = i.next();
      if (!chunkDownloader.isNull())
         chunkDownloader->tryToRemoveItsIncompleteFile();
   }

   Download::remove();
}

/**
  * Send a request to the source peer of the download to ask it the hashes. Only sent if needed.
  * Return true if a 'GetHashes' request has been sent to the peer.
  */
bool FileDownload::retrieveHashes()
{
   // If we've already got all the chunk hashes it's unnecessary to re-ask them.
   if (
      this->nbHashesKnown == this->NB_CHUNK ||
      this->status == COMPLETE ||
      this->status == DELETED ||
      this->status == PAUSED ||
      this->status == GETTING_THE_HASHES ||
      this->status == ENTRY_NOT_FOUND
   )
      return false;

   this->getHashesResult = this->peerSource->getHashes(this->remoteEntry);

   if (this->getHashesResult.isNull())
   {
      this->setStatus(UNKNOWN_PEER_SOURCE);
      return false;
   }
   // If the peer source is already occupied, we can't ask the hashes.
   else if (!this->occupiedPeersAskingForHashes.setPeerAsOccupied(this->peerSource))
   {
      this->getHashesResult.clear();
      return false;
   }

   this->setStatus(GETTING_THE_HASHES);
   connect(this->getHashesResult.data(), &PM::IGetHashesResult::result, this, &FileDownload::result);
   connect(this->getHashesResult.data(), &PM::IGetHashesResult::nextHash, this, &FileDownload::nextHash);
   connect(this->getHashesResult.data(), &PM::IGetHashesResult::timeout, this, &FileDownload::getHashTimeout);
   this->getHashesResult->start();

   return true;
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

   if (this->nbHashesKnown == NB_CHUNK)
      newStatus = COMPLETE;

   bool hasAtLeastAPeer = false;
   for (QListIterator<QSharedPointer<ChunkDownloader>> i(this->chunkDownloaders); i.hasNext();)
   {
      auto chunkDownloader = i.next();
      if (chunkDownloader.isNull())
         continue;

      if (chunkDownloader->isDownloading())
      {
         this->setStatus(DOWNLOADING);
         return false;
      }
      else if (chunkDownloader->getLastTransferStatus() >= 0x20)
      {
         newStatus = chunkDownloader->getLastTransferStatus();
         chunkDownloader->resetLastTransferStatus();

         // If the local file disappear we reset the download.
         if (newStatus == FILE_NON_EXISTENT)
         {
            this->reset();
            this->setStatus(newStatus);
            return false;
         }
      }
      else if (!hasAtLeastAPeer && !chunkDownloader->isComplete())
      {
         if (chunkDownloader->hasAtLeastAPeer())
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
   }
}

void FileDownload::nextHash(const Protos::Core::HashResult& hashResult)
{
   if (hashResult.hash().hash().size() == 0)
   {
      L_DEBU("The received hash contains no data");
      return;
   }

   Common::Hash hash { hashResult.hash().hash() };
   quint32 num = hashResult.num();

   L_DEBU(QString("New Hash received %2 num %1").arg(num).arg(hash.toStr()));

   if (num >= static_cast<quint32>(this->chunkDownloaders.size()))
   {
      L_WARN(QString("The received hash (%1) has a number greater than expected: %2").arg(hash.toStr()).arg(num));
      return;
   }

   if (!this->chunkDownloaders[num].isNull())
   {
      if (this->chunkDownloaders[num]->getHash() != hash)
      {
         L_WARN(
            QString("The hash (%1) num %2 received doesn't match the hash (%3) in our entry").
               arg(hash.toStr()).
               arg(num).
               arg(this->chunkDownloaders[num]->getHash().toStr())
         );
      }
      return;
   }

   QSharedPointer<ChunkDownloader> chunkDownloader = (new ChunkDownloader(this->linkedPeers, this->occupiedPeersDownloadingChunk, this->transferRateCalculator, this->threadPool, hash))->grabStrongRef();
   this->chunkDownloaders[num] = chunkDownloader;

   // If the file has already been created, the chunks are known.
   auto chunk = this->chunksWithoutDownloader.take(num);
   if (!chunk.isNull())
      chunkDownloader->setChunk(chunk);

   // If we have all the chunk hashes.
   if (++this->nbHashesKnown >= this->NB_CHUNK)
   {
      this->nbHashesKnown = this->NB_CHUNK;
      this->getHashesResult.clear();
      this->occupiedPeersAskingForHashes.setPeerAsFree(this->peerSource);
      this->updateStatus();
   }

   this->connectChunkDownloaderSignals(chunkDownloader);
   chunkDownloader->setPeerSource(this->peerSource); // May start a download.

   if (num < static_cast<quint32>(this->remoteEntry.chunk_size()))
      this->remoteEntry.mutable_chunk(num)->set_hash(hash.getData(), Common::Hash::HASH_SIZE); // Used during the saving of the queue, see Download::populateEntry(..).

   emit newHashKnown();
}

void FileDownload::getHashTimeout()
{
   L_DEBU("Unable to retrieve the hashes: timeout");
   this->getHashesResult.clear();
   this->setStatus(UNABLE_TO_RETRIEVE_THE_HASHES);
   this->occupiedPeersAskingForHashes.setPeerAsFree(this->peerSource);
}

void FileDownload::chunkDownloaderStarted()
{
   this->setStatus(DOWNLOADING);
}

void FileDownload::chunkDownloaderFinished()
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

   if (this->nbHashesKnown > 0)
   {
      Common::Hashes hashes;
      for (QListIterator<QSharedPointer<ChunkDownloader>> i(this->chunkDownloaders); i.hasNext();)
      {
         auto chunkDownloader = i.next();
         if (!chunkDownloader.isNull())
            hashes << chunkDownloader->getHash();
      }

      for (QListIterator<QSharedPointer<FM::IChunk>> i(this->fileManager->getAllChunks(this->localEntry, hashes)); i.hasNext();)
      {
         auto chunk = i.next();
         this->chunksWithoutDownloader.insert(chunk->getNum(), chunk);
      }

      if (!this->chunksWithoutDownloader.isEmpty())
         this->localEntry.set_exists(true);

      this->giveChunksToDownloaders();
   }

   return this->localEntry.exists();
}

void FileDownload::connectChunkDownloaderSignals(const QSharedPointer<ChunkDownloader>& chunkDownloader)
{
   connect(chunkDownloader.data(), &ChunkDownloader::downloadStarted, this, &FileDownload::chunkDownloaderStarted, Qt::DirectConnection);
   connect(chunkDownloader.data(), &ChunkDownloader::downloadFinished, this, &FileDownload::chunkDownloaderFinished, Qt::DirectConnection);
   connect(chunkDownloader.data(), &ChunkDownloader::numberOfPeersChanged, this, &FileDownload::updateStatus, Qt::DirectConnection);
}

/**
  * Try to create the file.
  * May change the status of the download if an error occurs.
  * @return 'true' if everything fine else 'false' if an error has occurred.
  */
bool FileDownload::createFile()
{
   try
   {
      for (QListIterator<QSharedPointer<FM::IChunk>> i(this->fileManager->newFile(this->localEntry)); i.hasNext();)
      {
         auto chunk = i.next();
         this->chunksWithoutDownloader.insert(chunk->getNum(), chunk);
      }

      this->giveChunksToDownloaders();
   }
   catch (FM::NoWriteableDirectoryException&)
   {
      L_DEBU(QString("There is no shared directory with writing rights for this download : %1").arg(Common::ProtoHelper::getStr(this->remoteEntry, &Protos::Common::Entry::name)));
      this->setStatus(NO_SHARED_DIRECTORY_TO_WRITE);
      return false;
   }
   catch (FM::InsufficientStorageSpaceException&)
   {
      L_DEBU(QString("There is no enough space storage available for this download : %1").arg(Common::ProtoHelper::getStr(this->remoteEntry, &Protos::Common::Entry::name)));
      this->setStatus(NO_ENOUGH_FREE_SPACE);
      return false;
   }
   catch (FM::UnableToCreateNewFileException&)
   {
      L_DEBU(QString("Unable to create the file, download : %1").arg(Common::ProtoHelper::getStr(this->remoteEntry, &Protos::Common::Entry::name)));
      this->setStatus(UNABLE_TO_CREATE_THE_FILE);
      return false;
   }
   catch (FM::UnableToCreateNewDirException&)
   {
      L_DEBU(QString("Unable to create the path, download : %1").arg(Common::ProtoHelper::getStr(this->remoteEntry, &Protos::Common::Entry::name)));
      this->setStatus(UNABLE_TO_CREATE_THE_DIRECTORY);
      return false;
   }

   return true;
}

void FileDownload::giveChunksToDownloaders()
{
   for (QMutableMapIterator<int, QSharedPointer<FM::IChunk>> i(this->chunksWithoutDownloader); i.hasNext();)
   {
      auto chunk = i.next();
      if (chunk.key() < this->chunkDownloaders.size() && !this->chunkDownloaders[chunk.key()].isNull())
      {
         this->chunkDownloaders[chunk.key()]->setChunk(chunk.value());
         i.remove();
      }
   }
}

/**
  * Reset all download chunk and set the local file as non-existent.
  */
void FileDownload::reset()
{
   this->chunksWithoutDownloader.clear();
   for (QListIterator<QSharedPointer<ChunkDownloader>> i(this->chunkDownloaders); i.hasNext();)
      i.next()->reset();
   this->localEntry.set_exists(false);
   this->localEntry.clear_shared_dir();
}
