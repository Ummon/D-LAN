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
  
#include <priv/DownloadManager.h>
using namespace DM;

#include <Protos/queue.pb.h>

#include <Common/Settings.h>
#include <Common/Constants.h>
#include <Common/ProtoHelper.h>

#include <Core/FileManager/Exceptions.h>

#include <priv/FileDownload.h>
#include <priv/DirDownload.h>
#include <priv/DownloadPredicate.h>
#include <priv/Constants.h>

LOG_INIT_CPP(DownloadManager);

DownloadManager::DownloadManager(QSharedPointer<FM::IFileManager> fileManager, QSharedPointer<PM::IPeerManager> peerManager) :
   NUMBER_OF_DOWNLOADER(static_cast<int>(SETTINGS.get<quint32>("number_of_downloader"))),
   fileManager(fileManager),
   peerManager(peerManager),
   threadPool(NUMBER_OF_DOWNLOADER),
   numberOfDownloadThreadRunning(0),
   queueChanged(false)
{
   connect(&this->occupiedPeersAskingForHashes, SIGNAL(newFreePeer(PM::IPeer*)), this, SLOT(peerNoLongerAskingForHashes(PM::IPeer*)));
   connect(&this->occupiedPeersAskingForEntries, SIGNAL(newFreePeer(PM::IPeer*)), this, SLOT(peerNoLongerAskingForEntries(PM::IPeer*)));
   connect(&this->occupiedPeersDownloadingChunk, SIGNAL(newFreePeer(PM::IPeer*)), this, SLOT(peerNoLongerDownloadingChunk(PM::IPeer*)));

   connect(this->fileManager.data(), SIGNAL(fileCacheLoaded()), this, SLOT(fileCacheLoaded()));

   this->rescanTimer.setInterval(RESCAN_QUEUE_PERIOD_IF_ERROR);
   this->rescanTimer.setSingleShot(true);
   connect(&this->rescanTimer, SIGNAL(timeout()), this, SLOT(scanTheQueue()));

   this->saveTimer.setInterval(SETTINGS.get<quint32>("save_queue_period"));
   connect(&this->saveTimer, SIGNAL(timeout()), this, SLOT(saveQueueToFile()));

   connect(this->peerManager.data(), SIGNAL(peerBecomesAvailable(PM::IPeer*)), this, SLOT(peerBecomesAvailable(PM::IPeer*)));
}

DownloadManager::~DownloadManager()
{
   this->queueChanged = true;
   this->saveQueueToFile();

   L_DEBU("DownloadManager deleted");
}

/**
  * Insert a new download at the end of the queue.
  */
void DownloadManager::addDownload(const Protos::Common::Entry& remoteEntry, const Common::Hash& peerSource)
{
   this->addDownload(remoteEntry, peerSource, Common::Hash(), "/", false, this->downloadQueue.size());
}

void DownloadManager::addDownload(const Protos::Common::Entry& remoteEntry, const Common::Hash& peerSource, const Common::Hash& destinationDirectoryID, const QString& relativePath)
{
   this->addDownload(remoteEntry, peerSource, destinationDirectoryID, relativePath, false, this->downloadQueue.size());
}

void DownloadManager::addDownload(const Protos::Common::Entry& remoteEntry, const Common::Hash& peerSource, const QString& absolutePath)
{
   try
   {
      QPair<Common::SharedDir, QString> result = this->fileManager->addASharedDir(absolutePath);
      this->addDownload(remoteEntry, peerSource, result.first.ID, result.second, false, this->downloadQueue.size());
   }
   catch(FM::DirsNotFoundException& e)
   {
      L_WARN(QString("The following directory isn't found: %1").arg(absolutePath));
   }
   catch(FM::UnableToCreateSharedDirectory& e)
   {
      L_WARN(QString("Unable to share dthe following directory: %1").arg(absolutePath));
   }
}

Download* DownloadManager::addDownload(const Protos::Common::Entry& remoteEntry, const Common::Hash& peerSource, const Common::Hash& destinationDirectoryID, const QString& localRelativePath, bool complete)
{
   return this->addDownload(remoteEntry, peerSource, destinationDirectoryID, localRelativePath, complete, this->downloadQueue.size());
}

/**
  * Insert a new download at the given position.
  */
Download* DownloadManager::addDownload(const Protos::Common::Entry& remoteEntry, const Common::Hash& peerSource, const Common::Hash& destinationDirectoryID, const QString& localRelativePath, bool complete, int position)
{
   Protos::Common::Entry localEntry(remoteEntry);
   Common::ProtoHelper::setStr(localEntry, &Protos::Common::Entry::set_path, localRelativePath);
   if (!destinationDirectoryID.isNull())
      localEntry.mutable_shared_dir()->mutable_id()->set_hash(destinationDirectoryID.getData(), Common::Hash::HASH_SIZE);
   localEntry.set_exists(false);

   return this->addDownload(remoteEntry, localEntry, peerSource, complete, position);
}

Download* DownloadManager::addDownload(const Protos::Common::Entry& remoteEntry, const Protos::Common::Entry& localEntry, const Common::Hash& peerSource, bool complete)
{
   return this->addDownload(remoteEntry, localEntry, peerSource, complete, this->downloadQueue.size());
}

Download* DownloadManager::addDownload(const Protos::Common::Entry& remoteEntry, const Protos::Common::Entry& localEntry, const Common::Hash& peerSource, bool complete, int position)
{
   Download* newDownload = 0;

   switch (remoteEntry.type())
   {
   case Protos::Common::Entry_Type_DIR :
      {
         DirDownload* dirDownload = new DirDownload(
            this->occupiedPeersAskingForEntries,
            peerSource,
            remoteEntry,
            localEntry
         );
         newDownload = dirDownload;
         connect(dirDownload, SIGNAL(newEntries(const Protos::Common::Entries&)), this, SLOT(newEntries(const Protos::Common::Entries&)), Qt::DirectConnection);
      }
      break;

   case Protos::Common::Entry_Type_FILE :
      {
         FileDownload* fileDownload = new FileDownload(
            this->fileManager,
            this->peerManager,
            this->occupiedPeersAskingForHashes,
            this->occupiedPeersDownloadingChunk,
            this->threadPool,
            peerSource,
            remoteEntry,
            localEntry,
            this->transferRateCalculator,
            complete
         );
         newDownload = fileDownload;
         connect(fileDownload, SIGNAL(newHashKnown()), this, SLOT(setQueueChanged()), Qt::DirectConnection);
      }
      break;

   default:
      return 0;
   }

   this->downloadQueue.insert(position, newDownload);
   newDownload->setPeer(this->peerManager->getPeer(peerSource));
   newDownload->start();

   this->setQueueChanged();

   return newDownload;
}

QList<IDownload*> DownloadManager::getDownloads() const
{
   QList<IDownload*> listDownloads;

   // TODO: very heavy!
   for (int i = 0; i < this->downloadQueue.size(); i++)
   {
      Download* download = this->downloadQueue[i];
      if (download->getStatus() != DELETED)
         listDownloads << download;
   }

   return listDownloads;
}

void DownloadManager::moveDownloads(quint64 downloadIDRef, bool moveBefore, const QList<quint64>& downloadIDs)
{
   this->downloadQueue.moveDownloads(downloadIDRef, moveBefore, downloadIDs);

   this->setQueueChanged();
}

/**
  * Use the method "QList::erase(..)" to remove many downloads in one call. The goal is to be more efficient than using only 'Download::remove()'.
  */
void DownloadManager::removeAllCompleteDownloads()
{
   IsComplete isComplete;
   if (this->downloadQueue.removeDownloads(isComplete))
      this->setQueueChanged();
}

void DownloadManager::removeDownloads(QList<quint64> IDs)
{
   if (IDs.isEmpty())
      return;

   IsContainedInAList isContainedInAList(IDs);
   if (this->downloadQueue.removeDownloads(isContainedInAList))
      this->setQueueChanged();
}

QList< QSharedPointer<IChunkDownload> > DownloadManager::getUnfinishedChunks(int n) const
{
   QList< QSharedPointer<IChunkDownload> > unfinishedChunks;

   for (int i = 0; i < this->downloadQueue.size(); i++)
   {
      FileDownload* fileDownload = dynamic_cast<FileDownload*>(this->downloadQueue[i]);
      if (!fileDownload)
         continue;

      fileDownload->getUnfinishedChunks(unfinishedChunks, n - unfinishedChunks.size());
   }

   return unfinishedChunks;
}

int DownloadManager::getDownloadRate()
{
   return this->transferRateCalculator.getTransferRate();
}

void DownloadManager::peerBecomesAvailable(PM::IPeer* peer)
{
   this->downloadQueue.setPeerSource(peer);

   // To handle the case where the peers source of some downloads without all the hashes become alive after being dead for a while. The hashes must be reasked.
   this->occupiedPeersAskingForEntries.newPeer(peer);
   this->occupiedPeersAskingForHashes.newPeer(peer);
}

void DownloadManager::fileCacheLoaded()
{
   this->loadQueueFromFile();
}

/**
  * Called when a directory knows its children. The children replace the directory.
  * The directory is removed from the queue and deleted.
  */
void DownloadManager::newEntries(const Protos::Common::Entries& remoteEntries)
{
   DirDownload* dirDownload = dynamic_cast<DirDownload*>(this->sender());
   int position = this->downloadQueue.find(dirDownload);
   if (position == -1)
      return;
   this->downloadQueue.remove(position);

   // Add files first.
   for (int n = 0; n < remoteEntries.entry_size(); n++)
      if (remoteEntries.entry(n).type() == Protos::Common::Entry_Type_FILE)
      {
         const Protos::Common::Entry& localEntry = dirDownload->getLocalEntry();
         QString relativePath = Common::ProtoHelper::getStr(localEntry, &Protos::Common::Entry::path).append(Common::ProtoHelper::getStr(localEntry, &Protos::Common::Entry::name)).append("/");
         this->addDownload(remoteEntries.entry(n), dirDownload->getPeerSourceID(), localEntry.has_shared_dir() ? localEntry.shared_dir().id().hash() : Common::Hash(), relativePath, false, position++);
      }

   // Then directories. TODO : code to refactor with the one above.
   for (int n = 0; n < remoteEntries.entry_size(); n++)
      if (remoteEntries.entry(n).type() == Protos::Common::Entry_Type_DIR)
      {
         const Protos::Common::Entry& localEntry = dirDownload->getLocalEntry();
         QString relativePath = Common::ProtoHelper::getStr(localEntry, &Protos::Common::Entry::path).append(Common::ProtoHelper::getStr(localEntry, &Protos::Common::Entry::name)).append("/");
         this->addDownload(remoteEntries.entry(n), dirDownload->getPeerSourceID(), localEntry.has_shared_dir() ? localEntry.shared_dir().id().hash() : Common::Hash(), relativePath, false, position++);
      }

   delete dirDownload;
}

/**
  * Search for a new file to asking hashes.
  */
void DownloadManager::peerNoLongerAskingForHashes(PM::IPeer* peer)
{
   L_DEBU(QString("Finish to ask hashes from peer: %1").arg(peer->getID().toStr()));

   if (!this->downloadQueue.isAPeerSource(peer->getID()))
      return;

   // We can't use 'downloadsIndexedBySourcePeerID' because the order matters.
   for (int i = 0; i < this->downloadQueue.size(); i++)
   {
      FileDownload* fileDownload = dynamic_cast<FileDownload*>(this->downloadQueue[i]);
      if (fileDownload && fileDownload->retrieveHashes())
         break;
   }
}

void DownloadManager::peerNoLongerAskingForEntries(PM::IPeer* peer)
{
   L_DEBU(QString("Finish to ask entries from peer: %1").arg(peer->getID().toStr()));

   if (!this->downloadQueue.isAPeerSource(peer->getID()))
      return;

   DownloadQueue::ScanningIterator<IsADirectory> i(this->downloadQueue);
   while (DirDownload* dirDownload = static_cast<DirDownload*>(i.next()))
      if (dirDownload->retrieveEntries())
         break;
}

void DownloadManager::peerNoLongerDownloadingChunk(PM::IPeer* peer)
{
   L_DEBU(QString("A peer is free: %1, number of downloading thread : %2").arg(peer->getID().toStr()).arg(this->numberOfDownloadThreadRunning));
   this->scanTheQueue();
}

/**
  * Search a chunk to download.
  */
void DownloadManager::scanTheQueue()
{
   L_DEBU("Scanning the queue..");

   int numberOfDownloadThreadRunningCopy = this->numberOfDownloadThreadRunning;

   QSharedPointer<ChunkDownload> chunkDownload;
   FileDownload* fileDownload = 0;
   DownloadQueue::ScanningIterator<IsDownloable> i(this->downloadQueue);

   const int nbPeers = this->peerManager->getPeers().size();
   while (numberOfDownloadThreadRunningCopy < NUMBER_OF_DOWNLOADER && nbPeers > this->occupiedPeersDownloadingChunk.nbOccupiedPeers())
   {
      if (chunkDownload.isNull()) // We can ask many chunks to download from the same file.
         if (!(fileDownload = static_cast<FileDownload*>(i.next())))
             break;

      fileDownload->updateStatus(); // Reset status.

      chunkDownload = fileDownload->getAChunkToDownload();

      if (fileDownload->isStatusErroneous())
      {
         this->rescanTimer.start();
         chunkDownload.clear();
         continue;
      }

      if (chunkDownload.isNull())
         continue;

      connect(chunkDownload.data(), SIGNAL(downloadFinished()), this, SLOT(chunkDownloadFinished()), Qt::DirectConnection);

      if (chunkDownload->startDownloading())
      {
         this->numberOfDownloadThreadRunning++;
         numberOfDownloadThreadRunningCopy = this->numberOfDownloadThreadRunning;
      }
   }

   L_DEBU("Scanning terminated");
}

/**
  * It must be called before 'peerNoLongerDownloadingChunk' when a download is finished.
  */
void DownloadManager::chunkDownloadFinished()
{
   L_DEBU(QString("DownloadManager::chunkDownloadFinished, numberOfDownloadThreadRunning = %1").arg(this->numberOfDownloadThreadRunning));
   this->sender()->disconnect(this, SLOT(chunkDownloadFinished()));
   this->numberOfDownloadThreadRunning--;
}

/**
  * Load the queue, called once at the begining of the program.
  * Will start the timer to save priodically the queue.
  */
void DownloadManager::loadQueueFromFile()
{
   Protos::Queue::Queue savedQueue = DownloadQueue::loadFromFile();

   for (int i = 0; i < savedQueue.entry_size(); i++)
   {
      const Protos::Queue::Queue_Entry& entry = savedQueue.entry(i);
      this->addDownload(entry.remote_entry(), entry.local_entry(), entry.peer_source_id().hash(), entry.complete());
   }

   this->saveTimer.start();
}

void DownloadManager::saveQueueToFile()
{
   if (this->queueChanged)
   {
      L_DEBU("Persisting queue ..");

      this->downloadQueue.saveToFile();
      this->queueChanged = false;

      L_DEBU("Persisting queue finished");
   }
}

/**
  * Called each time the queue is modified.
  * It may persist the queue.
  */
void DownloadManager::setQueueChanged()
{
   this->queueChanged = true;
}
