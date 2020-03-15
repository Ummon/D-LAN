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

#include <QStringBuilder>

#include <Protos/queue.pb.h>

#include <Common/Settings.h>
#include <Common/Constants.h>
#include <Common/ProtoHelper.h>

#include <Core/FileManager/Exceptions.h>

#include <priv/FileDownload.h>
#include <priv/DirDownload.h>
#include <priv/DownloadPredicate.h>
#include <priv/Constants.h>

LOG_INIT_CPP(DownloadManager)

DownloadManager::DownloadManager(QSharedPointer<FM::IFileManager> fileManager, QSharedPointer<PM::IPeerManager> peerManager) :
   NUMBER_OF_DOWNLOADER(static_cast<int>(SETTINGS.get<quint32>("number_of_downloader"))),
   fileManager(fileManager),
   peerManager(peerManager),
   threadPool(NUMBER_OF_DOWNLOADER),
   numberOfDownloadThreadRunning(0),
   queueChanged(false),
   queueLoaded(false)
{
   this->threadPool.setStackSize(MIN_DOWNLOAD_THREAD_STACK_SIZE + SETTINGS.get<quint32>("buffer_size_writing"));

   connect(&this->occupiedPeersAskingForHashes, &OccupiedPeers::newFreePeer, this, &DownloadManager::peerNoLongerAskingForHashes);
   connect(&this->occupiedPeersAskingForEntries, &OccupiedPeers::newFreePeer, this, &DownloadManager::peerNoLongerAskingForEntries);
   connect(&this->occupiedPeersDownloadingChunk, &OccupiedPeers::newFreePeer, this, &DownloadManager::peerNoLongerDownloadingChunk);

   // We wait the cache is loaded before loading the downloads queue.
   connect(this->fileManager.data(), &FM::IFileManager::fileCacheLoaded, this, &DownloadManager::fileCacheLoaded);

   this->startErroneousDownloadTimer.setInterval(RESTART_DOWNLOADS_PERIOD_IF_ERROR);
   this->startErroneousDownloadTimer.setSingleShot(true);
   connect(&this->startErroneousDownloadTimer, &QTimer::timeout, this, &DownloadManager::restartErroneousDownloads);

   this->saveTimer.setInterval(SETTINGS.get<quint32>("save_queue_period"));
   connect(&this->saveTimer, &QTimer::timeout, this, &DownloadManager::saveQueueToFile);

   connect(this->peerManager.data(), &PM::IPeerManager::peerBecomesAvailable, this, &DownloadManager::peerBecomesAvailable);
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
void DownloadManager::addDownload(const Protos::Common::Entry& remoteEntry, PM::IPeer* peerSource)
{
   this->addDownload(remoteEntry, peerSource, Common::Hash(), "/", Protos::Queue::Queue::Entry::QUEUED, this->downloadQueue.size());
}

void DownloadManager::addDownload(const Protos::Common::Entry& remoteEntry, PM::IPeer* peerSource, const Common::Hash& destinationDirectoryID, const QString& relativePath)
{
   this->addDownload(remoteEntry, peerSource, destinationDirectoryID, relativePath, Protos::Queue::Queue::Entry::QUEUED, this->downloadQueue.size());
}

void DownloadManager::addDownload(const Protos::Common::Entry& remoteEntry, PM::IPeer* peerSource, const QString& absolutePath)
{
   try
   {
      QPair<Common::SharedDir, QString> result = this->fileManager->addASharedDir(absolutePath);
      this->addDownload(remoteEntry, peerSource, result.first.ID, result.second, Protos::Queue::Queue::Entry::QUEUED, this->downloadQueue.size());
   }
   catch (FM::DirsNotFoundException& e)
   {
      L_WARN(QString("The following directory isn't found: %1").arg(absolutePath));
   }
   catch (FM::UnableToCreateSharedDirectory& e)
   {
      L_WARN(QString("Unable to share the following directory: %1").arg(absolutePath));
   }
}

Download* DownloadManager::addDownload(const Protos::Common::Entry& remoteEntry, PM::IPeer* peerSource, const Common::Hash& destinationDirectoryID, const QString& localRelativePath, Protos::Queue::Queue::Entry::Status status)
{
   return this->addDownload(remoteEntry, peerSource, destinationDirectoryID, localRelativePath, status, this->downloadQueue.size());
}

/**
  * Insert a new download at the given position.
  */
Download* DownloadManager::addDownload(const Protos::Common::Entry& remoteEntry, PM::IPeer* peerSource, const Common::Hash& destinationDirectoryID, const QString& localRelativePath, Protos::Queue::Queue::Entry::Status status, int position)
{
   Protos::Common::Entry localEntry(remoteEntry);

   localEntry.clear_shared_dir();
   localEntry.set_exists(false);

   Common::ProtoHelper::setStr(localEntry, &Protos::Common::Entry::set_path, localRelativePath);
   if (!destinationDirectoryID.isNull())
      localEntry.mutable_shared_dir()->mutable_id()->set_hash(destinationDirectoryID.getData(), Common::Hash::HASH_SIZE);

   return this->addDownload(remoteEntry, localEntry, peerSource, status, position);
}

Download* DownloadManager::addDownload(const Protos::Common::Entry& remoteEntry, const Protos::Common::Entry& localEntry, PM::IPeer* peerSource, Protos::Queue::Queue::Entry::Status status)
{
   return this->addDownload(remoteEntry, localEntry, peerSource, status, this->downloadQueue.size());
}

Download* DownloadManager::addDownload(const Protos::Common::Entry& remoteEntry, const Protos::Common::Entry& localEntry, PM::IPeer* peerSource, Protos::Queue::Queue::Entry::Status status, int position)
{
   Download* newDownload = nullptr;

   // We do not create a new download if a similar one is already in queue. This test can be CPU expensive.
   if (this->downloadQueue.isEntryAlreadyQueued(localEntry))
   {
      QString sharedDir = this->fileManager->getSharedDir(localEntry.shared_dir().id().hash());
      if (!sharedDir.isEmpty())
         sharedDir.remove(sharedDir.size() - 1, 1); // Remove the ending '/'.

      L_USER(tr("The file '%1' is already in queue").arg(sharedDir % Common::ProtoHelper::getRelativePath(localEntry)));
      return newDownload;
   }

   switch (remoteEntry.type())
   {
   case Protos::Common::Entry_Type_DIR:
      {
         DirDownload* dirDownload = new DirDownload(
            this->fileManager,
            this->occupiedPeersAskingForEntries,
            peerSource,
            remoteEntry,
            localEntry
         );
         newDownload = dirDownload;
         connect(dirDownload, &DirDownload::newEntries, this, &DownloadManager::newEntries, Qt::DirectConnection);
      }
      break;

   case Protos::Common::Entry_Type_FILE:
      {
         FileDownload* fileDownload = new FileDownload(
            this->fileManager,
            this->linkedPeers,
            this->occupiedPeersAskingForHashes,
            this->occupiedPeersDownloadingChunk,
            this->threadPool,
            peerSource,
            remoteEntry,
            localEntry,
            this->transferRateCalculator,
            status
         );
         newDownload = fileDownload;
         connect(fileDownload, &FileDownload::newHashKnown, this, &DownloadManager::setQueueChanged, Qt::DirectConnection);
      }
      break;

   default:
      return 0;
   }

   connect(newDownload, &Download::becomeErroneous, this, &DownloadManager::downloadStatusBecomeErroneous);
   this->downloadQueue.insert(position, newDownload);
   newDownload->start();

   this->setQueueChanged();

   return newDownload;
}

QList<IDownload*> DownloadManager::getDownloads() const
{
   QList<IDownload*> listDownloads;

   // A bit heavy . . .
   listDownloads.reserve(this->downloadQueue.size());
   for (int i = 0; i < this->downloadQueue.size(); i++)
   {
      Download* download = this->downloadQueue[i];
      if (download->getStatus() != DELETED)
         listDownloads << download;
   }

   return listDownloads;
}

void DownloadManager::moveDownloads(const QList<quint64>& downloadIDRefs, const QList<quint64>& downloadIDs, Protos::GUI::MoveDownloads::Position position)
{
   this->downloadQueue.moveDownloads(downloadIDRefs, downloadIDs, position);

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

void DownloadManager::pauseDownloads(QList<quint64> IDs, bool pause)
{
   if (IDs.isEmpty())
      return;

   if (this->downloadQueue.pauseDownloads(IDs, pause))
      this->setQueueChanged();

   if (!pause)
      this->scanTheQueue();
}

QList<QSharedPointer<IChunkDownloader>> DownloadManager::getTheFirstUnfinishedChunks(int n)
{
   QList<QSharedPointer<IChunkDownloader>> unfinishedChunks;

   DownloadQueue::ScanningIterator<IsDownloable> i(this->downloadQueue);
   FileDownload* fileDownload;
   while (unfinishedChunks.size() < n && (fileDownload = static_cast<FileDownload*>(i.next())))
   {
      fileDownload->getUnfinishedChunks(unfinishedChunks, n - unfinishedChunks.size(), false); // 'false' because we always want the first (unfinished) chunks.
   }

   return unfinishedChunks;
}

QList<QSharedPointer<IChunkDownloader>> DownloadManager::getTheOldestUnfinishedChunks(int n)
{
   return this->downloadQueue.getTheOldestUnfinishedChunks(n);
}

int DownloadManager::getDownloadRate()
{
   return this->transferRateCalculator.getTransferRate();
}

void DownloadManager::peerBecomesAvailable(PM::IPeer* peer)
{     
   this->downloadQueue.peerBecomesAvailable(peer);

   // To handle the case where the peers source of some downloads without all the hashes become alive after being dead for a while. The hashes must be reasked.
   this->occupiedPeersAskingForEntries.newPeer(peer);
   this->occupiedPeersAskingForHashes.newPeer(peer);
   this->occupiedPeersDownloadingChunk.newPeer(peer);
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
   DirDownload* dirDownload = static_cast<DirDownload*>(this->sender());
   int position = this->downloadQueue.find(dirDownload);
   if (position == -1)
      return;
   this->downloadQueue.remove(position);

   const Protos::Common::Entry& localEntry = dirDownload->getLocalEntry();
   const QString relativePath = Common::ProtoHelper::getStr(localEntry, &Protos::Common::Entry::path).append(Common::ProtoHelper::getStr(localEntry, &Protos::Common::Entry::name)).append("/");

   // Add files first then directories.
   for (auto type : QList<Protos::Common::Entry::Type> { Protos::Common::Entry::FILE, Protos::Common::Entry::DIR })
   {
      for (int n = 0; n < remoteEntries.entry_size(); n++)
         if (remoteEntries.entry(n).type() == type)
            this->addDownload(remoteEntries.entry(n), dirDownload->getPeerSource(), localEntry.has_shared_dir() ? localEntry.shared_dir().id().hash() : Common::Hash(), relativePath, Protos::Queue::Queue::Entry::QUEUED, position++);
   }

   delete dirDownload;
}

/**
  * Search for a new file to asking hashes.
  */
void DownloadManager::peerNoLongerAskingForHashes(PM::IPeer* peer)
{
   L_DEBU(QString("A peer is free from asking for hashes: %1").arg(peer->toStringLog()));

   if (!this->downloadQueue.isAPeerSource(peer))
      return;

   // We can't use 'downloadsIndexedBySourcePeerID' because the order matters.
   DownloadQueue::ScanningIterator<IsDownloable> i(this->downloadQueue);
   while (FileDownload* fileDownload = static_cast<FileDownload*>(i.next()))
      if (!fileDownload->isStatusErroneous() && fileDownload->retrieveHashes())
         break;
}

/**
  * Search a directory to explore in the download queue.
  */
void DownloadManager::peerNoLongerAskingForEntries(PM::IPeer* peer)
{
   L_DEBU(QString("A peer is free from asking for entries: %1").arg(peer->toStringLog()));

   if (!this->downloadQueue.isAPeerSource(peer))
      return;

   DownloadQueue::ScanningIterator<IsADirectory> i(this->downloadQueue);
   while (DirDownload* dirDownload = static_cast<DirDownload*>(i.next()))
      if (!dirDownload->isStatusErroneous() && dirDownload->retrieveEntries())
         break;
}

void DownloadManager::peerNoLongerDownloadingChunk(PM::IPeer* peer)
{
   L_DEBU(QString("A peer is free from downloading: %1, number of downloading thread : %2").arg(peer->toStringLog()).arg(this->numberOfDownloadThreadRunning));
   this->scanTheQueue();
}

/**
  * Search a chunk to download.
  */
void DownloadManager::scanTheQueue()
{
   L_DEBU("Scanning the queue . . .");

   int numberOfDownloadThreadRunningCopy = this->numberOfDownloadThreadRunning;

   QSharedPointer<ChunkDownloader> chunkDownloader;
   FileDownload* fileDownload = nullptr;

   // To know the number of peers not occupied that own at least one chunk in the queue.
   auto peers = this->linkedPeers.getPeers();
   QSet<PM::IPeer*> linkedPeersNotOccupied(peers.begin(), peers.end());
   linkedPeersNotOccupied -= this->occupiedPeersDownloadingChunk.getOccupiedPeers();

   DownloadQueue::ScanningIterator<IsDownloable> i(this->downloadQueue);

   while (numberOfDownloadThreadRunningCopy < NUMBER_OF_DOWNLOADER && !linkedPeersNotOccupied.isEmpty())
   {
      if (chunkDownloader.isNull()) // We can ask many chunks to download from the same file.
         if (!(fileDownload = static_cast<FileDownload*>(i.next())))
             break;

      if (fileDownload->isStatusErroneous())
         continue;

      chunkDownloader = fileDownload->getAChunkToDownload();

      if (chunkDownloader.isNull())
         continue;

      if (PM::IPeer* currentPeer = chunkDownloader->startDownloading())
      {
         connect(chunkDownloader.data(), &ChunkDownloader::downloadFinished, this, &DownloadManager::chunkDownloaderFinished, Qt::DirectConnection);
         linkedPeersNotOccupied -= currentPeer;
         this->numberOfDownloadThreadRunning++;
         numberOfDownloadThreadRunningCopy = this->numberOfDownloadThreadRunning;
      }
   }

   L_DEBU("Scanning terminated");
}

/**
  * Restart the first erroneous download.
  */
void DownloadManager::restartErroneousDownloads()
{
   while (Download* download = this->downloadQueue.getAnErroneousDownload())
   {
      if (download->isStatusErroneous())
      {
         download->start(); // We restart the download.
         L_DEBU(QString("Rescan timer timedout, the queue will be rescanned. File restarted: %1").arg(Common::ProtoHelper::getRelativePath(download->getLocalEntry())));
         this->startErroneousDownloadTimer.start();
         break;
      }
   }
}

/**
  * It must be called before 'peerNoLongerDownloadingChunk' when a download is finished.
  */
void DownloadManager::chunkDownloaderFinished()
{
   L_DEBU(QString("DownloadManager::chunkDownloaderFinished, numberOfDownloadThreadRunning = %1").arg(this->numberOfDownloadThreadRunning));
   this->sender()->disconnect(this, SLOT(chunkDownloaderFinished()));
   this->numberOfDownloadThreadRunning--;
}

/**
  * When a download status become erroneous a timer is activated. This will check
  * the erroneous downloads periodically.
  */
void DownloadManager::downloadStatusBecomeErroneous(Download* download)
{
   this->downloadQueue.setDownloadAsErroneous(download);

   if (!this->startErroneousDownloadTimer.isActive())
      this->startErroneousDownloadTimer.start();
}

/**
  * Load the queue, called once at the begining of the program.
  * Will start the timer to save periodically the queue.
  */
void DownloadManager::loadQueueFromFile()
{
   Protos::Queue::Queue savedQueue = DownloadQueue::loadFromFile();

   for (int i = 0; i < savedQueue.entry_size(); i++)
   {
      const Protos::Queue::Queue_Entry& entry = savedQueue.entry(i);
      this->addDownload(
         entry.remote_entry(),
         entry.local_entry(),
         this->peerManager->createPeer(entry.peer_source_id().hash(), Common::ProtoHelper::getStr(entry, &Protos::Queue::Queue::Entry::peer_source_nick)),
         entry.status()
      );
   }

   this->queueLoaded = true;
   this->saveTimer.start();
}

void DownloadManager::saveQueueToFile()
{
   if (this->queueChanged && this->queueLoaded)
   {
      L_DEBU("Persisting queue . . .");

      this->downloadQueue.saveToFile();
      this->queueChanged = false;

      L_DEBU("Persisting queue finished");
   }
}

/**
  * Called each time the queue is modified.
  */
void DownloadManager::setQueueChanged()
{
   this->queueChanged = true;
}

const quint32 DownloadManager::MIN_DOWNLOAD_THREAD_STACK_SIZE(64 * 1024);
