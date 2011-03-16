/**
  * D-LAN - A decentralized LAN file sharing software.
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
  
#include <priv/DownloadManager.h>
using namespace DM;

#include <Protos/queue.pb.h>

#include <Common/Settings.h>
#include <Common/PersistentData.h>
#include <Common/Constants.h>
#include <Common/ProtoHelper.h>

#include <priv/FileDownload.h>
#include <priv/DirDownload.h>
#include <priv/Constants.h>

LOG_INIT_CPP(DownloadManager);

DownloadManager::DownloadManager(QSharedPointer<FM::IFileManager> fileManager, QSharedPointer<PM::IPeerManager> peerManager) :
   NUMBER_OF_DOWNLOADER(static_cast<int>(SETTINGS.get<quint32>("number_of_downloader"))),
   fileManager(fileManager),
   peerManager(peerManager),
   numberOfDownload(0),
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

   connect(this->peerManager.data(), SIGNAL(peerBecomesAlive(PM::IPeer*)), this, SLOT(peerBecomesAlive(PM::IPeer*)));
}

DownloadManager::~DownloadManager()
{
   this->queueChanged = true;
   this->saveQueueToFile();

   for (QListIterator<Download*> i(this->downloads); i.hasNext();)
   {
      Download* download = i.next();
      disconnect(download, SIGNAL(deleted(Download*)), this, SLOT(downloadDeleted(Download*)));
      delete download;
   }

   L_DEBU("DownloadManager deleted");
}

/**
  * Insert a new download at the end of the queue.
  */
void DownloadManager::addDownload(const Protos::Common::Entry& remoteEntry, const Common::Hash& peerSource)
{
   QMutableListIterator<Download*> i(this->downloads);
   i.toBack();
   this->addDownload(remoteEntry, peerSource, Common::Hash(), "/", false, i);
}

void DownloadManager::addDownload(const Protos::Common::Entry& remoteEntry, const Common::Hash& peerSource, const Common::Hash& destinationDirectoryID, const QString& relativePath)
{
   QMutableListIterator<Download*> i(this->downloads);
   i.toBack();
   this->addDownload(remoteEntry, peerSource, destinationDirectoryID, relativePath, false, i);
}

Download* DownloadManager::addDownload(const Protos::Common::Entry& remoteEntry, const Common::Hash& peerSource, const Common::Hash& destinationDirectoryID, const QString& localRelativePath, bool complete)
{
   QMutableListIterator<Download*> i(this->downloads);
   i.toBack();
   return this->addDownload(remoteEntry, peerSource, destinationDirectoryID, localRelativePath, complete, i);
}

/**
  * Insert a new download at the given position.
  */
Download* DownloadManager::addDownload(const Protos::Common::Entry& remoteEntry, const Common::Hash& peerSource, const Common::Hash& destinationDirectoryID, const QString& localRelativePath, bool complete, QMutableListIterator<Download*>& iterator)
{
   Protos::Common::Entry localEntry(remoteEntry);
   Common::ProtoHelper::setStr(localEntry, &Protos::Common::Entry::set_path, localRelativePath);
   if (!destinationDirectoryID.isNull())
      localEntry.mutable_shared_dir()->mutable_id()->set_hash(destinationDirectoryID.getData(), Common::Hash::HASH_SIZE);
   localEntry.set_exists(false);

   return this->addDownload(remoteEntry, localEntry, peerSource, complete, iterator);
}

Download* DownloadManager::addDownload(const Protos::Common::Entry& remoteEntry, const Protos::Common::Entry& localEntry, const Common::Hash& peerSource, bool complete)
{
   QMutableListIterator<Download*> i(this->downloads);
   i.toBack();
   return this->addDownload(remoteEntry, localEntry, peerSource, complete, i);
}

Download* DownloadManager::addDownload(const Protos::Common::Entry& remoteEntry, const Protos::Common::Entry& localEntry, const Common::Hash& peerSource, bool complete,  QMutableListIterator<Download*>& iterator)
{
   if (this->isEntryAlreadyQueued(localEntry, peerSource))
   {
      L_WARN(QString("Entry already queued, it will no be added to the queue : %1").arg(Common::ProtoHelper::getStr(remoteEntry, &Protos::Common::Entry::name)));
      return 0;
   }

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

   iterator.insert(newDownload);
   this->downloadsIndexedBySourcePeerID.insert(peerSource, newDownload);

   newDownload->setPeer(this->peerManager->getPeer(peerSource));

   connect(newDownload, SIGNAL(deleted(Download*)), this, SLOT(downloadDeleted(Download*)), Qt::DirectConnection);

   newDownload->start();

   this->setQueueChanged();

   return newDownload;
}

QList<IDownload*> DownloadManager::getDownloads() const
{
   QList<IDownload*> listDownloads;

   // TODO : very heavy!
   for (QListIterator<Download*> i(this->downloads); i.hasNext();)
   {
      Download* download = i.next();
      if (download->getStatus() != DELETED)
         listDownloads << download;
   }

   return listDownloads;
}

void DownloadManager::moveDownloads(quint64 downloadIDRef, bool moveBefore, const QList<quint64>& downloadIDs)
{
   QList<quint64> downloadIDsCopy(downloadIDs);
   int iRef = -1; // Index of the download reference, -1 if unkown.
   QList<int> iToMove;

   for (int i = 0; i < this->downloads.size(); i++)
   {
      int j;
      if ((j = downloadIDsCopy.indexOf(this->downloads[i]->getID())) != -1)
      {
         if (iRef != -1)
         {
            this->downloads.insert(moveBefore ? iRef++ : ++iRef, this->downloads[i]);
            this->downloads.removeAt(i+1);
            continue;
         }
         else
            iToMove << i;

         downloadIDsCopy.removeAt(j);
      }

      if (this->downloads[i]->getID() == downloadIDRef)
      {
         iRef = i;
         int shift = 0;
         for (int j = 0; j < iToMove.size(); j++)
         {
            if (iToMove[j] == iRef && moveBefore)
               iRef++;
            else
            {
               this->downloads.insert(j == 0 && !moveBefore ? ++iRef : iRef, this->downloads[iToMove[j] + shift]);
               this->downloads.removeAt(iToMove[j] + shift);
               shift--;
            }
         }
         iToMove.clear();
      }
   }

   this->setQueueChanged();
}

QList< QSharedPointer<IChunkDownload> > DownloadManager::getUnfinishedChunks(int n) const
{
   QList< QSharedPointer<IChunkDownload> > unfinishedChunks;

   for (QListIterator<Download*> i(this->downloads); i.hasNext() && unfinishedChunks.size() < n;)
   {
      FileDownload* fileDownload = dynamic_cast<FileDownload*>(i.next());
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

void DownloadManager::peerBecomesAlive(PM::IPeer* peer)
{
   for (QMultiHash<Common::Hash, Download*>::iterator i = this->downloadsIndexedBySourcePeerID.find(peer->getID()); i != this->downloadsIndexedBySourcePeerID.end() && i.key() == peer->getID(); i++)
      i.value()->setPeer(peer);

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
   QMutableListIterator<Download*> i(this->downloads);
   if (!i.findNext(dirDownload))
      return;
   i.remove();

   // Add files first.
   for (int n = 0; n < remoteEntries.entry_size(); n++)
      if (remoteEntries.entry(n).type() == Protos::Common::Entry_Type_FILE)
      {
         const Protos::Common::Entry& localEntry = dirDownload->getLocalEntry();
         QString relativePath = Common::ProtoHelper::getStr(localEntry, &Protos::Common::Entry::path).append(Common::ProtoHelper::getStr(localEntry, &Protos::Common::Entry::name)).append("/");
         this->addDownload(remoteEntries.entry(n), dirDownload->getPeerSourceID(), localEntry.has_shared_dir() ? localEntry.shared_dir().id().hash().data() : Common::Hash(), relativePath, false, i);
      }

   // Then directories. TODO : code to refactor with the one above.
   for (int n = 0; n < remoteEntries.entry_size(); n++)
      if (remoteEntries.entry(n).type() == Protos::Common::Entry_Type_DIR)
      {
         const Protos::Common::Entry& localEntry = dirDownload->getLocalEntry();
         QString relativePath = Common::ProtoHelper::getStr(localEntry, &Protos::Common::Entry::path).append(Common::ProtoHelper::getStr(localEntry, &Protos::Common::Entry::name)).append("/");
         this->addDownload(remoteEntries.entry(n), dirDownload->getPeerSourceID(), localEntry.has_shared_dir() ? localEntry.shared_dir().id().hash().data() : Common::Hash(), relativePath, false, i);
      }

   delete dirDownload;
}

void DownloadManager::downloadDeleted(Download* download)
{
   this->downloads.removeOne(download);
   this->downloadsIndexedBySourcePeerID.remove(download->getPeerSourceID(), download);
   this->setQueueChanged();
}

/**
  * Search for a new file to asking hashes.
  */
void DownloadManager::peerNoLongerAskingForHashes(PM::IPeer* peer)
{
   L_DEBU(QString("Finish to ask hashes from peer: %1").arg(peer->getID().toStr()));

   if (!this->downloadsIndexedBySourcePeerID.contains(peer->getID()))
      return;

   // We can't use 'downloadsIndexedBySourcePeerID' because the order matters.
   for (QListIterator<Download*> i(this->downloads); i.hasNext();)
   {
      FileDownload* fileDownload = dynamic_cast<FileDownload*>(i.next());
      if (fileDownload && fileDownload->retrieveHashes())
         break;
   }
}

void DownloadManager::peerNoLongerAskingForEntries(PM::IPeer* peer)
{
   L_DEBU(QString("Finish to ask entries from peer: %1").arg(peer->getID().toStr()));

   if (!this->downloadsIndexedBySourcePeerID.contains(peer->getID()))
      return;

   // We can't use 'downloadsIndexedBySourcePeerID' because the order matters.
   for (QListIterator<Download*> i(this->downloads); i.hasNext();)
   {
      DirDownload* dirDownload = dynamic_cast<DirDownload*>(i.next());
      if (dirDownload && dirDownload->retrieveEntries())
         break;
   }
}

/**
  * Search a chunk to download.
  */
void DownloadManager::peerNoLongerDownloadingChunk(PM::IPeer* peer)
{
   L_DEBU(QString("A peer is free: %1, number of downloading thread : %2").arg(peer->getID().toStr()).arg(this->numberOfDownload));
   this->scanTheQueue();
}

void DownloadManager::scanTheQueue()
{
   L_DEBU("Scanning the queue..");

   int numberOfDownloadCopy = this->numberOfDownload;

   QSharedPointer<ChunkDownload> chunkDownload;
   FileDownload* fileDownload = 0;

   for (QListIterator<Download*> i(this->downloads); i.hasNext() && numberOfDownloadCopy < NUMBER_OF_DOWNLOADER;)
   {
      if (chunkDownload.isNull()) // We can ask many chunks to download from the same file.
      {
         if (!(fileDownload = dynamic_cast<FileDownload*>(i.next())))
            continue;
      }

      if (fileDownload->isStatusErroneous())
      {
         this->rescanTimer.start();
         continue;
      }

      chunkDownload = fileDownload->getAChunkToDownload();

      if (chunkDownload.isNull())
         continue;

      connect(chunkDownload.data(), SIGNAL(downloadFinished()), this, SLOT(chunkDownloadFinished()), Qt::DirectConnection);

      if (chunkDownload->startDownloading())
      {
         this->numberOfDownload++;
         numberOfDownloadCopy = this->numberOfDownload;
      }
   }

   L_DEBU("Scanning terminated");
}

/**
  * It must be called before 'peerNoLongerDownloadingChunk' when a download is finished.
  */
void DownloadManager::chunkDownloadFinished()
{
   L_DEBU(QString("DownloadManager::chunkDownloadFinished, numberOfDownload = %1").arg(this->numberOfDownload));
   this->sender()->disconnect(this, SLOT(chunkDownloadFinished()));
   this->numberOfDownload--;
}

/**
  * Load the queue, called once at the begining of the program.
  * Will start the timer to save priodically the queue.
  */
void DownloadManager::loadQueueFromFile()
{
   Protos::Queue::Queue savedQueue;

   try
   {
      Common::PersistentData::getValue(Common::FILE_QUEUE, savedQueue, Common::Global::LOCAL);
      if (static_cast<int>(savedQueue.version()) != FILE_QUEUE_VERSION)
      {
         L_USER(QString("The version (%1) of the queue file \"%2\" doesn't match the current version (%3). Queue will be reset.").arg(savedQueue.version()).arg(Common::FILE_QUEUE).arg(FILE_QUEUE_VERSION));
         Common::PersistentData::rmValue(Common::FILE_QUEUE, Common::Global::LOCAL);
         goto end;
      }

      for (int i = 0; i < savedQueue.entry_size(); i++)
      {
         const Protos::Queue::Queue_Entry& entry = savedQueue.entry(i);
         this->addDownload(entry.remote_entry(), entry.local_entry(), Common::Hash(entry.peer_id().hash().data()), entry.complete());
      }
   }
   catch (Common::UnknownValueException& e)
   {
      L_WARN(QString("The download queue file cache cannot be retrived (the file doesn't exist) : %1").arg(Common::FILE_QUEUE));
   }
   catch (...)
   {
      L_WARN(QString("The download queue file cache cannot be retrived (Unkown exception) : %1").arg(Common::FILE_QUEUE));
   }

end:
   this->saveTimer.start();
}

void DownloadManager::saveQueueToFile()
{
   if (this->queueChanged)
   {
      L_DEBU("Persisting queue ..");

      Protos::Queue::Queue savedQueue;
      savedQueue.set_version(FILE_QUEUE_VERSION);

      for (QListIterator<Download*> i(this->downloads); i.hasNext();)
      {
         Protos::Queue::Queue_Entry* queueEntry = savedQueue.add_entry();
         Download* download = i.next();
         download->populateRemoteEntry(queueEntry);
         download->populateLocalEntry(queueEntry);
         queueEntry->mutable_peer_id()->set_hash(download->getPeerSourceID().getData(), Common::Hash::HASH_SIZE);
      }

      try
      {
         Common::PersistentData::setValue(Common::FILE_QUEUE, savedQueue, Common::Global::LOCAL);
      }
      catch (Common::PersistentDataIOException& err)
      {
         L_ERRO(err.message);
      }
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

bool DownloadManager::isEntryAlreadyQueued(const Protos::Common::Entry& localEntry, const Common::Hash& peerSourceID)
{
   for (QMultiHash<Common::Hash, Download*>::iterator i = this->downloadsIndexedBySourcePeerID.find(peerSourceID); i != this->downloadsIndexedBySourcePeerID.end() && i.key() == peerSourceID; i++)
   {
      Download* download = i.value();
      if (download->getLocalEntry().shared_dir().id().hash() == localEntry.shared_dir().id().hash() && download->getLocalEntry().path() == localEntry.path() && download->getLocalEntry().name() == localEntry.name())
         return true;
   }
   return false;
}
