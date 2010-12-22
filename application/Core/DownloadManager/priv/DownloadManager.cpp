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
  
#include <priv/DownloadManager.h>
using namespace DM;

#include <Protos/queue.pb.h>

#include <Common/Settings.h>
#include <Common/PersistentData.h>
#include <Common/Constants.h>
#include <Common/ProtoHelper.h>

#include <priv/Log.h>
#include <priv/FileDownload.h>
#include <priv/DirDownload.h>
#include <priv/Constants.h>

DownloadManager::DownloadManager(QSharedPointer<FM::IFileManager> fileManager, QSharedPointer<PM::IPeerManager> peerManager) :
   NUMBER_OF_DOWNLOADER(static_cast<int>(SETTINGS.get<quint32>("number_of_downloader"))),
   fileManager(fileManager),
   peerManager(peerManager),
   numberOfDownload(0),
   retrievingEntries(false),
   queueChanged(false)
{
   connect(&this->occupiedPeersAskingForHashes, SIGNAL(newFreePeer(PM::IPeer*)), this, SLOT(peerNoLongerAskingForHashes(PM::IPeer*)));
   connect(&this->occupiedPeersDownloadingChunk, SIGNAL(newFreePeer(PM::IPeer*)), this, SLOT(peerNoLongerDownloadingChunk(PM::IPeer*)));

   connect(this->fileManager.data(), SIGNAL(fileCacheLoaded()), this, SLOT(fileCacheLoaded()));

   this->rescanTimer.setInterval(RESCAN_QUEUE_PERIOD_IF_ERROR);
   this->rescanTimer.setSingleShot(true);
   connect(&this->rescanTimer, SIGNAL(timeout()), this, SLOT(scanTheQueue()));

   this->saveTimer.setInterval(SETTINGS.get<quint32>("save_queue_period"));
   connect(&this->saveTimer, SIGNAL(timeout()), this, SLOT(saveQueueToFile()));
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
void DownloadManager::addDownload(const Protos::Common::Entry& entry, Common::Hash peerSource)
{
   QMutableListIterator<Download*> i(this->downloads);
   i.toBack();
   this->addDownload(entry, peerSource, false, i);
}

void DownloadManager::addDownload(const Protos::Common::Entry& entry, Common::Hash peerSource, bool complete)
{
   QMutableListIterator<Download*> i(this->downloads);
   i.toBack();
   this->addDownload(entry, peerSource, complete, i);
}

/**
  * Insert a new download at the given position.
  */
void DownloadManager::addDownload(const Protos::Common::Entry& entry, Common::Hash peerSource, bool complete, QMutableListIterator<Download*>& iterator)
{
   // If there is a lot of file in queue it can be a bit CPU consumer.
   if (this->isEntryAlreadyQueued(entry))
   {
      L_WARN(QString("Entry already queued, it will no be added to the queue : %1").arg(Common::ProtoHelper::getStr(entry, &Protos::Common::Entry::name)));
      return;
   }

   Download* newDownload = 0;

   switch (entry.type())
   {
   case Protos::Common::Entry_Type_DIR :
      {
         DirDownload* dirDownload = new DirDownload(this->fileManager, this->peerManager, peerSource, entry);
         newDownload = dirDownload;
         connect(dirDownload, SIGNAL(newEntries(const Protos::Common::Entries&)), this, SLOT(newEntries(const Protos::Common::Entries&)), Qt::DirectConnection);
         iterator.insert(dirDownload);
         this->scanTheQueueToRetrieveEntries();
      }
      break;

   case Protos::Common::Entry_Type_FILE :
      {
         FileDownload* fileDownload = new FileDownload(this->fileManager, this->peerManager, this->occupiedPeersAskingForHashes, this->occupiedPeersDownloadingChunk, peerSource, entry, complete);
         newDownload = fileDownload;
         connect(fileDownload, SIGNAL(newHashKnown()), this, SLOT(setQueueChanged()), Qt::DirectConnection);
         iterator.insert(fileDownload);
         fileDownload->start();
      }
      break;
   }

   if (newDownload)
      connect(newDownload, SIGNAL(deleted(Download*)), this, SLOT(downloadDeleted(Download*)), Qt::DirectConnection);

   this->setQueueChanged();
}

QList<IDownload*> DownloadManager::getDownloads() const
{
   QList<IDownload*> listDownloads;

   // TODO : very heavy!
   for (QListIterator<Download*> i(this->downloads); i.hasNext();)
      listDownloads << i.next();

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

      fileDownload->getUnfinishedChunks(unfinishedChunks, n);
   }

   return unfinishedChunks;
}

int DownloadManager::getDownloadRate() const
{
   int downloadRate = 0;
   for (QListIterator<Download*> i(this->downloads); i.hasNext();)
   {
      FileDownload* fileDownload = dynamic_cast<FileDownload*>(i.next());

      if (fileDownload && fileDownload->getStatus() == DOWNLOADING)
         downloadRate += fileDownload->getDownloadRate();
   }
   return downloadRate;
}

void DownloadManager::fileCacheLoaded()
{
   this->loadQueueFromFile();
}

void DownloadManager::newEntries(const Protos::Common::Entries& entries)
{
   DirDownload* dirDownload = dynamic_cast<DirDownload*>(this->sender());
   QMutableListIterator<Download*> i(this->downloads);
   if (!i.findNext(dirDownload))
      return;
   i.remove();

   for (int n = 0; n < entries.entry_size(); n++)
      this->addDownload(entries.entry(n), dirDownload->getPeerSourceID(), false, i);

   delete dirDownload;

   this->retrievingEntries = false;
   this->scanTheQueueToRetrieveEntries();
}

void DownloadManager::downloadDeleted(Download* download)
{
   this->downloads.removeOne(download);
   this->setQueueChanged();
}

/**
  * Search for a new file to asking hashes.
  */
void DownloadManager::peerNoLongerAskingForHashes(PM::IPeer* peer)
{
   for (QListIterator<Download*> i(this->downloads); i.hasNext();)
   {
      FileDownload* fileDownload = dynamic_cast<FileDownload*>(i.next());
      if (fileDownload && fileDownload->retreiveHashes())
         break;
   }
}

/**
  * Search a chunk to download.
  */
void DownloadManager::peerNoLongerDownloadingChunk(PM::IPeer* peer)
{
   L_DEBU(QString("A peer is free : %1, number of downloading thread : %2").arg(peer->getID().toStr()).arg(this->numberOfDownload));
   this->scanTheQueue();
}

void DownloadManager::scanTheQueueToRetrieveEntries()
{
   if (this->retrievingEntries)
      return;

   L_DEBU("Scanning the queue to retrieve entries");

   Common::Hash peerID;

   for (QListIterator<Download*> i(this->downloads); i.hasNext();)
   {
      DirDownload* dirDownload = dynamic_cast<DirDownload*>(i.next());
      if (!dirDownload || dirDownload->getPeerSourceID() == peerID)
         continue;

      if (!dirDownload->retrieveEntries())
      {
         // If we can't retrieve entries from 'dirDownload' because its peer is unknown we save its peerID to prevent
         // to reask to the same peer twice.
         peerID = dirDownload->getPeerSourceID();
         continue;
      }

      this->retrievingEntries = true;
      return;
   }
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

      chunkDownload = fileDownload->getAChunkToDownload();

      if (fileDownload->isStatusErroneous())
         this->rescanTimer.start();

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
      Common::PersistentData::getValue(Common::FILE_QUEUE, savedQueue);
      if (static_cast<int>(savedQueue.version()) != FILE_QUEUE_VERSION)
      {
         L_ERRO(QString("The version (%1) of the queue file \"%2\" doesn't match the current version (%3)").arg(savedQueue.version()).arg(Common::FILE_QUEUE).arg(FILE_QUEUE_VERSION));
         Common::PersistentData::rmValue(Common::FILE_QUEUE);
         goto end;
      }

      for (int i = 0; i < savedQueue.entry_size(); i++)
      {
         const Protos::Queue::Queue_Entry& entry = savedQueue.entry(i);
         this->addDownload(entry.entry(), Common::Hash(entry.peer_id().hash().data()), entry.complete());
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
         i.next()->populateEntry(savedQueue.add_entry());
      }

      try
      {
         Common::PersistentData::setValue(Common::FILE_QUEUE, savedQueue);
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

bool DownloadManager::isEntryAlreadyQueued(const Protos::Common::Entry& entry)
{
   for (QListIterator<Download*> i(this->downloads); i.hasNext();)
   {
      Download* download = i.next();
      // TODO : Do we should check peer_id also?
      if (
         download->getEntry().type() == entry.type() &&
         download->getEntry().path() == entry.path() &&
         download->getEntry().name() == entry.name() &&
         download->getEntry().size() == entry.size()
      )
         return true;
   }
   return false;
}
