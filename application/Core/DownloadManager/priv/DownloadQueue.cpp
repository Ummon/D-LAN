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
  
#include <priv/DownloadQueue.h>
using namespace DM;

#include <QSet>

#include <Common/PersistentData.h>
#include <Common/Constants.h>
#include <Common/ProtoHelper.h>

#include <priv/Download.h>
#include <priv/FileDownload.h>
#include <priv/Log.h>
#include <priv/Constants.h>

/**
  * @class DM::DownloadQueue
  *
  * Goals:
  *  - Manage a queue of downloads.
  *  - Index queue by download peers to improve performance.
  *  - Save some positions (markers) to improve itarating performance (see the 'ScanningIterator' class).
  *  - Persist/load the queue to/from a file.
  */

DownloadQueue::DownloadQueue()
{
}

DownloadQueue::~DownloadQueue()
{
   while (!this->downloads.isEmpty())
      delete this->downloads.takeFirst();

   for (QListIterator<Marker> i(this->markers); i.hasNext();)
      delete i.next().predicate;
}

int DownloadQueue::size() const
{
   return this->downloads.size();
}

void DownloadQueue::insert(int position, Download* download)
{
   this->updateMarkersInsert(position, download);

   this->downloads.insert(position, download);
   this->downloadsIndexedBySourcePeer.insert(download->getPeerSource(), download);

   if (FileDownload* fileDownload = dynamic_cast<FileDownload*>(download))
   {
      this->downloadsSortedByTime.insert(QTime(), fileDownload);
      connect(fileDownload, SIGNAL(lastTimeGetAllUnfinishedChunksChanged(QTime)), this, SLOT(fileDownloadTimeChanged(QTime)), Qt::QueuedConnection);
   }
}

Download* DownloadQueue::operator[] (int position) const
{
   return this->downloads[position];
}

int DownloadQueue::find(Download* download) const
{
   return this->downloads.indexOf(download);
}

void DownloadQueue::remove(int position)
{
   this->updateMarkersRemove(position);

   Download* download = (*this)[position];

   if (FileDownload* fileDownload = dynamic_cast<FileDownload*>(download))
      this->downloadsSortedByTime.remove(fileDownload->getLastTimeGetAllUnfinishedChunks(), fileDownload);

   this->downloadsIndexedBySourcePeer.remove(download->getPeerSource(), download);
   this->downloads.removeAt(position);
   this->erroneousDownloads.removeOne(download);
}

void DownloadQueue::peerBecomesAvailable(PM::IPeer* peer)
{
   for (QMultiHash<PM::IPeer*, Download*>::iterator i = this->downloadsIndexedBySourcePeer.find(peer); i != this->downloadsIndexedBySourcePeer.end() && i.key() == peer; ++i)
      i.value()->peerSourceBecomesAvailable();
}

bool DownloadQueue::isAPeerSource(PM::IPeer* peer) const
{
   return this->downloadsIndexedBySourcePeer.contains(peer);
}

void DownloadQueue::moveDownloads(const QList<quint64>& downloadIDRefs, const QList<quint64>& downloadIDs, Protos::GUI::MoveDownloads::Position position)
{
   if (downloadIDRefs.isEmpty() || downloadIDs.isEmpty())
      return;

   QList<quint64> downloadIDsCopy(downloadIDs);
   QList<quint64> downloadIDRefsCopy(downloadIDRefs);

   quint64 downloadIDRef = downloadIDRefsCopy.size() == 1 ? downloadIDRefsCopy.first() : 0;
   int iRef = -1; // Index of the download reference, -1 if unknown.
   QList<int> iToMove;

   for (int i = 0; i < this->downloads.size(); i++)
   {
      int j;
      if ((j = downloadIDsCopy.indexOf(this->downloads[i]->getID())) != -1)
      {
         if (iRef != -1)
         {
            const int whereToInsert = position == Protos::GUI::MoveDownloads::BEFORE ? iRef++ : ++iRef;
            const int whereToRemove = i + 1;

            this->updateMarkersMove(whereToInsert, whereToRemove, this->downloads[i]);

            this->downloads.insert(whereToInsert, this->downloads[i]);
            this->downloads.removeAt(whereToRemove);
            continue;
         }
         else
            iToMove << i;

         downloadIDsCopy.removeAt(j);
      }

      // If the download reference isn't defined we have to search one among 'downloadIDRefsCopy'.
      int k;
      if (downloadIDRef == 0 && (k = downloadIDRefsCopy.indexOf(this->downloads[i]->getID())) != -1)
      {
         if (position == Protos::GUI::MoveDownloads::BEFORE)
         {
            downloadIDRef = this->downloads[i]->getID();
         }
         else
         {
            if (downloadIDRefsCopy.size() == 1)
               downloadIDRef = downloadIDRefsCopy.first();
            else
               downloadIDRefsCopy.removeAt(k);
         }
      }

      if (this->downloads[i]->getID() == downloadIDRef)
      {
         iRef = i;
         int shift = 0;
         for (int j = 0; j < iToMove.size(); j++)
         {
            if (iToMove[j] == iRef && position == Protos::GUI::MoveDownloads::BEFORE)
               iRef++;
            else
            {
               const int whereToInsert = position == Protos::GUI::MoveDownloads::AFTER ? iRef + 1 : iRef;
               const int whereToRemove = iToMove[j] + shift;

               this->updateMarkersMove(whereToInsert, whereToRemove, this->downloads[whereToRemove]);

               this->downloads.insert(whereToInsert, this->downloads[whereToRemove]);
               this->downloads.removeAt(whereToRemove);
               shift--;
            }
         }
         iToMove.clear();
      }
   }
}

/**
  * Remove all download for which the given predicate is true.
  * It uses QList::erase(iterator begin, iterator end) to improve the performance.
  * @return Returns 'true' is the list has been altered.
  */
bool DownloadQueue::removeDownloads(const DownloadPredicate& predicate)
{
   bool queueChanged = false;
   QList<Download*> downloadsToDelete;
   QList<Download*>::iterator i = this->downloads.begin();
   QList<Download*>::iterator j = i;
   int position = 0; // Only used to update the markers.
   while (j != this->downloads.end())
   {
      if (predicate(*j))
      {
         queueChanged = true;
         (*j)->setAsDeleted();
         if (FileDownload* fileDownload = dynamic_cast<FileDownload*>(*j))
            this->downloadsSortedByTime.remove(fileDownload->getLastTimeGetAllUnfinishedChunks(), fileDownload);
         this->downloadsIndexedBySourcePeer.remove((*j)->getPeerSource(), *j);
         downloadsToDelete << *j;
         this->erroneousDownloads.removeOne(*j);
         ++j;

         this->updateMarkersRemove(position);
      }
      else if (i != j)
      {
         j = this->downloads.erase(i, j);
         i = j;
      }
      else
      {
         ++j;
         ++i;
         position++;
      }
   }

   if (i != j)
      this->downloads.erase(i, j);

   for (QListIterator<Download*> k(downloadsToDelete); k.hasNext();)
      k.next()->remove();

   return queueChanged;
}

/**
  * Return true if one or more download have been paused or unpaused.
  */
bool DownloadQueue::pauseDownloads(QList<quint64> IDs, bool pause)
{
   QSet<quint64> IDsRemaining(IDs.toSet());

   bool stateChanged = false;

   for (QListIterator<Download*> i(this->downloads); i.hasNext() && !IDsRemaining.isEmpty();)
   {
      Download* download = i.next();
      if (IDsRemaining.remove(download->getID()))
      {
         if (download->pause(pause))
            stateChanged = true;
      }
   }

   return stateChanged;
}

/**
  * To know if a given entry is already in queue. It depends of (shared dir id, name, path).
  */
bool DownloadQueue::isEntryAlreadyQueued(const Protos::Common::Entry& localEntry)
{
   for (QListIterator<Download*> i(this->downloads); i.hasNext();)
   {
      const Download* download = i.next();
      if (
         download->getLocalEntry().name() == localEntry.name() &&
         download->getLocalEntry().path() == localEntry.path() &&
         (!localEntry.has_shared_dir() || download->getLocalEntry().shared_dir().id().hash() == localEntry.shared_dir().id().hash())
      )
         return true;
   }
   return false;
}

void DownloadQueue::setDownloadAsErroneous(Download* download)
{
   this->erroneousDownloads << download;
}

Download* DownloadQueue::getAnErroneousDownload()
{
   if (!this->erroneousDownloads.isEmpty())
      return this->erroneousDownloads.takeFirst();
   return 0;
}

QList< QSharedPointer<IChunkDownload> > DownloadQueue::getTheOldestUnfinishedChunks(int n)
{
   QList< QSharedPointer<IChunkDownload> > unfinishedChunks;

   for (QMutableMapIterator<QTime, FileDownload*> i(this->downloadsSortedByTime); i.hasNext() && unfinishedChunks.size() < n;)
   {
      i.next();
      if (i.value()->getStatus() == COMPLETE || i.value()->getStatus() == DELETED)
         i.remove();
      else
         i.value()->getUnfinishedChunks(unfinishedChunks, n - unfinishedChunks.size());
   }

   return unfinishedChunks;
}

/**
  * Load the queue from the file and return it. Do not create the downloads itself.
  */
Protos::Queue::Queue DownloadQueue::loadFromFile()
{
   Protos::Queue::Queue savedQueue;

   try
   {
      Common::PersistentData::getValue(Common::Constants::FILE_QUEUE, savedQueue, Common::Global::LOCAL);
      if (static_cast<int>(savedQueue.version()) != FILE_QUEUE_VERSION)
      {
         L_USER(QString(QObject::tr("The version (%1) of the queue file \"%2\" doesn't match the current version (%3). Queue will be reset.")).arg(savedQueue.version()).arg(Common::Constants::FILE_QUEUE).arg(FILE_QUEUE_VERSION));
         Common::PersistentData::rmValue(Common::Constants::FILE_QUEUE, Common::Global::LOCAL);
         savedQueue.Clear();
      }
   }
   catch (Common::UnknownValueException& e)
   {
      L_WARN(QString("The download queue file cache cannot be retrived (the file doesn't exist) : %1").arg(Common::Constants::FILE_QUEUE));
   }
   catch (...)
   {
      L_WARN(QString("The download queue file cache cannot be retrived (Unkown exception) : %1").arg(Common::Constants::FILE_QUEUE));
   }

   return savedQueue;
}

void DownloadQueue::saveToFile() const
{
   Protos::Queue::Queue savedQueue;
   savedQueue.set_version(FILE_QUEUE_VERSION);

   for (QListIterator<Download*> i(this->downloads); i.hasNext();)
   {
      Protos::Queue::Queue::Entry* queueEntry = savedQueue.add_entry();
      Download* download = i.next();
      download->populateQueueEntry(queueEntry);
   }

   try
   {
      Common::PersistentData::setValue(Common::Constants::FILE_QUEUE, savedQueue, Common::Global::LOCAL);
   }
   catch (Common::PersistentDataIOException& err)
   {
      L_ERRO(err.message);
   }
}

void DownloadQueue::fileDownloadTimeChanged(QTime oldTime)
{
   FileDownload* fileDownload = static_cast<FileDownload*>(this->sender());
   this->downloadsSortedByTime.remove(oldTime, fileDownload);
   this->downloadsSortedByTime.insert(fileDownload->getLastTimeGetAllUnfinishedChunks(), fileDownload);
}

void DownloadQueue::updateMarkersInsert(int position, Download* download)
{
   for (QMutableListIterator<Marker> i(this->markers); i.hasNext();)
   {
      Marker& m = i.next();
      if (!(*m.predicate)(download))
      {
         if (position <= m.position)
            m.position++;
      }
      else
      {
         if (position < m.position)
            m.position = position;
      }
   }
}

void DownloadQueue::updateMarkersRemove(int position)
{
   for (QMutableListIterator<Marker> i(this->markers); i.hasNext();)
   {
      Marker& m = i.next();
      if (position < m.position)
         m.position--;
   }
}

void DownloadQueue::updateMarkersMove(int insertPosition, int removePosition, Download* download)
{
   for (QMutableListIterator<Marker> i(this->markers); i.hasNext();)
   {
      Marker& m = i.next();
      if (!(*m.predicate)(download))
      {
         if (insertPosition <= m.position)
            m.position++;
         if (removePosition < m.position)
            m.position--;
      }
      else
      {
         if (removePosition > m.position && insertPosition < m.position)
            m.position = insertPosition;
         if (removePosition < m.position && insertPosition > m.position)
            m.position--;
      }
   }
}
