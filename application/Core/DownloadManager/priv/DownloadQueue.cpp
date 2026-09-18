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
#include <utility>

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
  *  - Save some positions (markers) to improve iterating performance (see the 'ScanningIterator' class).
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
   if (position < 0 || position > this->downloads.size())
   {
      L_DEBU(
         QString("Unable to insert download %1, invalid position: %2")
            .arg(Common::ProtoHelper::getDebugStr(download->getLocalEntry()))
            .arg(position)
      );

      return;
   }

   this->updateMarkersInsert(position, download);

   this->downloads.insert(position, download);
   this->downloadsIndexedBySourcePeer.insert(download->getPeerSource(), download);

   if (FileDownload* fileDownload = dynamic_cast<FileDownload*>(download))
   {
      this->downloadsIndexedByName.insert(download->getLocalEntry().name(), download);
      this->downloadTimePositions.insert(fileDownload, this->downloadsSortedByTime.insert(0, fileDownload));

      // The connection must be direct: a queued one would be delivered after the download has possibly been removed
      // and deleted, 'sender()' would then return a null pointer and 'downloadsSortedByTime' would keep a dangling
      // entry indexed by the old time. 'getTheOldestUnfinishedChunks(..)' advances its iterator
      // before asking for chunks, since this signal can erase the current file's index entry.
      connect(fileDownload, &FileDownload::lastTimeGetAllUnfinishedChunksChanged, this, &DownloadQueue::fileDownloadTimeChanged, Qt::DirectConnection);
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
   {
      this->removeFromTimeIndex(fileDownload);
      disconnect(fileDownload, &FileDownload::lastTimeGetAllUnfinishedChunksChanged, this, &DownloadQueue::fileDownloadTimeChanged);
   }

   this->downloadsIndexedByName.remove(download->getLocalEntry().name(), download);
   this->downloadsIndexedBySourcePeer.remove(download->getPeerSource(), download);
   this->downloads.removeAt(position);
   this->erroneousDownloads.removeAll(download);
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

   const QSet<quint64> IDsToMove(downloadIDs.cbegin(), downloadIDs.cend());
   const QSet<quint64> referenceIDs(downloadIDRefs.cbegin(), downloadIDRefs.cend());
   QList<Download*> moved;
   QList<Download*> remaining;
   remaining.reserve(this->downloads.size());
   int insertionPosition = -1;

   // Use the first reference for BEFORE and the last for AFTER. Measure the
   // boundary in the retained queue, even when a reference is itself selected.
   for (Download* download : std::as_const(this->downloads))
   {
      const bool isReference = referenceIDs.contains(download->getID());
      if (isReference && position == Protos::GUI::MoveDownloads::BEFORE && insertionPosition == -1)
         insertionPosition = remaining.size();

      if (IDsToMove.contains(download->getID()))
         moved.append(download);
      else
         remaining.append(download);

      if (isReference && position == Protos::GUI::MoveDownloads::AFTER)
         insertionPosition = remaining.size();
   }

   if (insertionPosition == -1 || moved.isEmpty())
      return;

   // Rebuild once, preserving the queue order of both groups regardless of the
   // order (or duplicates) in the supplied ID lists.
   this->downloads.clear();
   for (int i = 0; i < insertionPosition; ++i)
      this->downloads.append(remaining[i]);
   this->downloads.append(moved);
   for (int i = insertionPosition; i < remaining.size(); ++i)
      this->downloads.append(remaining[i]);
   this->rebuildMarkers();
}

/**
  * Remove all download for which the given predicate is true.
  * @return Returns 'true' is the list has been altered.
  */
bool DownloadQueue::removeDownloads(const DownloadPredicate& predicate)
{
   QList<Download*> downloadsToDelete;
   QSet<Download*> removed;
   int retained = 0;
   for (int i = 0; i < this->downloads.size(); ++i)
   {
      Download* download = this->downloads[i];
      if (predicate(download))
      {
         download->setAsDeleted();
         downloadsToDelete.append(download);
         removed.insert(download);
         this->updateMarkersRemove(retained);
         if (FileDownload* fileDownload = dynamic_cast<FileDownload*>(download))
         {
            this->removeFromTimeIndex(fileDownload);
            disconnect(fileDownload, &FileDownload::lastTimeGetAllUnfinishedChunksChanged, this, &DownloadQueue::fileDownloadTimeChanged);
         }
      }
      else
         this->downloads[retained++] = download;
   }

   if (removed.isEmpty())
      return false;

   this->downloads.resize(retained);
   this->erroneousDownloads.removeIf([&removed](Download* download) { return removed.contains(download); });

   // Erase each index entry at most once. Removing individual values repeatedly
   // also becomes quadratic when many downloads share a peer or name.
   const auto removeFromIndex = [&removed](auto& index)
   {
      for (auto i = index.begin(); i != index.end();)
         if (removed.contains(i.value()))
            i = index.erase(i);
         else
            ++i;
   };
   removeFromIndex(this->downloadsIndexedBySourcePeer);
   removeFromIndex(this->downloadsIndexedByName);

   // Removing a file can trigger queue scans. Finish compaction and index cleanup
   // before any of those callbacks can run.
   for (Download* download : std::as_const(downloadsToDelete))
      download->remove();

   return true;
}

/**
  * Return true if one or more download have been paused or un-paused.
  */
bool DownloadQueue::pauseDownloads(QList<quint64> IDs, bool pause)
{
   QSet<quint64> IDsRemaining(IDs.begin(), IDs.end());

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
   auto i = this->downloadsIndexedByName.constFind(localEntry.name());

   while (i != this->downloadsIndexedByName.constEnd() && i.key() == localEntry.name())
   {
      if (
         // Pending custom destinations have no share ID yet. Compare their absolute
         // file paths as well, including against an already-created shared file.
         (!localEntry.path().empty() || localEntry.shared_entry().path().empty() ||
            i.value()->getLocalEntry().shared_entry().path() == localEntry.shared_entry().path()) &&
         i.value()->getLocalEntry().path() == localEntry.path() &&
         (
            !localEntry.has_shared_entry() || localEntry.shared_entry().id().hash().empty() ||
            i.value()->getLocalEntry().shared_entry().id().hash() == localEntry.shared_entry().id().hash()
         )
      )
         return true;

       ++i;
   }

   return false;
}

void DownloadQueue::setDownloadAsErroneous(Download* download)
{
   // A download may recover and fail again before its pending retry is consumed.
   // Keep its original retry position and never retain duplicate pointers.
   if (!this->erroneousDownloads.contains(download))
      this->erroneousDownloads << download;
}

/**
  * If a erroneous download is returns it is removed from the erroneous list.
  */
Download* DownloadQueue::getAnErroneousDownload()
{
   if (!this->erroneousDownloads.isEmpty())
      return this->erroneousDownloads.takeFirst();
   return 0;
}

QList<QSharedPointer<IChunkDownloader>> DownloadQueue::getTheOldestUnfinishedChunks(int n)
{
   if (n <= 0)
      return {};

   QList<QSharedPointer<IChunkDownloader>> unfinishedChunks;
   QSet<FileDownload*> visited;
   auto i = this->downloadsSortedByTime.begin();
   while (i != this->downloadsSortedByTime.end() && unfinishedChunks.size() < n)
   {
      FileDownload* download = i.value();
      if (download->getStatus() == Protos::Common::DownloadStatus::COMPLETE || download->getStatus() == Protos::Common::DownloadStatus::DELETED)
      {
         this->downloadTimePositions.remove(download);
         i = this->downloadsSortedByTime.erase(i);
         continue;
      }

      // Collecting chunks can erase and reinsert this file with a new timestamp.
      // Other QMultiMap iterators remain valid; advance before the signal fires.
      // Skip reinserted files so each file contributes at most once per request,
      // including when several files receive the same millisecond timestamp.
      ++i;
      if (download->getStatus() == Protos::Common::DownloadStatus::PAUSED || visited.contains(download))
         continue;
      visited.insert(download);
      download->getUnfinishedChunks(unfinishedChunks, n - unfinishedChunks.size());
      // Files with unknown or completed chunks may contribute nothing. Keep
      // scanning until the chunk budget is filled or the index is exhausted.
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
      Common::PersistentData::getValue(Common::Constants::FILE_QUEUE, savedQueue, Common::Global::DataFolderType::LOCAL);
      if (static_cast<int>(savedQueue.version()) != FILE_QUEUE_VERSION)
      {
         L_USER(
            QString(
               QObject::tr("The version (%1) of the queue file \"%2\" doesn't match the current version (%3). Queue will be reset.")
            )
               .arg(savedQueue.version())
               .arg(Common::Constants::FILE_QUEUE)
               .arg(FILE_QUEUE_VERSION)
         );
         Common::PersistentData::rmValue(Common::Constants::FILE_QUEUE, Common::Global::DataFolderType::LOCAL);
         savedQueue.Clear();
      }
   }
   catch (Common::UnknownValueException& e)
   {
      L_WARN(QString("The download queue file cache cannot be retrieved (the file doesn't exist): %1").arg(Common::Constants::FILE_QUEUE));
   }
   catch (...)
   {
      L_WARN(QString("The download queue file cache cannot be retrieved (Unknown exception): %1").arg(Common::Constants::FILE_QUEUE));
   }

   return savedQueue;
}

/**
  * Return true only when the queue was successfully persisted.
  */
bool DownloadQueue::saveToFile() const
{
   Protos::Queue::Queue savedQueue;
   savedQueue.set_version(FILE_QUEUE_VERSION);

   for (const auto& download : this->downloads)
   {
      Protos::Queue::Queue::Entry* queueEntry = savedQueue.add_entries();
      download->populateQueueEntry(queueEntry);
   }

   try
   {
      Common::PersistentData::setValue(Common::Constants::FILE_QUEUE, savedQueue, Common::Global::DataFolderType::LOCAL);
   }
   catch (Common::PersistentDataIOException& err)
   {
      L_ERRO(err.message);
      return false;
   }
   return true;
}

/**
  * Called by 'FileDownload::getUnfinishedChunks(..)' via a direct connection, see 'insert(..)'.
  */
void DownloadQueue::fileDownloadTimeChanged()
{
   FileDownload* fileDownload = static_cast<FileDownload*>(this->sender());
   auto position = this->downloadTimePositions.find(fileDownload);
   if (position == this->downloadTimePositions.end())
      return;

   // Most newly queued files share timestamp zero. Erasing by iterator avoids
   // scanning that entire group for every file whose discovery round finishes.
   this->downloadsSortedByTime.erase(position.value());
   position.value() = this->downloadsSortedByTime.insert(fileDownload->getLastTimeGetAllUnfinishedChunks(), fileDownload);
}

void DownloadQueue::removeFromTimeIndex(FileDownload* download)
{
   auto position = this->downloadTimePositions.find(download);
   if (position == this->downloadTimePositions.end())
      return; // Discovery may already have pruned a completed or deleted file.

   this->downloadsSortedByTime.erase(position.value());
   this->downloadTimePositions.erase(position);
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

void DownloadQueue::rebuildMarkers()
{
   for (Marker& marker : this->markers)
   {
      marker.position = 0;
      while (marker.position < this->downloads.size() && !(*marker.predicate)(this->downloads[marker.position]))
         ++marker.position;
   }
}
