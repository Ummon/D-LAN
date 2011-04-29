#include <priv/DownloadQueue.h>
using namespace DM;

#include <Common/PersistentData.h>
#include <Common/Constants.h>

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
  *  - Persist/load the queue to/from a file.
  */

DownloadQueue::DownloadQueue() :
   firstUnfinished(0)
{
}

DownloadQueue::~DownloadQueue()
{
   while (!this->downloads.isEmpty())
      delete this->downloads.takeFirst();
}

int DownloadQueue::size() const
{
   return this->downloads.size();
}

void DownloadQueue::insert(int position, Download* download)
{
   if (download->getStatus() == COMPLETE || download->getStatus() == DELETED)
   {
      if (position <= this->firstUnfinished)
         this->firstUnfinished++;
   }
   else
   {
      if (position < this->firstUnfinished)
         this->firstUnfinished = position;
   }

   this->downloads.insert(position, download);
   this->downloadsIndexedBySourcePeerID.insert(download->getPeerSourceID(), download);
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
   if (position < this->firstUnfinished)
      this->firstUnfinished--;

   Download* download = (*this)[position];
   this->downloadsIndexedBySourcePeerID.remove(download->getPeerSourceID(), download);
   this->downloads.removeAt(position);
}

void DownloadQueue::setPeerSource(PM::IPeer* peer)
{
   for (QMultiHash<Common::Hash, Download*>::iterator i = this->downloadsIndexedBySourcePeerID.find(peer->getID()); i != this->downloadsIndexedBySourcePeerID.end() && i.key() == peer->getID(); i++)
      i.value()->setPeer(peer);
}

bool DownloadQueue::isAPeerSource(const Common::Hash& peerID) const
{
   return this->downloadsIndexedBySourcePeerID.contains(peerID);
}

void DownloadQueue::moveDownloads(quint64 downloadIDRef, bool moveBefore, const QList<quint64>& downloadIDs)
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
            int whereToInsert = moveBefore ? iRef++ : ++iRef;
            int whereToRemove = i + 1;

            this->updateFirstUnfinishedMove(this->downloads[i]->getStatus(), whereToInsert, whereToRemove);

            this->downloads.insert(whereToInsert, this->downloads[i]);
            this->downloads.removeAt(whereToRemove);
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
               int whereToInsert = j == 0 && !moveBefore ? ++iRef : iRef;
               int whereToRemove = iToMove[j] + shift;

               this->updateFirstUnfinishedMove(this->downloads[whereToRemove]->getStatus(), whereToInsert, whereToRemove);

               this->downloads.insert(whereToInsert, this->downloads[iToMove[j] + shift]);
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
bool DownloadQueue::removeDownloads(DownloadPredicate& predicate)
{
   bool queueChanged = false;
   QList<Download*> downloadsToDelete;
   QList<Download*>::iterator i = this->downloads.begin();
   QList<Download*>::iterator j = i;
   int position = 0; // Only used to update 'this->firstUnfinished'.
   while (j != this->downloads.end())
   {
      if (predicate(*j))
      {
         queueChanged = true;
         (*j)->setAsDeleted();
         this->downloadsIndexedBySourcePeerID.remove((*j)->getPeerSourceID(), *j);
         downloadsToDelete << *j;
         j++;

         if (position < this->firstUnfinished)
            this->firstUnfinished--;
      }
      else if (i != j)
      {
         j = this->downloads.erase(i, j);
         i = j;
      }
      else
      {
         j++;
         i++;
      }
      position++;
   }

   if (i != j)
      this->downloads.erase(i, j);

   for (QListIterator<Download*> k(downloadsToDelete); k.hasNext();)
      k.next()->remove();

   return queueChanged;
}

bool DownloadQueue::isEntryAlreadyQueued(const Protos::Common::Entry& localEntry, const Common::Hash& peerSourceID)
{
   for (QMultiHash<Common::Hash, Download*>::iterator i = this->downloadsIndexedBySourcePeerID.find(peerSourceID); i != this->downloadsIndexedBySourcePeerID.end() && i.key() == peerSourceID; i++)
   {
      Download* download = i.value();
      if (download->getLocalEntry().shared_dir().id().hash() == localEntry.shared_dir().id().hash() && download->getLocalEntry().path() == localEntry.path() && download->getLocalEntry().name() == localEntry.name())
         return true;
   }
   return false;
}

/**
  * Load the queue from the file and return it. Do not create the downloads itself.
  */
Protos::Queue::Queue DownloadQueue::loadFromFile()
{
   Protos::Queue::Queue savedQueue;

   try
   {
      Common::PersistentData::getValue(Common::FILE_QUEUE, savedQueue, Common::Global::LOCAL);
      if (static_cast<int>(savedQueue.version()) != FILE_QUEUE_VERSION)
      {
         L_USER(QString("The version (%1) of the queue file \"%2\" doesn't match the current version (%3). Queue will be reset.").arg(savedQueue.version()).arg(Common::FILE_QUEUE).arg(FILE_QUEUE_VERSION));
         Common::PersistentData::rmValue(Common::FILE_QUEUE, Common::Global::LOCAL);
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

   return savedQueue;
}

void DownloadQueue::saveToFile() const
{
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
}

void DownloadQueue::updateFirstUnfinishedMove(Status status, int insertPosition, int removePosition)
{
   if (status == COMPLETE || status == DELETED)
   {
      if (insertPosition <= this->firstUnfinished)
         this->firstUnfinished++;
      if (removePosition < this->firstUnfinished)
         this->firstUnfinished--;
   }
   else
   {
      if (removePosition > this->firstUnfinished && insertPosition < this->firstUnfinished)
         this->firstUnfinished = insertPosition;
      if (removePosition < this->firstUnfinished && insertPosition > this->firstUnfinished)
         this->firstUnfinished--;
   }
}

/**
  * @class DM::ScanningIterator
  *
  * Iterate only on non-deleted and non-complete downloads.
  */
DownloadQueue::ScanningIterator::ScanningIterator(DownloadQueue& queue) :
   queue(queue), position(queue.firstUnfinished)
{
}

FileDownload* DownloadQueue::ScanningIterator::next()
{
   while (this->position < this->queue.size())
   {
      FileDownload* fileDownload = dynamic_cast<FileDownload*>(this->queue[this->position++]);
      if (!fileDownload || fileDownload->getStatus() == COMPLETE || fileDownload->getStatus() == DELETED)
      {
         if (this->position - 1 == queue.firstUnfinished)
            queue.firstUnfinished++;
         continue;
      }

      return fileDownload;
   }
   return 0;
}
