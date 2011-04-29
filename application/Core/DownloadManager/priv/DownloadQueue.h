#ifndef DOWNLOADMANAGER_DOWNLOADQUEUE_H
#define DOWNLOADMANAGER_DOWNLOADQUEUE_H

#include <QList>
#include <QMultiHash>

#include <Protos/common.pb.h>
#include <Protos/queue.pb.h>

#include <Common/Hash.h>

#include <IDownload.h>
#include <priv/DownloadPredicate.h>

namespace PM { class IPeer; }

namespace DM
{
   class Download;
   class FileDownload;

   class DownloadQueue
   {
   public:
      DownloadQueue();
      ~DownloadQueue();

      int size() const;
      void insert(int position, Download* download);
      Download* operator[] (int position) const;
      int find(Download* download) const;
      void remove(int position);

      void setPeerSource(PM::IPeer* peer);
      bool isAPeerSource(const Common::Hash& peerID) const;

      void moveDownloads(quint64 downloadIDRef, bool moveBefore, const QList<quint64>& downloadIDs);
      bool removeDownloads(DownloadPredicate& predicate);
      bool isEntryAlreadyQueued(const Protos::Common::Entry& localEntry, const Common::Hash& peerSourceID);

      Protos::Queue::Queue loadFromFile();
      void saveToFile() const;

      /**
        * @class DM::DownloadQueue::ScanningIterator
        *
        * An iterator to search a file to download. It will only iterate on 'FileDownload' which are not finished an not deleted.
        */
      class ScanningIterator
      {
      public:
         ScanningIterator(DownloadQueue& queue);
         FileDownload* next();

      private:
         DownloadQueue& queue;
         int position;
      };

   private:
      void updateFirstUnfinishedMove(Status status, int insertPosition, int removePosition);

      int firstUnfinished; ///< The position of the first download not complete. It may be a equal to 'this->downloads.size()'. The goal is to avoid to scan complete download. For example, if the three first downloads are complete then the 'firstUnfininshed' will be the fourth.
      QList<Download*> downloads;
      QMultiHash<Common::Hash, Download*> downloadsIndexedBySourcePeerID;
   };
}

#endif
