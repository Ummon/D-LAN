#ifndef DOWNLOADMANAGER_DOWNLOADQUEUE_H
#define DOWNLOADMANAGER_DOWNLOADQUEUE_H

#include <QList>
#include <QMultiHash>

#include <Protos/common.pb.h>
#include <Protos/queue.pb.h>

#include <Common/Hash.h>

#include <priv/DownloadPredicate.h>

namespace PM { class IPeer; }

namespace DM
{
   class Download;

   class DownloadQueue
   {
   public:
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

   private:
      QList<Download*> downloads;
      QMultiHash<Common::Hash, Download*> downloadsIndexedBySourcePeerID;
   };
}

#endif
