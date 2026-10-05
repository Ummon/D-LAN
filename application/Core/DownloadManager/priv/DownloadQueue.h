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

#pragma once

#include <list>
#include <map>
#include <tuple>

#include <QList>
#include <QHash>
#include <QMultiHash>
#include <QMultiMap>

#include <Protos/common.pb.h>
#include <Protos/gui_protocol.pb.h>
#include <Protos/queue.pb.h>

#include <Common/Hash.h>
#include <Common/Uncopyable.h>

#include <IDownload.h>
#include <IChunkDownloader.h>
#include <priv/DownloadPredicate.h>

namespace PM { class IPeer; }

namespace DM
{
   class Download;
   class FileDownload;

   class DownloadQueue : public QObject, Common::Uncopyable
   {
      Q_OBJECT
   public:
      DownloadQueue();
      ~DownloadQueue();

      int size() const;
      void insert(int position, Download* download);
      Download* operator[] (int position) const;
      int find(Download* download) const;
      void remove(int position);

      void peerBecomesAvailable(PM::IPeer* peer);
      bool isAPeerSource(PM::IPeer* peer) const;
      QList<FileDownload*> getNextFilesToHash(const FileDownload* current);

      void moveDownloads(const QList<quint64>& downloadIDRefs, const QList<quint64>& downloadIDs, Protos::GUI::MoveDownloads::Position position);
      bool removeDownloads(const DownloadPredicate& predicate);
      bool pauseDownloads(QList<quint64> IDs, bool pause = true);
      bool isEntryAlreadyQueued(const Protos::Common::Entry& localEntry);

      void setDownloadAsErroneous(Download* download);
      QList<Download*> takeErroneousDownloads();

      QList<QSharedPointer<IChunkDownloader>> getTheOldestUnfinishedChunks(int n);

      static Protos::Queue::Queue loadFromFile();
      bool saveToFile() const;

   private slots:
      void fileDownloadTimeChanged();

   private:
      template <typename P>
      struct Marker { P predicate; int position = 0; };

   public:
      template <typename P>
      class ScanningIterator
      {
      public:
         ScanningIterator(DownloadQueue& queue);
         Download* next();

      private:
         Marker<P>& marker;
         DownloadQueue& queue;
         int position;
      };

   private:
      template <typename F>
      void forEachMarker(F function);
      void updateMarkersInsert(int position, Download* download);
      void updateMarkersRemove(int position);
      void rebuildMarkers();
      void removeFromTimeIndex(FileDownload* download);
      void insertHashingHintCandidate(FileDownload* download, int position);
      void removeHashingHintCandidate(FileDownload* download);
      void rebuildHashingHintCandidates();

      /// Saved some positions: the first downloadable file and the first directory. The goal is to speed up the scan.
      /// There is one marker per predicate the queue can be scanned with, see the class 'ScanningIterator'.
      std::tuple<Marker<IsDownloadable>, Marker<IsADirectory>> markers;

      QList<Download*> downloads; ///< All downloads, it also includes erroneous downloads.
      QList<Download*> erroneousDownloads;
      QMultiMap<qint64, FileDownload*> downloadsSortedByTime; // Key: [ms] since epoch, 0 if never. See 'FileDownload::lastTimeGetAllUnfinishedChunks'.
      // The time map must not be copied: its stored iterators rely on it remaining unshared.
      QHash<FileDownload*, QMultiMap<qint64, FileDownload*>::iterator> downloadTimePositions;
      // Queue order per peer, without files already hinted or with all hashes known.
      // Stable iterators allow pruning and removal without scanning the remaining candidates.
      std::map<PM::IPeer*, std::list<FileDownload*>> hashingHintCandidates;
      QHash<FileDownload*, std::list<FileDownload*>::iterator> hashingHintPositions;
      QMultiHash<PM::IPeer*, Download*> downloadsIndexedBySourcePeer;
      QMultiMap<std::string, Download*> downloadsIndexedByName;
   };
}

/**
  * @class DM::DownloadQueue::ScanningIterator
  *
  * To iterate over the queue for all downloads which match a predicate 'P', one of those having a marker in the queue.
  *
  * A marker is the position before which no download matches its predicate, the scans begin there.
  * A marker only moves forward past downloads not matching when they are scanned, it is never checked again behind it.
  * Thus a predicate can only be given a marker if a queued download can stop matching it but never start matching it
  * again, like 'IsDownloadable' (COMPLETE and DELETED are final) or 'IsADirectory' (the type never changes).
  * A predicate like 'IsComplete' would skip the downloads completed behind its marker.
  * The insertions, removals and moves of downloads are handled, see 'updateMarkersInsert(..)', 'updateMarkersRemove(..)' and 'rebuildMarkers()'.
  */
template <typename P>
DM::DownloadQueue::ScanningIterator<P>::ScanningIterator(DM::DownloadQueue& queue) :
   marker(std::get<Marker<P>>(queue.markers)),
   queue(queue),
   position(this->marker.position)
{
}

/**
  * @return 'nullptr' at the end of the list.
  */
template <typename P>
DM::Download* DM::DownloadQueue::ScanningIterator<P>::next()
{
   while (this->position < this->queue.size())
   {
      Download* download = this->queue[this->position++];
      if (!this->marker.predicate(download))
      {
         if (this->position - 1 == this->marker.position)
            this->marker.position++;
         continue;
      }

      return download;
   }
   return nullptr;
}
