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

#include <map>
#include <memory>
#include <typeindex>
#include <typeinfo>

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

      void moveDownloads(const QList<quint64>& downloadIDRefs, const QList<quint64>& downloadIDs, Protos::GUI::MoveDownloads::Position position);
      bool removeDownloads(const DownloadPredicate& predicate);
      bool pauseDownloads(QList<quint64> IDs, bool pause = true);
      bool isEntryAlreadyQueued(const Protos::Common::Entry& localEntry);

      void setDownloadAsErroneous(Download* download);
      Download* getAnErroneousDownload();

      QList<QSharedPointer<IChunkDownloader>> getTheOldestUnfinishedChunks(int n);

      static Protos::Queue::Queue loadFromFile();
      bool saveToFile() const;

   private slots:
      void fileDownloadTimeChanged();

   private:
      struct Marker;

   public:
      template <typename P>
      class ScanningIterator
      {
      public:
         ScanningIterator(DownloadQueue& queue);
         Download* next();

      private:
         Marker* marker;
         DownloadQueue& queue;
         int position;
      };

   private:
      void updateMarkersInsert(int position, Download* download);
      void updateMarkersRemove(int position);
      void rebuildMarkers();
      void removeFromTimeIndex(FileDownload* download);

      struct Marker { std::unique_ptr<DownloadPredicate> predicate; int position = 0; };

      /// Saved some positions like the first downloadable file or the first directory, one per predicate type. The goal is to speed up the scan.
      /// See the class 'ScanningIterator', which keeps a pointer to its marker: the container must not move them when a marker is added.
      std::map<std::type_index, Marker> markers;

      QList<Download*> downloads; ///< All downloads, it also includes erroneous downloads.
      QList<Download*> erroneousDownloads;
      QMultiMap<qint64, FileDownload*> downloadsSortedByTime; // Key: [ms] since epoch, 0 if never. See 'FileDownload::lastTimeGetAllUnfinishedChunks'.
      // The time map must not be copied: its stored iterators rely on it remaining unshared.
      QHash<FileDownload*, QMultiMap<qint64, FileDownload*>::iterator> downloadTimePositions;
      QMultiHash<PM::IPeer*, Download*> downloadsIndexedBySourcePeer;
      QMultiMap<std::string, Download*> downloadsIndexedByName;
   };
}

/**
  * @class DM::DownloadQueue::ScanningIterator
  *
  * To iterate over the queue for all downloads which match a predicate 'P'.
  */
template <typename P>
DM::DownloadQueue::ScanningIterator<P>::ScanningIterator(DM::DownloadQueue& queue) :
   marker(&queue.markers[typeid(P)]),
   queue(queue)
{
   if (!this->marker->predicate) // First scan with 'P'.
      this->marker->predicate = std::make_unique<P>();
   this->position = this->marker->position;
}

/**
  * @return Return 0 at the end of the list.
  */
template <typename P>
DM::Download* DM::DownloadQueue::ScanningIterator<P>::next()
{
   while (this->position < this->queue.size())
   {
      Download* download = this->queue[this->position++];
      if (!(*this->marker->predicate)(download))
      {
         if (this->position - 1 == this->marker->position)
            this->marker->position++;
         continue;
      }

      return download;
   }
   return 0;
}
