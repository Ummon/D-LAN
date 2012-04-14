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
  
#ifndef DOWNLOADMANAGER_DOWNLOADQUEUE_H
#define DOWNLOADMANAGER_DOWNLOADQUEUE_H

#include <typeinfo>

#include <QList>
#include <QMultiHash>
#include <QMultiMap>
#include <QTime>

#include <Protos/common.pb.h>
#include <Protos/gui_protocol.pb.h>
#include <Protos/queue.pb.h>

#include <Common/Hash.h>

#include <IDownload.h>
#include <IChunkDownload.h>
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

      QList< QSharedPointer<IChunkDownload> > getTheOldestUnfinishedChunks(int n);

      static Protos::Queue::Queue loadFromFile();
      void saveToFile() const;

   private slots:
      void fileDownloadTimeChanged(QTime oldTime);

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
      void updateMarkersMove(int insertPosition, int removePosition, Download* download);

      struct Marker { Marker(DownloadPredicate* p) : predicate(p), position(0) {} DownloadPredicate* predicate; int position; };
      QList<Marker> markers; ///< Saved some positions like the first downloadable file or the first directory. The goal is to speed up the scan. See the class 'ScanningIterator'.

      QList<Download*> downloads;
      QList<Download*> erroneousDownloads;
      QMultiMap<QTime, FileDownload*> downloadsSortedByTime; // See 'FileDownload::lastTimeGetAllUnfinishedChunks'.
      QMultiHash<PM::IPeer*, Download*> downloadsIndexedBySourcePeer;
   };
}

/***** Definitions *****/
using namespace DM;

/**
  * @class DM::DownloadQueue::ScanningIterator
  *
  * To iterate over the queue for all downloads which match a predicate 'P'.
  */
template <typename P>
DownloadQueue::ScanningIterator<P>::ScanningIterator(DownloadQueue& queue) :
   queue(queue)
{
   // Search if a marker of type 'P' already exists.
   for (QMutableListIterator<Marker> i(this->queue.markers); i.hasNext();)
   {
      this->marker = &i.next();
      this->position = this->marker->position;
      if (typeid(P) == typeid(*this->marker->predicate))
         return;
   }

   // No marker found, we create a new one.
   this->queue.markers << Marker(new P);
   this->marker = &this->queue.markers.last();
   this->position = this->marker->position;
}

/**
  * @return Return 0 at the end of the list.
  */
template <typename P>
Download* DownloadQueue::ScanningIterator<P>::next()
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

#endif
