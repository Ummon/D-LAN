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
  
#ifndef DOWNLOADMANAGER_DOWNLOADMANAGER_H
#define DOWNLOADMANAGER_DOWNLOADMANAGER_H

#include <QList>
#include <QSet>
#include <QSharedPointer>
#include <QTimer>
#include <QMultiHash>

#include <Common/TransferRateCalculator.h>
#include <Common/ThreadPool.h>

#include <Core/FileManager/IFileManager.h>
#include <Core/PeerManager/IPeerManager.h>

#include <IDownloadManager.h>
#include <priv/DownloadQueue.h>
#include <priv/DownloadPredicate.h>
#include <priv/OccupiedPeers.h>
#include <priv/LinkedPeers.h>
#include <priv/Log.h>

namespace PM
{
   class IPeer;
}

namespace DM
{
   class Download;
   class FileDownload;

   class DownloadManager : public QObject, public IDownloadManager
   {
      Q_OBJECT
   public:
      DownloadManager(QSharedPointer<FM::IFileManager> fileManager, QSharedPointer<PM::IPeerManager> peerManager);
      ~DownloadManager();

      void addDownload(const Protos::Common::Entry& remoteEntry, PM::IPeer* peerSource);
      void addDownload(const Protos::Common::Entry& remoteEntry, PM::IPeer* peerSource, const Common::Hash& destinationDirectoryID, const QString& relativePath);
      void addDownload(const Protos::Common::Entry& remoteEntry, PM::IPeer* peerSource, const QString& absolutePath);

      Download* addDownload(const Protos::Common::Entry& remoteEntry, PM::IPeer* peerSource, const Common::Hash& destinationDirectoryID, const QString& localRelativePath, Protos::Queue::Queue::Entry::Status status);
      Download* addDownload(const Protos::Common::Entry& remoteEntry, PM::IPeer* peerSource, const Common::Hash& destinationDirectoryID, const QString& localRelativePath, Protos::Queue::Queue::Entry::Status status, int position);

      Download* addDownload(const Protos::Common::Entry& remoteEntry, const Protos::Common::Entry& localEntry, PM::IPeer* peerSource, Protos::Queue::Queue::Entry::Status status);
      Download* addDownload(const Protos::Common::Entry& remoteEntry, const Protos::Common::Entry& localEntry, PM::IPeer* peerSource, Protos::Queue::Queue::Entry::Status status, int position);

      QList<IDownload*> getDownloads() const;
      void moveDownloads(const QList<quint64>& downloadIDRefs, const QList<quint64>& downloadIDs, Protos::GUI::MoveDownloads::Position position);

      void removeAllCompleteDownloads();
      void removeDownloads(QList<quint64> IDs);

      void pauseDownloads(QList<quint64> IDs, bool pause = true);

      QList< QSharedPointer<IChunkDownload> > getTheFirstUnfinishedChunks(int n);
      QList< QSharedPointer<IChunkDownload> > getTheOldestUnfinishedChunks(int n);

      int getDownloadRate();

   private slots:
      void peerBecomesAvailable(PM::IPeer* peer);

      void fileCacheLoaded();

      void newEntries(const Protos::Common::Entries& remoteEntries);

      void peerNoLongerAskingForHashes(PM::IPeer* peer);
      void peerNoLongerAskingForEntries(PM::IPeer* peer);
      void peerNoLongerDownloadingChunk(PM::IPeer* peer);

      void scanTheQueue();
      void rescanTimerActivated();
      void chunkDownloadFinished();
      void downloadStatusBecomeErroneous(Download* download);

   private:
      void loadQueueFromFile();

   private slots:
      void saveQueueToFile();
      void setQueueChanged();

   private:
      LOG_INIT_H("DownloadManager");

      const int NUMBER_OF_DOWNLOADER;

      QSharedPointer<FM::IFileManager> fileManager;
      QSharedPointer<PM::IPeerManager> peerManager;
      LinkedPeers linkedPeers; // Number of 'ChunkDownload' each peer owns.

      Common::TransferRateCalculator transferRateCalculator;

      OccupiedPeers occupiedPeersAskingForHashes;
      OccupiedPeers occupiedPeersAskingForEntries;
      OccupiedPeers occupiedPeersDownloadingChunk;

      Common::ThreadPool threadPool;

      DownloadQueue downloadQueue;

      int numberOfDownloadThreadRunning;

      QTimer rescanTimer; // When a download has an error status, the queue will be rescaned periodically.

      QTimer saveTimer; // To know when to save the queue, for exemple each 5min.
      bool queueChanged;
   };
}
#endif
