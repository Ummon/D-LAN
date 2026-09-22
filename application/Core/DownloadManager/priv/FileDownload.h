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

#include <QList>
#include <QMap>
#include <QSharedPointer>
#include <QTimer>

#include <Common/ThreadPool.h>

#include <Core/FileManager/IChunk.h>
#include <Core/PeerManager/IPeerManager.h>
#include <Core/PeerManager/IGetHashesResult.h>

#include <Protos/common.pb.h>

#include <priv/OccupiedPeers.h>
#include <priv/LinkedPeers.h>
#include <priv/Download.h>
#include <priv/ChunkDownloader.h>

namespace DM
{
   class FileDownload : public Download
   {
      Q_OBJECT

   public:
      FileDownload(
         QSharedPointer<FM::IFileManager> fileManager,
         LinkedPeers& linkedPeers,
         OccupiedPeers& occupiedPeersAskingForHashes,
         OccupiedPeers& occupiedPeersDownloadingChunk,
         Common::ThreadPool& threadPool,
         PM::IPeer* peerSource,
         const Protos::Common::Entry& remoteEntry,
         const Protos::Common::Entry& localEntry,
         Common::TransferRateCalculator& transferRateCalculator,
         Protos::Queue::Queue::Entry::Status status = Protos::Queue::Queue::Entry::QUEUED
      );
      ~FileDownload() override;

      void start() override;
      void stop() override;

      bool pause(bool pause, bool stopTransfers = true) override;

      void peerSourceBecomesAvailable() override;

      void populateQueueEntry(Protos::Queue::Queue::Entry* entry) const override;

      quint64 getDownloadedBytes() const override;
      QSet<PM::IPeer*> getPeers() const override;

      QSharedPointer<ChunkDownloader> getAChunkToDownload();

      void getUnfinishedChunks(QList<QSharedPointer<IChunkDownloader>>& chunks, int nMax, bool notAlreadyAsked = true);

      inline qint64 getLastTimeGetAllUnfinishedChunks() const;

      void remove() override;

   public slots:
      bool retrieveHashes();

   signals:
      void newHashKnown();
      void lastTimeGetAllUnfinishedChunksChanged(qint64 oldTime);

   protected:
      void setStatus(Protos::Common::DownloadStatus status) override;

   private slots:
      bool updateStatus() override;
      void scheduleStatusUpdate();
      void result(const Protos::Core::GetHashesResult& result);
      void nextHash(const Protos::Core::HashResult&);
      void getHashTimeout();
      void retryToGetHashes();

      void chunkDownloaderStarted();
      void chunkDownloaderFinished();

   private:
      bool tryToLinkToAnExistingFile();
      void connectChunkDownloaderSignals(const QSharedPointer<ChunkDownloader>& chunkDownload);
      bool createFile();
      bool prepareFileForResume();
      void giveChunksToDownloaders();
      void reset();

      LinkedPeers& linkedPeers;

      const int NB_CHUNK;

      // Chunks without downloader associated.
      QMap<int, QSharedPointer<FM::IChunk>> chunksWithoutDownloader;
      QList<QSharedPointer<ChunkDownloader>> chunkDownloaders;

      int nbChunkAsked; // Position in 'chunkDownloaders' of the next chunk to give in 'getUnfinishedChunks(..)'.

      OccupiedPeers& occupiedPeersAskingForHashes;
      OccupiedPeers& occupiedPeersDownloadingChunk;

      Common::ThreadPool& threadPool;

      int nbHashesKnown;
      QSharedPointer<PM::IGetHashesResult> getHashesResult;

      Common::TransferRateCalculator& transferRateCalculator;

      qint64 lastTimeGetAllUnfinishedChunks; // [ms] since epoch. Updated when ALL hashes are send via the method 'getUnfinishedChunks(..)'. 0 if never.
      QTimer statusUpdateTimer;
      QTimer retryToGetHashesTimer; // When the peer source doesn't have the file, it's asked again periodically.
   };
}

inline qint64 DM::FileDownload::getLastTimeGetAllUnfinishedChunks() const
{
   return this->lastTimeGetAllUnfinishedChunks;
}
