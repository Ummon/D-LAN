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
   class DownloadQueue;

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
         Protos::Queue::Queue::Entry::Status status = Protos::Queue::Queue::Entry::QUEUED,
         DownloadQueue* downloadQueue = nullptr
      );
      ~FileDownload() override;

      void start() override;
      void stop();

      bool pause(bool pause) override;

      void peerSourceBecomesAvailable() override;

      void populateQueueEntry(Protos::Queue::Queue::Entry* entry) const override;

      quint64 getDownloadedBytes() const override;
      QSet<PM::IPeer*> getPeers() const override;

      QSharedPointer<ChunkDownloader> getAChunkToDownload();

      void getUnfinishedChunks(QList<QSharedPointer<IChunkDownloader>>& chunks, int nMax, bool notAlreadyAsked = true);

      inline qint64 getLastTimeGetAllUnfinishedChunks() const;

      void remove() override;

      bool needsHashingHint() const;
      bool canBeGivenAsNextFileToHash() const;

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
      bool hasHashesToRetrieve() const;
      void addHash(const Protos::Core::HashResult& hashResult);
      void unableToRetrieveTheHashes();
      bool tryToLinkToAnExistingFile();
      QList<QSharedPointer<FM::IChunk>> getChunksOfTheExistingFile(const QList<Common::Hash>& hashes);
      QSharedPointer<ChunkDownloader> createChunkDownloader(const Common::Hash& hash);
      bool createFile();
      bool prepareFileForResume();
      void giveChunksToDownloaders();
      void reset();
      void releaseChunkDownloaders();

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
      quint32 nbHashesExpected = 0; // Hashes the pending request still has to give, as announced by the peer source. 0 if not known.

      DownloadQueue* downloadQueue; // To give the next files to hash to the peer source, may be null.
      bool givenAsNextFileToHash = false; // Not persisted, see 'canBeGivenAsNextFileToHash()'.

      Common::TransferRateCalculator& transferRateCalculator;

      qint64 lastTimeGetAllUnfinishedChunks; // [ms] since epoch. Updated when ALL hashes are send via the method 'getUnfinishedChunks(..)'. 0 if never.

      // There can be a lot of downloads: a flag and a posted call instead of a 'QTimer' per download, see 'scheduleStatusUpdate()'.
      bool statusUpdatePending = false;

      // When the peer source doesn't have the file, it's asked again periodically. Created on the first need, rarely used.
      QTimer* retryToGetHashesTimer = nullptr;
   };
}

inline qint64 DM::FileDownload::getLastTimeGetAllUnfinishedChunks() const
{
   return this->lastTimeGetAllUnfinishedChunks;
}
