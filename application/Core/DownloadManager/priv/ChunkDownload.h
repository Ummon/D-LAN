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
  
#ifndef DOWNLOADMANAGER_CHUNKDOWNLOAD_H
#define DOWNLOADMANAGER_CHUNKDOWNLOAD_H

#include <QSharedPointer>
#include <QList>
#include <QThread>
#include <QElapsedTimer>

#include <Protos/core_protocol.pb.h>

#include <Common/TransferRateCalculator.h>
#include <Common/Hash.h>
#include <Common/Uncopyable.h>
#include <Common/IRunnable.h>
#include <Common/ThreadPool.h>
#include <Core/FileManager/IChunk.h>
#include <Core/FileManager/IDataWriter.h>
#include <Core/PeerManager/IPeer.h>
#include <Core/PeerManager/IGetChunkResult.h>

#include <IChunkDownload.h>
#include <IDownload.h>

#include <priv/OccupiedPeers.h>
#include <priv/LinkedPeers.h>

namespace PM { class IPeer; }

namespace DM
{
   class ChunkDownload : public QObject, public Common::IRunnable, public IChunkDownload, Common::Uncopyable
   {
      static const int MINIMUM_DELTA_TIME_TO_COMPUTE_SPEED;

      Q_OBJECT
   public:
      ChunkDownload(LinkedPeers& linkedPeers, OccupiedPeers& occupiedPeersDownloadingChunk, Common::TransferRateCalculator& transferRateCalculator, Common::ThreadPool& threadPool, Common::Hash chunkHash);
      ~ChunkDownload();

      void stop();

      Common::Hash getHash() const;

      void addPeer(PM::IPeer* peer);
      void rmPeer(PM::IPeer* peer);

      void init(QThread* thread);
      void run();
      void finished();

      void setChunk(QSharedPointer<FM::IChunk> chunk);
      QSharedPointer<FM::IChunk> getChunk() const;

      void setPeerSource(PM::IPeer* peer, bool informOccupiedPeers = true);

      int isReadyToDownload();
      bool isDownloading() const;
      bool isComplete() const;
      bool isPartiallyDownloaded() const;
      bool hasAtLeastAPeer();
      Status getLastTransfertStatus() const;
      void resetLastTransfertStatus();

      int getDownloadedBytes() const;
      QList<Common::Hash> getPeers();

      PM::IPeer* startDownloading();
      void tryToRemoveItsIncompleteFile();
      void reset();

   signals:
      void downloadStarted();
      /**
        * Emitted when a downlad is terminated (or aborted).
        */
      void downloadFinished();
      void numberOfPeersChanged();

   private slots:
      void result(const Protos::Core::GetChunkResult& result);
      void stream(QSharedPointer<PM::ISocket> socket);
      void getChunkTimeout();

      void downloadingEnded();

   private:
      PM::IPeer* getTheFastestFreePeer();
      int getNumberOfFreePeer();

      LinkedPeers& linkedPeers;
      OccupiedPeers& occupiedPeersDownloadingChunk; // The peers from where we downloading.
      Common::TransferRateCalculator& transferRateCalculator;
      Common::ThreadPool& threadPool;

      Common::Hash chunkHash;
      QSharedPointer<FM::IChunk> chunk;

      QList<PM::IPeer*> peers; // The peers which own this chunk.
      PM::IPeer* currentDownloadingPeer;

      QSharedPointer<PM::ISocket> socket;

      int chunkSize;
      QSharedPointer<PM::IGetChunkResult> getChunkResult;

      bool downloading;
      bool closeTheSocket;
      Status lastTransfertStatus;

      QThread* mainThread;

      mutable QMutex mutex; // To protect 'peers' and 'downloading'.
   };
}
#endif
