#ifndef DOWNLOADMANAGER_CHUNKDOWNLOAD_H
#define DOWNLOADMANAGER_CHUNKDOWNLOAD_H

#include <QSharedPointer>
#include <QList>
#include <QThread>

#include <Protos/core_protocol.pb.h>

#include <Common/Hash.h>
#include <Core/FileManager/IChunk.h>
#include <Core/FileManager/IDataWriter.h>
#include <Core/PeerManager/IPeerManager.h>
#include <Core/PeerManager/IGetChunkResult.h>

#include <IChunkDownload.h>

#include <priv/OccupiedPeers.h>

namespace PM { class IPeer; }

namespace DM
{
   class ChunkDownload : public QThread, public IChunkDownload
   {
      Q_OBJECT
   public:
      ChunkDownload(QSharedPointer<PM::IPeerManager> peerManager, OccupiedPeers& occupiedPeersDownloadingChunk, Common::Hash chunkHash);

      Common::Hash getHash();
      void addPeerID(const Common::Hash& peerID);
      void rmPeerID(const Common::Hash& peerID);

      void setChunk(QSharedPointer<FM::IChunk> chunk);

      void setPeerSource(PM::IPeer* peer);

      bool isReadyToDownload();

      void startDownloading();

   signals:
      /**
        * Emitted when a downlad is terminated (or aborted).
        */
      void downloadFinished();

   protected:
      void run();

   private slots:
      void result(const Protos::Core::GetChunkResult& result);
      void stream(PM::ISocket* socket);

      void downloadingEnded();

   private:
      PM::IPeer* getTheFastestFreePeer() const;

      QSharedPointer<PM::IPeerManager> peerManager; // To retrieve the peers from their ID.

      OccupiedPeers& occupiedPeersDownloadingChunk; // The peers from where we downloading.

      Common::Hash chunkHash;
      QSharedPointer<FM::IChunk> chunk;

      QList<PM::IPeer*> peers; // The peers which own this chunk.
      PM::IPeer* currentDownloadingPeer;

      PM::ISocket* socket;

      QThread* mainThread; // Only use to move the socket from and to the main thread.

      int chunkSize;
      QSharedPointer<PM::IGetChunkResult> getChunkResult;

      bool downloading;
      bool networkError;

      mutable QMutex mutex; // To protect 'peers'.
   };
}
#endif
