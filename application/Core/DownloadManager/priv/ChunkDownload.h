#ifndef DOWNLOADMANAGER_CHUNKDOWNLOAD_H
#define DOWNLOADMANAGER_CHUNKDOWNLOAD_H

#include <QSharedPointer>
#include <QList>
#include <QMutex>

#include <Common/Hash.h>
#include <Core/FileManager/IChunk.h>
#include <Core/PeerManager/IPeerManager.h>

#include <IChunkDownload.h>

#include <priv/OccupiedPeers.h>

namespace PM { class IPeer; }

namespace DM
{
   class ChunkDownload : public IChunkDownload
   {
      Q_OBJECT
   public:
      ChunkDownload(QSharedPointer<PM::IPeerManager> peerManager, OccupiedPeers& occupiedPeersDownloadingChunk, Common::Hash chunkHash);

      Common::Hash getHash();
      void setPeerIDs(const QList<Common::Hash>& peerIDs);

      void setChunk(QSharedPointer<FM::IChunk> chunk);

      void setPeerSource(PM::IPeer* peer);

      /**
        * Tell the chunkDownload to download the chunk from one of its peer.
        */
      bool downloadChunk();

   signals:
      /**
        * When it go from zero peer to one peer this signal is emitted.
        */
      void chunkReadyToDownload(ChunkDownload*);

   private:
      PM::IPeer* getFastestPeer() const;

      QSharedPointer<PM::IPeerManager> peerManager;

      OccupiedPeers& occupiedPeersDownloadingChunk;

      Common::Hash chunkHash;
      QList<PM::IPeer*> peers;

      bool complete;
      bool downloading;

      mutable QMutex mutex;
   };
}
#endif
