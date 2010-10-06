#ifndef DOWNLOADMANAGER_CHUNKDOWNLOAD_H
#define DOWNLOADMANAGER_CHUNKDOWNLOAD_H

#include <QSharedPointer>
#include <QList>
#include <QMutex>

#include <Common/Hash.h>
#include <Core/FileManager/IChunk.h>
#include <Core/PeerManager/IPeerManager.h>

#include <IChunkDownload.h>

namespace PM { class IPeer; }

namespace DM
{
   class ChunkDownload : public IChunkDownload
   {
      Q_OBJECT
   public:
      ChunkDownload(QSharedPointer<PM::IPeerManager> peerManager, QSharedPointer<FM::IChunk> chunk, PM::IPeer* peerSource);
      Common::Hash getHash();
      void setPeerIDs(const QList<Common::Hash>& peerIDs);

      bool setAsDownloading();
      void releaseDownloading();

   signals:
      /**
        * When it go from zero peer to one peer this signal is emitted.
        */
      void chunkReadyToDownload(ChunkDownload*);

   private:
      QSharedPointer<PM::IPeerManager> peerManager;

      QSharedPointer<FM::IChunk> chunk;
      QList<PM::IPeer*> peers;

      bool complete;
      bool downloading;

      QMutex mutex;
   };
}
#endif
