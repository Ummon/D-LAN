#ifndef DOWNLOADMANAGER_CHUNKDOWNLOAD_H
#define DOWNLOADMANAGER_CHUNKDOWNLOAD_H

#include <QSharedPointer>

#include <Common/Hash.h>
#include <Core/FileManager/IChunk.h>

#include <IChunkDownload.h>

namespace PM { class IPeer; }

namespace DM
{
   class ChunkDownload : public IChunkDownload
   {
      virtual Common::Hash getHash();
      virtual void setPeerIDs(const QList<Common::Hash>& peerIDs);

   private:
      QSharedPointer<FM::IChunk> chunk;
      PM::IPeer* peer;
   };
}
#endif
