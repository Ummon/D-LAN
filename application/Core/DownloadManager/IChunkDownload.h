#ifndef DOWNLOADMANAGER_ICHUNKDOWNLOAD_H
#define DOWNLOADMANAGER_ICHUNKDOWNLOAD_H

#include <QtGlobal>

#include <Common/Hash.h>

namespace DM
{
   /**
     * A chunk which one or more known peer have.
     * The chunk is not or partially downloaded.
     * Once a chunk is terminated, the IChunkDownload is deleted.
     */
   class IChunkDownload
   {
   public:
      virtual ~IChunkDownload() {}

      /**
        * Gets the hash of the associated chunk.
        */
      virtual Common::Hash getHash() = 0;

      /**
        * Defines the peers which have the chunk.
        */
      virtual void setPeerIDs(const QList<quint32>& peerIDs) = 0;
   };
}
#endif
