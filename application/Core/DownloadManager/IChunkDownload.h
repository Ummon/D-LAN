#ifndef DOWNLOADMANAGER_ICHUNKDOWNLOAD_H
#define DOWNLOADMANAGER_ICHUNKDOWNLOAD_H

#include <QObject>

#include <Common/Hash.h>

namespace DM
{
   /**
     * Once a chunk is downloader, the IChunkDownload is deleted.
     */
   class IChunkDownload
   {
   public:
      virtual ~IChunkDownload() {}

      /**
        * Gets the hash of the associated chunk.
        */
      virtual Common::Hash getHash() const = 0;

      /**
        * Define (or redefine) the peers which have the chunk.
        */
      virtual void addPeerID(const Common::Hash& peerID) = 0;
      virtual void rmPeerID(const Common::Hash& peerID) = 0;
   };
}
#endif
