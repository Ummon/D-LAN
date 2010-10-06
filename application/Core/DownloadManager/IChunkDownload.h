#ifndef DOWNLOADMANAGER_ICHUNKDOWNLOAD_H
#define DOWNLOADMANAGER_ICHUNKDOWNLOAD_H

#include <QObject>

#include <Common/Hash.h>

namespace DM
{
   /**
     * Once a chunk is downloader, the IChunkDownload is deleted.
     */
   class IChunkDownload : public QObject
   {
   public:
      virtual ~IChunkDownload() {}

      /**
        * Gets the hash of the associated chunk.
        */
      virtual Common::Hash getHash() = 0;

      /**
        * Define (or redefine) the peers which have the chunk.
        */
      virtual void setPeerIDs(const QList<Common::Hash>& peerIDs) = 0;
   };
}
#endif
