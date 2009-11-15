#ifndef DOWNLOADMANAGER_CHUNKDOWNLOAD_H
#define DOWNLOADMANAGER_CHUNKDOWNLOAD_H

#include <IChunkDownload.h>

namespace FM { class IChunk; }
namespace PM { class IPeer; }

namespace DM
{
   /**
     * /!\ The 'ChunkDownload' objects are in a download thread, see 'ChunkDownloader'.
     */
   class ChunkDownload : public IChunkDownload
   {
   private:
      FM::IChunk* chunk;
      PM::IPeer* peer;
   };
}
#endif
