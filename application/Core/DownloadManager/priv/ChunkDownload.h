#ifndef DOWNLOADMANAGER_CHUNKDOWNLOAD_H
#define DOWNLOADMANAGER_CHUNKDOWNLOAD_H

#include <IChunkDownload.h>

namespace FileManager { class IChunk; } 
namespace PeerManager { class IPeer; } 

namespace DownloadManager
{  
   class ChunkDownload : public IChunkDownload
   {
   private:
      FileManager::IChunk* chunk; 
      PeerManager::IPeer* peer;
   };
}
#endif
