#ifndef UPLOADMANAGER_UPLOAD_H
#define UPLOADMANAGER_UPLOAD_H

#include <IUpload.h>

namespace PeerManager { class IPeer; }
namespace FileManager { class IChunk; }

namespace UploadManager
{
   class Upload : public IUpload
   {
   private:
      PeerManager::IPeer* peer;
      FileManager::IChunk* chunk;
   };
}
#endif
