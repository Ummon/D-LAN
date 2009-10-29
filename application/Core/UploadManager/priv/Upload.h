#ifndef UPLOADMANAGER_UPLOAD_H
#define UPLOADMANAGER_UPLOAD_H

#include <IUpload.h>

namespace PM { class IPeer; }
namespace FM { class IChunk; }

namespace UM
{
   class Upload : public IUpload
   {
   private:
      PM::IPeer* peer;
      FM::IChunk* chunk;
   };
}
#endif
