#ifndef UPLOADMANAGER_IUPLOAD_H
#define UPLOADMANAGER_IUPLOAD_H

#include <QSharedPointer>

#include <Common/Hash.h>

namespace UM
{
   class IPeer;
   class IChunk;

   class IUpload
   {
   public:
      virtual ~IUpload() {}

      virtual Common::Hash getPeerID() const = 0;
      virtual QSharedPointer<FM::IChunk> getChunk() const = 0;
   };
}
#endif
