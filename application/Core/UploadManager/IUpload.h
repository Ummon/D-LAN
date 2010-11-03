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

      virtual quint64 getID() const = 0;

      virtual Common::Hash getPeerID() const = 0;

      /**
        * Return a value between 0 and 100.
        */
      virtual int getProgress() const = 0;

      virtual QSharedPointer<FM::IChunk> getChunk() const = 0;
   };
}
#endif
