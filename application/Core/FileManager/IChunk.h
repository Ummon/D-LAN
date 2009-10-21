#ifndef FILEMANAGER_ICHUNK_H
#define FILEMANAGER_ICHUNK_H

#include <QSharedPointer>

#include <Common/Hash.h>

namespace FM
{
   class IDataReader;
   class IDataWriter;

   class IChunk
   {
   public:
      virtual ~IChunk() {}

      virtual QSharedPointer<IDataReader> getDataReader() = 0;
      virtual QSharedPointer<IDataWriter> getDataWriter() = 0;

      virtual Common::Hash getHash() = 0;
   };
}
#endif
