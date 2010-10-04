#ifndef FILEMANAGER_IDATAWRITER_H
#define FILEMANAGER_IDATAWRITER_H

#include <QByteArray>

namespace FM
{
   class IDataWriter
   {
   public:
      virtual ~IDataWriter() {}

      /**
        * @exception IOErrorException
        * @exception ChunkDeletedException
        * @exception TryToWriteBeyondTheEndOfChunkException
        */
      virtual bool write(const QByteArray& buffer) = 0;
   };
}

#endif
