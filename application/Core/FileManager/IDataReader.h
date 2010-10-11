#ifndef FILEMANAGER_IDATAREADER_H
#define FILEMANAGER_IDATAREADER_H

#include <QtGlobal>

namespace FM
{
   class IDataReader
   {
   public:

      virtual ~IDataReader() {}

      /**
        * @exception IOErrorException
        * @exception ChunkDeletedException
        * @exception ChunkNotCompletedException
        */
      virtual quint64 read(char* buffer, uint offset) = 0;
   };
}

#endif
