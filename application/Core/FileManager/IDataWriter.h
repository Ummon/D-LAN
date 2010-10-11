#ifndef FILEMANAGER_IDATAWRITER_H
#define FILEMANAGER_IDATAWRITER_H

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
      virtual bool write(const char* buffer, int nbBytes) = 0;
   };
}

#endif
