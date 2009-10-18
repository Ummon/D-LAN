#ifndef FILEMANAGER_DATAWRITER_H
#define FILEMANAGER_DATAWRITER_H

#include <IDataWriter.h>
#include <priv/Cache/Chunk.h>

namespace FileManager
{
   class DataWriter : public IDataWriter
   {
   public:
      DataWriter(Chunk& chunk);
      ~DataWriter();

      bool write(const QByteArray& buffer, uint offset);

   private:
      Chunk& chunk;
   };
}

#endif
