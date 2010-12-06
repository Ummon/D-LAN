#ifndef FILEMANAGER_DATAWRITER_H
#define FILEMANAGER_DATAWRITER_H

#include <Common/Uncopyable.h>

#include <IDataWriter.h>
#include <priv/Cache/Chunk.h>

namespace FM
{
   class DataWriter : public IDataWriter, Common::Uncopyable
   {
   public:
      DataWriter(Chunk& chunk);
      ~DataWriter();

      bool write(const char* buffer, int nbBytes);

   private:
      Chunk& chunk;
   };
}

#endif
