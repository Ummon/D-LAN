#ifndef FILEMANAGER_DATAREADER_H
#define FILEMANAGER_DATAREADER_H

#include <IDataReader.h>
#include <priv/Cache/Chunk.h>

namespace FM
{
   class DataReader : public IDataReader
   {
   public:
      DataReader(Chunk& chunk);
      ~DataReader();

      quint64 read(char* buffer, uint offset);

   protected:
      void run();

   private:
      Chunk& chunk;
   };
}

#endif
