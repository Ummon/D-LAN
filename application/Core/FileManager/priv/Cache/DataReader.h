#ifndef FILEMANAGER_DATAREADER_H
#define FILEMANAGER_DATAREADER_H

#include <Common/Uncopyable.h>

#include <IDataReader.h>
#include <priv/Cache/Chunk.h>

namespace FM
{
   class DataReader : public IDataReader, Common::Uncopyable
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
