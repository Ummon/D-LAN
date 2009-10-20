#ifndef FILEMANAGER_DATAREADER_H
#define FILEMANAGER_DATAREADER_H

#include <QByteArray>

#include <IDataReader.h>
#include <priv/Cache/Chunk.h>

namespace FM
{
   class DataReader : public IDataReader
   {
   public:
      DataReader(Chunk& chunk);
      ~DataReader();

      qint64 read(QByteArray& buffer, uint offset);

   private:
      Chunk& chunk;
   };
}

#endif
