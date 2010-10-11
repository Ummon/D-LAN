#ifndef FILEMANAGER_DATAREADER_H
#define FILEMANAGER_DATAREADER_H

#include <QByteArray>
#include <QThread>
#include <QMutex>
#include <QWaitCondition>

#include <IDataReader.h>
#include <priv/Cache/Chunk.h>

namespace FM
{
   class DataReader : virtual public QThread, public IDataReader
   {
   public:
      DataReader(Chunk& chunk);
      ~DataReader();

      void read(uint offset);

   protected:
      void run();

   private:
      Chunk& chunk;
      uint offset;
      const int BUFFER_SIZE;
      char* buffer;

      QWaitCondition waitCondition;
      QMutex mutex;
      bool continueReading;
      bool quit;
   };
}

#endif
