#ifndef FILEMANAGER_DATAWRITER_H
#define FILEMANAGER_DATAWRITER_H

#include <QThread>
#include <QMutex>
#include <QWaitCondition>

#include <IDataWriter.h>
#include <priv/Cache/Chunk.h>

namespace FM
{
   class DataWriter : virtual public QThread, public IDataWriter
   {
   public:
      DataWriter(Chunk& chunk);
      ~DataWriter();

      char* getBuffer();
      int getBufferSize() const;

      void write(int nbBytes);

   protected:
      void run();

   private:
      Chunk& chunk;
      int nbBytes;
      const int BUFFER_SIZE;
      char* buffer;

      QWaitCondition waitCondition;
      QMutex mutex;
      bool continueWriting;
      bool quit;
   };
}

#endif
