#include <priv/Cache/DataWriter.h>
using namespace FM;

#include <QMetaType>

#include <Common/Settings.h>

#include <priv/Log.h>
#include <Exceptions.h>

DataWriter::DataWriter(Chunk& chunk)
   : chunk(chunk), BUFFER_SIZE(SETTINGS.getUInt32("buffer_size"))
{
   qRegisterMetaType<FM::IDataWriter::Status>("FM::IDataWriter::Status");

   this->buffer = new char[BUFFER_SIZE];
   this->chunk.newDataWriterCreated();
   this->start();
}

char* DataWriter::getBuffer()
{
   return this->buffer;
}

int DataWriter::getBufferSize() const
{
   return this->BUFFER_SIZE;
}

DataWriter::~DataWriter()
{
   this->chunk.dataWriterDeleted();
   delete[] this->buffer;

   this->mutex.lock();
   this->quit = true;
   this->continueWriting = true;
   this->waitCondition.wakeOne();
   this->mutex.unlock();
   this->wait();
}

void DataWriter::write(int nbBytes)
{
   this->nbBytes = nbBytes;

   this->mutex.lock();
   this->continueWriting = true;
   this->waitCondition.wakeOne();
   this->mutex.unlock();
}

void DataWriter::run()
{
   forever
   {
      this->mutex.lock();
      if (!this->continueWriting)
         this->waitCondition.wait(&this->mutex);
      this->continueWriting = false;
      if (this->quit)
      {
         this->mutex.unlock();
         return;
      }
      this->mutex.unlock();

      try
      {
         bool endOfTheChunk = this->chunk.write(this->buffer, this->nbBytes);
         emit writeFinished(endOfTheChunk ? END_OF_CHUNK : NOT_FINISHED);
      }
      catch(IOErrorException&)
      {
         L_ERRO("IOErrorException");
         emit writeFinished(IO_ERROR);
      }
      catch (ChunkDeletedException&)
      {
         L_ERRO("ChunkDeletedException");
         emit writeFinished(IO_ERROR);
      }
      catch (TryToWriteBeyondTheEndOfChunkException&)
      {
         L_ERRO("TryToWriteBeyondTheEndOfChunkException");
         emit writeFinished(IO_ERROR);
      }
   }
}
