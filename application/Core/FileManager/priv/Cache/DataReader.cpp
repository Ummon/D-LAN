#include <priv/Cache/DataReader.h>
using namespace FM;

#include <Common/Settings.h>

#include <priv/Log.h>
#include <Exceptions.h>

DataReader::DataReader(Chunk& chunk)
   : chunk(chunk), BUFFER_SIZE(SETTINGS.getUInt32("buffer_size")), continueReading(false), quit(false)
{
   this->buffer = new char[BUFFER_SIZE];
   this->chunk.newDataReaderCreated();
   this->start();
}

DataReader::~DataReader()
{
   this->chunk.dataReaderDeleted();
   delete[] this->buffer;

   this->mutex.lock();
   this->quit = true;
   this->continueReading = true;
   this->waitCondition.wakeOne();
   this->mutex.unlock();
   this->wait();
}

void DataReader::read(uint offset)
{
   this->offset = offset;

   this->mutex.lock();
   this->continueReading = true;
   this->waitCondition.wakeOne();
   this->mutex.unlock();
}

void DataReader::run()
{
   forever
   {
      this->mutex.lock();
      if (!this->continueReading)
         this->waitCondition.wait(&this->mutex);
      this->continueReading = false;
      if (this->quit)
      {
         this->mutex.unlock();
         return;
      }
      this->mutex.unlock();

      try
      {
         int nbByteRead = this->chunk.read(this->buffer, this->BUFFER_SIZE, this->offset);
         emit readFinished(this->buffer, nbByteRead);
      }
      catch(IOErrorException&)
      {
         L_ERRO("IOErrorException");
         emit readFinished(this->buffer, -1);
      }
      catch (ChunkDeletedException&)
      {
         L_ERRO("ChunkDeletedException");
         emit readFinished(this->buffer, -1);
      }
      catch(ChunkNotCompletedException)
      {
         L_ERRO("ChunkNotCompletedException");
         emit readFinished(this->buffer, -1);
      }
   }
}

