#include <priv/Upload.h>
using namespace UM;

#include <Common/Settings.h>

#include <priv/Log.h>

quint64 Upload::currentID(1);

Upload::Upload(QSharedPointer<FM::IChunk> chunk, int offset, QSharedPointer<PM::ISocket> socket, Common::TransferRateCalculator& transferRateCalculator) :
   Common::Timeoutable(SETTINGS.get<quint32>("upload_lifetime")), toStop(false), ID(currentID++), chunk(chunk), offset(offset), socket(socket), transferRateCalculator(transferRateCalculator), networkError(false)
{
   this->mainThread = QThread::currentThread();
}

Upload::~Upload()
{
   this->stop();
   L_DEBU(QString("Upload#%1 deleted").arg(this->ID));
}

quint64 Upload::getID() const
{
   return this->ID;
}

Common::Hash Upload::getPeerID() const
{
   return this->socket->getRemotePeerID();
}

int Upload::getProgress() const
{
   QMutexLocker locker(&this->mutex);

   const int chunkSize = this->chunk->getChunkSize();
   if (chunkSize != 0)
      return 10000LL * this->offset / this->chunk->getChunkSize();
   else
      return 0;
}

QSharedPointer<FM::IChunk> Upload::getChunk() const
{
   return this->chunk;
}

void Upload::init(QThread* thread)
{
  this->socket->moveToThread(thread);
}

void Upload::run()
{
   L_DEBU(QString("Starting uploading a chunk from offset %1 : %2").arg(this->offset).arg(this->chunk->toStringLog()));

   static const quint32 BUFFER_SIZE = SETTINGS.get<quint32>("buffer_size_reading");
   static const quint32 SOCKET_BUFFER_SIZE = SETTINGS.get<quint32>("socket_buffer_size");
   static const quint32 SOCKET_TIMEOUT = SETTINGS.get<quint32>("socket_timeout");

   try
   {
      QSharedPointer<FM::IDataReader> reader = this->chunk->getDataReader();

      char buffer[BUFFER_SIZE];
      int bytesRead = 0;

      while (bytesRead = reader->read(buffer, this->offset))
      {
         int bytesSent = this->socket->write(buffer, bytesRead);

         if (bytesSent == -1)
         {
            L_WARN(QString("Socket : cannot send data : %1").arg(this->chunk->toStringLog()));
            this->networkError = true;
            goto end;
         }

         this->mutex.lock();
         if (this->toStop)
         {
            this->mutex.unlock();
            goto end;
         }
         this->offset += bytesSent;
         this->mutex.unlock();

         while (socket->bytesToWrite() > SOCKET_BUFFER_SIZE)
         {
            if (!socket->waitForBytesWritten(SOCKET_TIMEOUT))
            {
               L_WARN(QString("Socket : cannot write data, error : %1, chunk : %2").arg(socket->errorString()).arg(this->chunk->toStringLog()));
               this->networkError = true;
               goto end;
            }
         }

         this->transferRateCalculator.addData(bytesSent);
      }
   }
   catch(FM::UnableToOpenFileInReadModeException&)
   {
      L_WARN("UnableToOpenFileInReadModeException");
   }
   catch(FM::IOErrorException&)
   {
      L_WARN("IOErrorException");
   }
   catch (FM::ChunkDeletedException)
   {
      L_WARN("ChunkDeletedException");
   }
   catch (FM::ChunkNotCompletedException)
   {
      L_WARN("ChunkNotCompletedException");
   }

end:
   this->socket->moveToThread(this->mainThread);
}

void Upload::finished()
{
   this->socket->finished(this->networkError ? PM::ISocket::SFS_ERROR : PM::ISocket::SFS_OK);
   this->startTimer();
}

/**
  * Stop the current upload. It returns immediately.
  * Do nothing if there is no current upload.
  * See 'Upload::upload()'.
  */
void Upload::stop()
{
   this->mutex.lock();
   this->toStop = true;
   this->mutex.unlock();
}
