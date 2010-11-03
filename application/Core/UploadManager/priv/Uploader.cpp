#include <priv/Uploader.h>
using namespace UM;

#include <QByteArray>

#include <Common/Settings.h>

#include <Core/FileManager/Exceptions.h>
#include <Core/PeerManager/ISocket.h>

#include <priv/Constants.h>
#include <priv/Log.h>

Uploader::Uploader(QSharedPointer<FM::IChunk> chunk, int offset, QSharedPointer<PM::ISocket> socket) :
   chunk(chunk), offset(offset), socket(socket)
{
   this->mainThread = QThread::currentThread();
   this->socket->getQSocket()->moveToThread(this);
}

int Uploader::getUploadRate() const
{
   return this->transferRateCalculator.getTransferRate();
}

Common::Hash Uploader::getPeerID() const
{
   return this->socket->getPeerID();
}

int Uploader::getProgress() const
{
   return 100 * this->chunk->getKnownBytes() / this->chunk->getChunkSize();
}

QSharedPointer<FM::IChunk> Uploader::getChunk() const
{
   return this->chunk;
}

QSharedPointer<PM::ISocket> Uploader::getSocket()
{
   return this->socket;
}

void Uploader::run()
{
   L_DEBU(QString("Starting uploading a chunk from offset %1 : %2").arg(this->offset).arg(this->chunk->toStr()));

   bool networkError = false;
   int currentOffset = this->offset;

   try
   {
      QSharedPointer<FM::IDataReader> reader = this->chunk->getDataReader();

      char buffer[SETTINGS.get<quint32>("buffer_size")];
      qint64 bytesRead = 0;

      this->transferRateCalculator.reset();

      while (bytesRead = reader->read(buffer, currentOffset))
      {
         int bytesSent = socket->getQSocket()->write(buffer, bytesRead);
         if (bytesSent == -1)
         {
            L_ERRO(QString("Socket : cannot send data : %1").arg(this->chunk->toStr()));
            networkError = true;
            break;
         }

         currentOffset += bytesSent;

         // Sometimes will block when data are send between the 'bytesToWrite' call and the 'waitForBytesWritten' call.
         /*if (socket->getQSocket()->bytesToWrite() > SETTINGS.get<quint32>("socket_buffer_size"))
         {
            if (!socket->getQSocket()->waitForBytesWritten(SETTINGS.get<quint32>("socket_timeout")))
            {
               L_ERRO(QString("Socket : cannot write data, timeout, chunk : %1, error : %2").arg(this->chunk->toStr()).arg(socket->getQSocket()->errorString()));
               networkError = true;
               break;
            }
         }*/

         this->transferRateCalculator.addData(bytesSent);
      }
   }
   catch(FM::UnableToOpenFileInReadModeException&)
   {
      L_ERRO("UnableToOpenFileInReadModeException");
   }
   catch(FM::IOErrorException&)
   {
      L_ERRO("IOErrorException");
   }
   catch (FM::ChunkDeletedException)
   {
      L_ERRO("ChunkDeletedException");
   }

   /*L_WARN(QString("OMG : %1").arg(currentOffset));
   this->socket->getQSocket()->flush();
   this->usleep(2000000);*/

   this->socket->getQSocket()->moveToThread(this->mainThread);

   emit uploadFinished(networkError);
}
