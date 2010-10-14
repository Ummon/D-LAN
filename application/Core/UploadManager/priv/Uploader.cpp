#include <priv/Uploader.h>
using namespace UM;

#include <QByteArray>

#include <Common/Settings.h>

#include <Core/FileManager/Exceptions.h>
#include <Core/PeerManager/ISocket.h>

#include <priv/Constants.h>
#include <priv/Log.h>

Uploader::Uploader(QSharedPointer<FM::IChunk> chunk, int offset, PM::ISocket* socket)
   : chunk(chunk), offset(offset), socket(socket)
{
   this->mainThread = QThread::currentThread();
   this->socket->getQSocket()->moveToThread(this);
}

Common::Hash Uploader::getPeerID() const
{
   return this->socket->getPeerID();
}

QSharedPointer<FM::IChunk> Uploader::getChunk() const
{
   return this->chunk;
}

PM::ISocket* Uploader::getSocket()
{
   return this->socket;
}

void Uploader::run()
{
   L_DEBU(QString("Starting uploading a chunk : %1").arg(this->chunk->toStr()));

   bool networkError = false;

   try
   {
      QSharedPointer<FM::IDataReader> reader = this->chunk->getDataReader();

      char buffer[SETTINGS.get<quint32>("buffer_size")];
      int currentOffset = this->offset;
      qint64 bytesRead = 0;

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

         if (socket->getQSocket()->bytesToWrite() > SETTINGS.get<quint32>("socket_buffer_size"))
         {
            if (!socket->getQSocket()->waitForBytesWritten(SETTINGS.get<quint32>("timeout_during_transfert")))
            {
               L_ERRO(QString("Socket : cannot write data : %1").arg(this->chunk->toStr()));
               networkError = true;
               break;
            }
         }
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
   this->socket->getQSocket()->moveToThread(this->mainThread);

   emit uploadFinished(networkError);
}
