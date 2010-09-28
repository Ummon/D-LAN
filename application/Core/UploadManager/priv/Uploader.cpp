#include <priv/Uploader.h>
using namespace UM;

#include <QByteArray>

#include <Core/FileManager/IDataReader.h>
#include <Core/FileManager/Exceptions.h>
#include <Core/PeerManager/ISocket.h>

#include <priv/Constants.h>

Uploader::Uploader(QSharedPointer<FM::IChunk> chunk, int offset, PM::ISocket* socket)
   : chunk(chunk), offset(offset), socket(socket)
{

}

void Uploader::run()
{
   try
   {
      QSharedPointer<FM::IDataReader> reader = this->chunk->getDataReader();

      QByteArray buffer(BUFFER_SIZE, 0);
      int currentOffset = this->offset;
      qint64 bytesRead = 0;

      while (bytesRead = reader->read(buffer, currentOffset))
      {
         socket->getDevice()->write(buffer.data(), bytesRead);
         currentOffset += bytesRead;
      }
   }
   catch (FM::ChunkDeletedException)
   {
   }
   socket->finished();
}

Common::Hash Uploader::getPeerID() const
{
   return this->socket->getPeerID();
}

QSharedPointer<FM::IChunk> Uploader::getChunk() const
{
   return this->chunk;
}
