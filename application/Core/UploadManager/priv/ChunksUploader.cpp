/**
  * D-LAN - A decentralized LAN file sharing software.
  * Copyright (C) 2010-2012 Greg Burri <greg.burri@gmail.com>
  *
  * This program is free software: you can redistribute it and/or modify
  * it under the terms of the GNU General Public License as published by
  * the Free Software Foundation, either version 3 of the License, or
  * (at your option) any later version.
  *
  * This program is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  *
  * You should have received a copy of the GNU General Public License
  * along with this program.  If not, see <http://www.gnu.org/licenses/>.
  */

#include <priv/ChunksUploader.h>
using namespace UM;

#include <QCoreApplication>

#include <Common/Settings.h>

#include <priv/Log.h>

/**
  * Un chunk uploader will write a given chunk to a given socket.
  * This operation is threaded and must be run by a 'Common::ThreadPool'.
  */

quint64 ChunksUploader::currentID(1);

ChunksUploader::ChunksUploader(
   QList<std::pair<QSharedPointer<FM::IChunk>, int>> chunksAndOffsets,
   const QSharedPointer<PM::ISocket>& socket,
   Common::TransferRateCalculator& transferRateCalculator
) :
   Common::Timeoutable(SETTINGS.get<quint32>("upload_lifetime")),
   mainThread(QThread::currentThread()),
   ID(currentID++),
   chunks(chunksAndOffsets),
   socket(socket),
   transferRateCalculator(transferRateCalculator),
   closeTheSocket(false),
   toStop(false)
{
}

ChunksUploader::~ChunksUploader()
{
   this->stop();
   L_DEBU(QString("Upload#%1 deleted").arg(this->ID));
}

quint64 ChunksUploader::getID() const
{
   return this->ID;
}

Common::Hash ChunksUploader::getPeerID() const
{
   return this->socket->getRemotePeerID();
}

int ChunksUploader::getProgress() const
{
   QMutexLocker locker(&this->mutex);

   Q_ASSERT(!this->chunks.isEmpty());

   int totalOffset = 0;
   for (const auto& chunk : this->chunks)
   {
      const int chunkSize = chunk.first->getChunkSize();
      if (chunkSize > 0)
         totalOffset += 10000LL * chunk.second / chunkSize;
   }

   return totalOffset / this->chunks.size();

   // const int chunkSize = this->chunk->getChunkSize();
   // if (chunkSize != 0)
   //    return 10000LL * this->offset / chunkSize;
   // else
   //    return 0;
}

QList<QSharedPointer<FM::IChunk>> ChunksUploader::getChunks() const
{
   // TODO: Offsets needed?
   QList<QSharedPointer<FM::IChunk>> result;
   for (auto chunk : this->chunks)
      result << chunk.first;
   return result;
}

void ChunksUploader::init(QThread* thread)
{
  this->socket->moveToThread(thread);
}

/**
  * Called by the thread pool ('Common::ThreadPool') in another thread.
  */
void ChunksUploader::run()
{
   static const quint32 BUFFER_SIZE = SETTINGS.get<quint32>("buffer_size_reading");
   static const quint32 SOCKET_BUFFER_SIZE = SETTINGS.get<quint32>("socket_buffer_size");
   static const quint32 SOCKET_TIMEOUT = SETTINGS.get<quint32>("socket_timeout");

   try
   {
      for (auto& chunk : this->chunks)
      {
         L_DEBU(
            QString("Starting uploading a chunk from offset %1: %2")
               .arg(chunk.second)
               .arg(chunk.first->toStringLog()
            )
         );

         QSharedPointer<FM::IDataReader> reader = chunk.first->getDataReader();

         QByteArray buffer(BUFFER_SIZE, Qt::Uninitialized);
         int bytesRead = 0;

         while (bytesRead = reader->read(buffer.data(), chunk.second))
         {
            const int bytesSent = this->socket->write(buffer.constData(), bytesRead);

            if (bytesSent == -1)
            {
               L_WARN(QString("Socket: cannot send data: %1").arg(chunk.first->toStringLog()));
               this->closeTheSocket = true;
               goto end;
            }

            this->mutex.lock();
            if (this->toStop)
            {
               this->mutex.unlock();
               goto end;
            }

            chunk.second += bytesSent;
            this->mutex.unlock();

            while (socket->bytesToWrite() > SOCKET_BUFFER_SIZE)
            {
               if (!socket->waitForBytesWritten(SOCKET_TIMEOUT))
               {
                  L_WARN(
                     QString("Socket: cannot write data, error: \"%1\", chunk: %2")
                        .arg(socket->errorString(), chunk.first->toStringLog()
                     )
                  );
                  this->closeTheSocket = true;
                  goto end;
               }
            }

            this->transferRateCalculator.addData(bytesSent);
         }
      }
   }
   catch (FM::UnableToOpenFileInReadModeException&)
   {
      L_WARN("UnableToOpenFileInReadModeException");
      this->closeTheSocket = true;
   }
   catch (FM::IOErrorException&)
   {
      L_WARN("IOErrorException");
      this->closeTheSocket = true;
   }
   catch (FM::ChunkDeletedException)
   {
      L_WARN("ChunkDeletedException");
      this->closeTheSocket = true;
   }
   catch (FM::ChunkDataUnknownException)
   {
      L_WARN("ChunkDataUnknownException");
      this->closeTheSocket = true;
   }

end:
   this->socket->moveToThread(this->mainThread);
}

void ChunksUploader::finished()
{
   this->socket->finished(this->closeTheSocket);
   this->startTimer();
}

/**
  * Stop the current upload. It returns immediately.
  * Do nothing if there is no current upload.
  * See 'Upload::upload()'.
  */
void ChunksUploader::stop()
{
   this->mutex.lock();
   this->toStop = true;
   this->mutex.unlock();
}
