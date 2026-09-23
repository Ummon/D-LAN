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

#include <typeinfo>
#include <utility>

#include <QElapsedTimer>

#include <Common/Settings.h>
#include <Core/FileManager/Exceptions.h>
#include <Core/FileManager/IDataReader.h>

#include <priv/Log.h>

/**
  * Un chunk uploader will write a given chunk to a given socket.
  * This operation is threaded and must be run by a 'Common::ThreadPool'.
  */

namespace
{
   const int STOP_POLL_INTERVAL = 100; // Maximum socket wait before checking cancellation again, in ms.
}

quint64 ChunksUploader::currentID(1);

ChunksUploader::ChunksUploader(
   QList<PM::GetChunkParams> chunksParams,
   const QSharedPointer<PM::ISocket>& socket,
   Common::TransferRateCalculator& transferRateCalculator
) :
   Common::Timeoutable(SETTINGS.get<quint32>("upload_lifetime")),
   mainThread(QThread::currentThread()),
   ID(currentID++),
   chunks(std::move(chunksParams)),
   socket(socket),
   transferRateCalculator(transferRateCalculator),
   socketTimeout(SETTINGS.get<quint32>("socket_timeout")),
   closeTheSocket(false),
   toStop(false)
{
}

ChunksUploader::~ChunksUploader()
{
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

/**
  * May be called from another thread than the one running 'run()', see 'UploadManager::getChunksUploaders()'.
  */
QList<PM::GetChunkParams> ChunksUploader::getChunks() const
{
   QMutexLocker locker(&this->mutex);
   return this->chunks;
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
   bool completed = false;

   try
   {
      completed = this->uploadChunks();
   }
   catch (FM::UnableToOpenFileInReadModeException&)
   {
      L_WARN("UnableToOpenFileInReadModeException");
   }
   catch (FM::IOErrorException&)
   {
      L_WARN("IOErrorException");
   }
   catch (FM::ChunkDeletedException&)
   {
      L_WARN("ChunkDeletedException");
   }
   catch (FM::ChunkDataUnknownException&)
   {
      L_WARN("ChunkDataUnknownException");
   }
   // Nothing may leave this method: it is called from 'QThread::run()' by the thread pool, an escaping
   // exception would terminate the process and the socket would never be given back to the main thread.
   catch (const std::exception& e)
   {
      L_ERRO(QString("Unexpected exception, type: %1, what: %2").arg(typeid(e).name(), e.what()));
   }
   catch (...)
   {
      L_ERRO("Unknown exception");
   }

   // The peer was promised a raw stream of the announced size, there is no way to tell it the upload has been
   // truncated: closing the socket is the only way to avoid it reading the next messages as chunk data.
   this->closeTheSocket = !completed;

   this->socket->moveToThread(this->mainThread);
}

void ChunksUploader::finished()
{
   this->socket->finished(this->closeTheSocket);
   this->startTimer();
}

/**
  * Sends the announced range of each chunk.
  * @return 'false' if the upload has been cancelled or has failed, the socket must then be closed.
  */
bool ChunksUploader::uploadChunks()
{
   static const quint32 BUFFER_SIZE = SETTINGS.get<quint32>("buffer_size_reading");

   // Allocated once for all the chunks, 'buffer_size_reading' may be large.
   QByteArray buffer(BUFFER_SIZE, Qt::Uninitialized);

   // 'this->chunks' is never iterated by reference: 'getChunks()' may copy it from another thread at any
   // moment, the detach occurring at the next write would then invalidate any reference into it. Only the
   // current element is kept as a local copy, the shared list is written under the mutex.
   for (int i = 0; i < this->chunks.size(); i++)
   {
      if (this->mustStop())
         return false;
      // Only this thread writes the elements, reading one without the mutex is safe.
      PM::GetChunkParams chunk = this->chunks.at(i);

      // Also guard internal callers: no invalid range may reach the reader or offset arithmetic.
      if (chunk.getOffset() < 0 || chunk.getEndOffset() < chunk.getOffset())
      {
         L_WARN(QString("Invalid upload range [%1, %2), closing the socket.")
            .arg(chunk.getOffset()).arg(chunk.getEndOffset()));
         return false;
      }

      // An offset equal to the announced endpoint is a valid empty range.
      if (chunk.getOffset() == chunk.getEndOffset())
         continue;

      L_DEBU(
         QString("Starting uploading a chunk from offset %1: %2")
            .arg(chunk.getOffset())
            .arg(chunk.getChunk()->toStringLog()
         )
      );

      QSharedPointer<FM::IDataReader> reader = chunk.getChunk()->getDataReader();

      // The reader's available data may grow beyond the announced endpoint. Once that
      // endpoint is reached, even an EOF probe is unnecessary and could fail after success.
      while (chunk.getOffset() < chunk.getEndOffset())
      {
         if (this->mustStop())
            return false;

         int bytesRead = reader->read(buffer.data(), chunk.getOffset());
         // A read may block; do not write its result if stop() was called meanwhile.
         if (this->mustStop())
            return false;
         if (bytesRead <= 0)
            break;
         // 'IChunk::getKnownBytes()', which bounds the reader, may have grown since the size was announced
         // to the peer: a chunk may be uploaded while being downloaded. Only the announced amount may be
         // sent, the peer reads exactly this many bytes and would take the next ones for a message header.
         bytesRead = qMin(bytesRead, chunk.getEndOffset() - chunk.getOffset());

         const int bytesSent = this->writeToSocket(buffer.constData(), bytesRead, chunk);
         if (bytesSent < 0)
            return false;

         this->transferRateCalculator.addData(bytesSent);

         chunk.setOffset(chunk.getOffset() + bytesSent);
         {
            QMutexLocker locker(&this->mutex);
            this->chunks[i].setOffset(chunk.getOffset());
         }

         if (!this->waitForSocketBufferRoom(chunk))
            return false;
      }

      if (chunk.getOffset() < chunk.getEndOffset())
      {
         L_WARN(
            QString("Only %1 of the %2 announced bytes could be read, closing the socket. Chunk: %3")
               .arg(chunk.getOffset())
               .arg(chunk.getEndOffset())
               .arg(chunk.getChunk()->toStringLog())
         );
         return false;
      }
   }

   return true;
}

/**
  * Writes the given data, retrying while the socket accepts none of it.
  * @return The number of bytes written, may be less than 'size', or -1 if the upload must be aborted.
  */
int ChunksUploader::writeToSocket(const char* data, int size, const PM::GetChunkParams& chunk)
{
   qint64 bytesSent = this->socket->write(data, size);

   QElapsedTimer writeStalled;
   writeStalled.start();
   while (bytesSent == 0)
   {
      const qint64 remaining = this->socketTimeout - writeStalled.elapsed();
      if (remaining <= 0)
      {
         L_WARN(QString("Socket: no data accepted before timeout: %1").arg(chunk.getChunk()->toStringLog()));
         return -1;
      }

      // Retry the same buffer, without rereading the file or advancing progress. Even if
      // other queued bytes drain, this write must accept data within its timeout budget.
      this->waitForBytesWritten(remaining);
      if (this->mustStop())
         return -1;
      bytesSent = this->socket->write(data, size);
   }

   if (bytesSent < 0)
   {
      L_WARN(QString("Socket: cannot send data: %1").arg(chunk.getChunk()->toStringLog()));
      return -1;
   }

   return static_cast<int>(bytesSent);
}

/**
  * Waits until the data queued in the socket is below 'socket_buffer_size'.
  * @return 'false' if the upload has been cancelled or if no data could be written during 'socket_timeout'.
  */
bool ChunksUploader::waitForSocketBufferRoom(const PM::GetChunkParams& chunk)
{
   static const quint32 SOCKET_BUFFER_SIZE = SETTINGS.get<quint32>("socket_buffer_size");

   QElapsedTimer noProgress;
   noProgress.start();
   while (this->socket->bytesToWrite() > SOCKET_BUFFER_SIZE)
   {
      // Checked here too: this loop may last as long as the whole chunk and 'stop()' expects the
      // upload to end quickly, see 'UploadManager::~UploadManager()'.
      if (this->mustStop())
         return false;

      const qint64 remaining = this->socketTimeout - noProgress.elapsed();
      if (remaining <= 0)
      {
         L_WARN(
            QString("Socket: cannot write data, error: \"%1\", chunk: %2")
               .arg(this->socket->errorString(), chunk.getChunk()->toStringLog()
            )
         );
         return false;
      }

      // A short wait timing out is not an upload failure. Keep the full configured
      // no-progress budget, restarting it only when bytes have actually been written.
      if (this->waitForBytesWritten(remaining))
         noProgress.restart();
   }

   return true;
}

/**
  * Waits at most 'STOP_POLL_INTERVAL' ms, or 'maxWait' if it is shorter, so that a cancellation is noticed quickly.
  * Some socket errors make the wait return immediately: the rest of the period is then slept to avoid a busy loop.
  * @return 'true' if some data has been written.
  */
bool ChunksUploader::waitForBytesWritten(qint64 maxWait)
{
   const int waitTime = static_cast<int>(qMin<qint64>(STOP_POLL_INTERVAL, maxWait));

   QElapsedTimer waitDuration;
   waitDuration.start();
   if (this->socket->waitForBytesWritten(waitTime))
      return true;

   const qint64 delay = waitTime - waitDuration.elapsed();
   if (delay > 0 && !this->mustStop())
      QThread::msleep(static_cast<unsigned long>(delay));

   return false;
}

/**
  * Returns 'true' if 'stop()' has been called, the upload must then be aborted.
  */
bool ChunksUploader::mustStop() const
{
   return this->toStop;
}

/**
  * Stop the current upload. It returns immediately and may be called from any thread.
  * Socket waits check this request every 100 ms. An in-progress synchronous file read must
  * return before cancellation can be observed; its data will then be discarded.
  */
void ChunksUploader::stop()
{
   this->toStop = true;
}
