/**
  * Aybabtu - A decentralized LAN file sharing software.
  * Copyright (C) 2010-2011 Greg Burri <greg.burri@gmail.com>
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
  
#include <priv/Uploader.h>
using namespace UM;

#include <QByteArray>
#include <QSharedPointer>

#include <Common/Settings.h>

#include <Core/FileManager/Exceptions.h>
#include <Core/PeerManager/ISocket.h>

#include <priv/Constants.h>
#include <priv/Log.h>

/**
  * @class Uploader
  * An uploader is used to send one chunk to one peer via one socket.
  * The transfert is threaded.
  * The signal 'uploadFinished' is emitted when the upload is finished or if the socket is closed,
  * in this latter case or if any error appears the signal error flag is set to true.
  */

quint64 Uploader::currentID(1);

Uploader::Uploader(QSharedPointer<FM::IChunk> chunk, int offset, QSharedPointer<PM::ISocket> socket)
   : ID(currentID++), chunk(chunk), offset(offset), socket(socket)
{
   this->mainThread = QThread::currentThread();
   this->socket->getQSocket()->moveToThread(this);

   this->timer.setInterval(SETTINGS.get<quint32>("upload_live_time"));
   this->timer.setSingleShot(true);
   connect(&this->timer, SIGNAL(timeout()), this, SIGNAL(uploadTimeout()));
}

quint64 Uploader::getID() const
{
   return this->ID;
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
   QMutexLocker locker(&this->mutex);
   return 100LL * this->offset / this->chunk->getChunkSize();
}

QSharedPointer<FM::IChunk> Uploader::getChunk() const
{
   return this->chunk;
}

QSharedPointer<PM::ISocket> Uploader::getSocket() const
{
   return this->socket;
}

void Uploader::startTimer()
{
   this->timer.start();
}

void Uploader::run()
{
   L_DEBU(QString("Starting uploading a chunk from offset %1 : %2").arg(this->offset).arg(this->chunk->toStr()));

   bool networkError = false;

   try
   {
      QSharedPointer<FM::IDataReader> reader = this->chunk->getDataReader();

      char buffer[SETTINGS.get<quint32>("buffer_size")];
      qint64 bytesRead = 0;

      this->transferRateCalculator.reset();

      while (bytesRead = reader->read(buffer, this->offset))
      {
         int bytesSent = this->socket->getQSocket()->write(buffer, bytesRead);
         if (bytesSent == -1)
         {
            L_ERRO(QString("Socket : cannot send data : %1").arg(this->chunk->toStr()));
            networkError = true;
            break;
         }

         this->mutex.lock();
         this->offset += bytesSent;
         this->mutex.unlock();

         // Sometimes it will block when data are send between the 'bytesToWrite' call and the 'waitForBytesWritten' call.
         if (socket->getQSocket()->bytesToWrite() > SETTINGS.get<quint32>("socket_buffer_size"))
         {
            if (!socket->getQSocket()->waitForBytesWritten(SETTINGS.get<quint32>("socket_timeout")))
            {
               L_ERRO(QString("Socket : cannot write data, timeout, chunk : %1, error : %2").arg(this->chunk->toStr()).arg(socket->getQSocket()->errorString()));
               networkError = true;
               break;
            }
         }

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
   catch (FM::ChunkNotCompletedException)
   {
      L_ERRO("ChunkNotCompletedException");
   }

   this->socket->getQSocket()->moveToThread(this->mainThread);

   emit uploadFinished(networkError);
}
