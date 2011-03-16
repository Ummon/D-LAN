/**
  * D-LAN - A decentralized LAN file sharing software.
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

Uploader::Uploader(QSharedPointer<FM::IChunk> chunk, int offset, QSharedPointer<PM::ISocket> socket, Common::TransferRateCalculator& transferRateCalculator) :
   ID(currentID++), chunk(chunk), offset(offset), socket(socket), transferRateCalculator(transferRateCalculator)
{
   this->mainThread = QThread::currentThread();
   this->socket->moveToThread(this);

   this->timer.setInterval(SETTINGS.get<quint32>("upload_live_time"));
   this->timer.setSingleShot(true);
   connect(&this->timer, SIGNAL(timeout()), this, SIGNAL(uploadTimeout()));
}

Uploader::~Uploader()
{
   L_DEBU(QString("Uploader#%1 deleted").arg(this->ID));
}

quint64 Uploader::getID() const
{
   return this->ID;
}

Common::Hash Uploader::getPeerID() const
{
   return this->socket->getRemotePeerID();
}

int Uploader::getProgress() const
{
   QMutexLocker locker(&this->mutex);

   const int chunkSize = this->chunk->getChunkSize();
   if (chunkSize != 0)
      return 100LL * this->offset / this->chunk->getChunkSize();
   else
      return 0;
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
   L_DEBU(QString("Starting uploading a chunk from offset %1 : %2").arg(this->offset).arg(this->chunk->toStringLog()));

   static const quint32 BUFFER_SIZE = SETTINGS.get<quint32>("buffer_size");
   static const quint32 SOCKET_BUFFER_SIZE = SETTINGS.get<quint32>("socket_buffer_size");
   static const quint32 SOCKET_TIMEOUT = SETTINGS.get<quint32>("socket_timeout");

   bool networkError = false;

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
            networkError = true;
            break;
         }

         this->mutex.lock();
         this->offset += bytesSent;
         this->mutex.unlock();

         while (socket->bytesToWrite() > SOCKET_BUFFER_SIZE)
         {
            if (!socket->waitForBytesWritten(SOCKET_TIMEOUT))
            {
               L_WARN(QString("Socket : cannot write data, error : %1, chunk : %2").arg(socket->errorString()).arg(this->chunk->toStringLog()));
               networkError = true;
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

   emit uploadFinished(networkError);
}
