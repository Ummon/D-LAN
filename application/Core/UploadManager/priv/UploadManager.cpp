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
  
#include <priv/UploadManager.h>
using namespace UM;

#include <QSharedPointer>

#include <Protos/core_protocol.pb.h>

#include <Common/Settings.h>
#include <Core/FileManager/Exceptions.h>
#include <Core/FileManager/IChunk.h>
#include <Core/PeerManager/ISocket.h>

#include <priv/ChunksUploader.h>

/**
  * @class UM::UploaderManager
  *
  * Will listen the signal 'getChunk' of the peerManager, when this signal is received an Uploader is created and data is sent to the peer.
  * After the chunk was sent to the peer the Uploader is deleted.
  *
  * We cannot use a QThreadPool object instead of the class 'Uploader' because we have to use the method 'PM::ISocket::moveToThread' when using a socket in a thread. This isn't possible with the 'QRunnable' class.
  */

LOG_INIT_CPP(UploadManager)

UploadManager::UploadManager(QSharedPointer<PM::IPeerManager> peerManager) :
   peerManager(peerManager),
   threadPool(static_cast<int>(SETTINGS.get<quint32>("upload_min_nb_thread")),
   SETTINGS.get<quint32>("upload_thread_lifetime"))
{
   this->threadPool.setStackSize(MIN_UPLOAD_THREAD_STACK_SIZE + SETTINGS.get<quint32>("buffer_size_reading"));
   connect(
      this->peerManager.data(),
      &PM::IPeerManager::getChunks,
      this,
      &UploadManager::getChunks,
      Qt::DirectConnection
   );
}

UploadManager::~UploadManager()
{
   L_DEBU("UploadManager deleted");

   // We stop all uploads to avoid the thread pool to wait that all threads have finished their job.
   for (QListIterator<QSharedPointer<ChunksUploader>> i(this->uploads); i.hasNext();)
      i.next()->stop();
}

QList<IChunksUploader*> UploadManager::getChunksUploaders() const
{
   QList<IChunksUploader*> uploaders;

   for (QListIterator<QSharedPointer<ChunksUploader>> i(this->uploads); i.hasNext();)
      uploaders << i.next().data();

   return uploaders;
}

int UploadManager::getUploadRate()
{
   return this->transferRateCalculator.getTransferRate();
}

void UploadManager::getChunks(
   const QList<PM::GetChunkParams>& chunksParams,
   const QSharedPointer<PM::ISocket>& socket
)
{
   QSharedPointer<ChunksUploader> upload(new ChunksUploader(chunksParams, socket, this->transferRateCalculator));

   // The connection must be queued: 'uploadTimeout()' releases the last reference to the uploader and thus
   // deletes it. A direct call would run from 'Common::Timeoutable::timeoutSlot()', that is from within the
   // timer event of the very 'QTimer' emitting the signal, and would delete it under its own event handler.
   connect(upload.data(), &Common::Timeoutable::timeout, this, &UploadManager::uploadTimeout, Qt::QueuedConnection);

   this->uploads << upload;
   this->threadPool.run(upload.toWeakRef());
}

/**
  * Called by the event loop, see the queued connection in 'getChunks(..)'.
  */
// TODO: test timeout.
void UploadManager::uploadTimeout()
{
   ChunksUploader* upload = static_cast<ChunksUploader*>(this->sender());

   for (QMutableListIterator<QSharedPointer<ChunksUploader>> i(this->uploads); i.hasNext();)
      if (i.next().data() == upload)
      {
         i.remove();
         break;
      }
}

const quint32 UploadManager::MIN_UPLOAD_THREAD_STACK_SIZE(32 * 1024);
