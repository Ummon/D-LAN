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

#include <Common/Settings.h>
#include <Core/PeerManager/ISocket.h>

#include <priv/ChunksUploader.h>

/**
  * @class UM::UploadManager
  *
  * Listens to the signal 'getChunks' of the peer manager, each time it is received a 'ChunksUploader' is created and
  * sends the data to the peer. The uploader is kept 'upload_lifetime' ms after it has finished, so the GUI can show it,
  * and is then deleted.
  *
  * We cannot use a QThreadPool object instead of 'Common::ThreadPool' because we have to use the method 'PM::ISocket::moveToThread'
  * when using a socket in a thread. This isn't possible with the 'QRunnable' class.
  */

LOG_INIT_CPP(UploadManager)

UploadManager::UploadManager(QSharedPointer<PM::IPeerManager> peerManager) :
   peerManager(peerManager),
   threadPool(static_cast<int>(SETTINGS.get<quint32>("upload_min_nb_thread")),
   SETTINGS.get<quint32>("upload_thread_lifetime"))
{
   // Keep the platform's default thread stack size: ChunksUploader's read buffer is heap allocated.
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
   for (const auto& upload : std::as_const(this->uploads))
      upload->stop();
}

QList<IChunksUploader*> UploadManager::getChunksUploaders() const
{
   QList<IChunksUploader*> uploaders;
   uploaders.reserve(this->uploads.size());

   for (const auto& upload : this->uploads)
      uploaders << upload.data();

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
   auto upload = QSharedPointer<ChunksUploader>::create(chunksParams, socket, this->transferRateCalculator);

   // The connection must be queued: 'removeUpload(..)' releases the last reference to the uploader and thus
   // deletes it. A direct call would run from 'Common::Timeoutable::timeoutSlot()', that is from within the
   // timer event of the very 'QTimer' emitting the signal, and would delete it under its own event handler.
   connect(
      upload.data(),
      &Common::Timeoutable::timeout,
      this,
      [this, uploadPtr = upload.data()] { this->removeUpload(uploadPtr); },
      Qt::QueuedConnection
   );

   this->uploads << upload;
   this->threadPool.run(upload.toWeakRef());
}

void UploadManager::removeUpload(const ChunksUploader* upload)
{
   this->uploads.removeIf([upload](const QSharedPointer<ChunksUploader>& u) { return u.data() == upload; });
}
