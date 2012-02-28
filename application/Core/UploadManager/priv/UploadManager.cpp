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

#include <Common/ZeroCopyStreamQIODevice.h>
#include <Common/Settings.h>
#include <Core/FileManager/Exceptions.h>
#include <Core/FileManager/IChunk.h>
#include <Core/PeerManager/ISocket.h>

#include <priv/Upload.h>

/**
  * @class UM::UploaderManager
  *
  * Will listen the signal 'getChunk' of the peerManager, when this signal is received an Uploader is created and data is sent to the peer.
  * After the chunk was sent to the peer the Uploader is deleted.
  *
  * We cannot use a QThreadPool object instead of the class 'Uploader' because we have to use the method 'PM::ISocket::moveToThread' when using a socket in a thread. This isn't possible with the 'QRunnable' class.
  */

LOG_INIT_CPP(UploadManager);

UploadManager::UploadManager(QSharedPointer<PM::IPeerManager> peerManager) :
   peerManager(peerManager), threadPool(static_cast<int>(SETTINGS.get<quint32>("upload_min_nb_thread")), SETTINGS.get<quint32>("upload_thread_lifetime"))
{
   connect(this->peerManager.data(), SIGNAL(getChunk(QSharedPointer<FM::IChunk>, int, QSharedPointer<PM::ISocket>)), this, SLOT(getChunk(QSharedPointer<FM::IChunk>, int, QSharedPointer<PM::ISocket>)), Qt::DirectConnection);
}

UploadManager::~UploadManager()
{
   L_DEBU("UploadManager deleted");
}

QList<IUpload*> UploadManager::getUploads() const
{
   QList<IUpload*> uploads;

   for (QListIterator< QSharedPointer<Upload> > i(this->uploads); i.hasNext();)
      uploads << i.next().data();

   return uploads;
}

int UploadManager::getUploadRate()
{
   return this->transferRateCalculator.getTransferRate();
}

void UploadManager::getChunk(QSharedPointer<FM::IChunk> chunk, int offset, QSharedPointer<PM::ISocket> socket)
{
   QSharedPointer<Upload> upload(new Upload(chunk, offset, socket, this->transferRateCalculator));
   connect(upload.data(), SIGNAL(timeout()), this, SLOT(uploadTimeout()));
   this->uploads << upload;
   this->threadPool.run(upload.toWeakRef());
}

void UploadManager::uploadTimeout()
{
   Upload* upload = dynamic_cast<Upload*>(this->sender());

   for (QMutableListIterator< QSharedPointer<Upload> > i(this->uploads); i.hasNext();)
      if (i.next().data() == upload)
      {
         i.remove();
         break;
      }
}
