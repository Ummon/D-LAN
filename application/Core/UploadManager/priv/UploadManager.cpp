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
  
#include <priv/UploadManager.h>
using namespace UM;

#include <QSharedPointer>

#include <Protos/core_protocol.pb.h>

#include <Common/ZeroCopyStreamQIODevice.h>
#include <Common/Settings.h>
#include <Core/FileManager/Exceptions.h>
#include <Core/FileManager/IChunk.h>
#include <Core/PeerManager/ISocket.h>

#include <priv/Log.h>
#include <priv/Uploader.h>

/**
  * @class UploaderManager
  * Will listen the signal 'getChunk' of the peerManager, when this signal is received an Uploader is created and data is sent to the peer.
  * After the chunk was sent to the peer the Uploader is deleted.
  */

UploadManager::UploadManager(QSharedPointer<PM::IPeerManager> peerManager) :
   peerManager(peerManager)
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

   for (QListIterator<Uploader*> i(this->uploaders); i.hasNext();)
      uploads << i.next();

   return uploads;
}

int UploadManager::getUploadRate()
{
   return this->transferRateCalculator.getTransferRate();
}

void UploadManager::getChunk(QSharedPointer<FM::IChunk> chunk, int offset, QSharedPointer<PM::ISocket> socket)
{
   Uploader* uploader = new Uploader(chunk, offset, socket, this->transferRateCalculator);
   connect(uploader, SIGNAL(uploadFinished(bool)), this, SLOT(uploadFinished(bool)), Qt::QueuedConnection);
   connect(uploader, SIGNAL(uploadTimeout()), this, SLOT(deleteUpload()));
   this->uploaders << uploader;
   uploader->start();
}

void UploadManager::uploadFinished(bool networkError)
{
   Uploader* uploader = dynamic_cast<Uploader*>(this->sender());
   uploader->wait(); // TODO : wait for the finished signal instead, like the downloaders : connect(this, SIGNAL(finished()), this, SLOT(downloadingEnded()), Qt::QueuedConnection);

   L_DEBU(QString("Upload finished, chunk : %1").arg(uploader->getChunk()->toStr()));

   uploader->getSocket()->finished(networkError ? PM::SFS_ERROR : PM::SFS_OK);
   uploader->startTimer(); // Will delay the call to 'deleteUploade'.
}

void UploadManager::deleteUpload()
{
   Uploader* uploader = dynamic_cast<Uploader*>(this->sender());
   this->uploaders.removeOne(uploader);
   delete uploader;
}
