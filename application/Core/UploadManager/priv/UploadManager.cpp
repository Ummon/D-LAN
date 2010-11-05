#include <priv/UploadManager.h>
using namespace UM;

#include <Protos/core_protocol.pb.h>

#include <Common/ZeroCopyStreamQIODevice.h>
#include <Common/Settings.h>
#include <Common/Network.h>
#include <Core/FileManager/IChunk.h>
#include <Core/FileManager/Exceptions.h>
#include <Core/PeerManager/ISocket.h>

#include <priv/Log.h>
#include <priv/Uploader.h>

UploadManager::UploadManager(QSharedPointer<FM::IFileManager> fileManager, QSharedPointer<PM::IPeerManager> peerManager)
   : fileManager(fileManager), peerManager(peerManager)
{
   connect(this->peerManager.data(), SIGNAL(getChunk(Common::Hash, int, QSharedPointer<PM::ISocket>)), this, SLOT(getChunk(Common::Hash, int, QSharedPointer<PM::ISocket>)), Qt::DirectConnection);
}

QList<IUpload*> UploadManager::getUploads()
{
   QList<IUpload*> uploads;

   for (QListIterator<Uploader*> i(this->uploaders); i.hasNext();)
      uploads << i.next();

   return uploads;
}

int UploadManager::getUploadRate() const
{
   int uploadRate = 0;
   for (QListIterator<Uploader*> i(this->uploaders); i.hasNext();)
      uploadRate += i.next()->getUploadRate();

   return uploadRate;
}

void UploadManager::getChunk(Common::Hash hash, int offset, QSharedPointer<PM::ISocket> socket)
{
   QSharedPointer<FM::IChunk> chunk = this->fileManager->getChunk(hash);
   if (chunk.isNull())
   {
      Protos::Core::GetChunkResult result;
      result.set_status(Protos::Core::GetChunkResult_Status_DONT_HAVE);
      socket->send(Common::Network::CORE_GET_CHUNK_RESULT, result);
      socket->finished();

      L_ERRO(QString("UploadManager::getChunk(..) : Chunk unknown : %1").arg(hash.toStr()));
   }
   else
   {
      Protos::Core::GetChunkResult result;
      result.set_status(Protos::Core::GetChunkResult_Status_OK);
      result.set_chunk_size(chunk->getKnownBytes());
      socket->send(Common::Network::CORE_GET_CHUNK_RESULT, result);

      socket->stopListening();
      Uploader* uploader = new Uploader(chunk, offset, socket);
      connect(uploader, SIGNAL(uploadFinished(bool)), this, SLOT(uploadFinished(bool)), Qt::QueuedConnection);
      connect(uploader, SIGNAL(uploadTimeout()), this, SLOT(deleteUpload()));
      this->uploaders << uploader;
      uploader->start();
   }
}

void UploadManager::uploadFinished(bool networkError)
{
   Uploader* uploader = dynamic_cast<Uploader*>(this->sender());
   uploader->wait(); // TODO : wait for the finished signal instead, like the downloaders : connect(this, SIGNAL(finished()), this, SLOT(downloadingEnded()), Qt::QueuedConnection);

   L_DEBU(QString("Upload finished, chunk : %1").arg(uploader->getChunk()->toStr()));

   uploader->getSocket()->finished(networkError);
   uploader->startTimer();
}

void UploadManager::deleteUpload()
{
   Uploader* uploader = dynamic_cast<Uploader*>(this->sender());
   this->uploaders.removeOne(uploader);
   delete uploader;
}
