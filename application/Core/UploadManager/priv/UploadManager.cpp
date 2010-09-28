#include <priv/UploadManager.h>
using namespace UM;

#include <Protos/core_protocol.pb.h>

#include <Common/ZeroCopyStreamQIODevice.h>
#include <Core/FileManager/Exceptions.h>
#include <Core/PeerManager/ISocket.h>

#include <priv/Uploader.h>

UploadManager::UploadManager(QSharedPointer<FM::IFileManager> fileManager, QSharedPointer<PM::IPeerManager> peerManager)
   : fileManager(fileManager), peerManager(peerManager)
{
   connect(this->peerManager.data(), SIGNAL(getChunk(Common::Hash, int, ISocket*)), this, SLOT(getChunk(Common::Hash,int,PM::ISocket*)));
}

void UploadManager::getChunk(Common::Hash hash, int offset, PM::ISocket* socket)
{
   try
   {
      QSharedPointer<Uploader> uploader = QSharedPointer<Uploader>(new Uploader(this->fileManager->getChunk(hash), offset, socket));
      this->uploaders << uploader;
      uploader->start();
   }
   catch(FM::UnknownChunkException)
   {
      Protos::Core::GetChunkResult result;
      result.set_status(Protos::Core::GetChunkResult_Status_DONT_HAVE);
      Common::ZeroCopyOutputStreamQIODevice stream(socket->getDevice());
      result.SerializeToZeroCopyStream(&stream);
   }
}
