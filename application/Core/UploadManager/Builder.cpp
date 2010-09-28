#include <Builder.h>
using namespace UM;

#include <Core/FileManager/IFileManager.h>
#include <Core/PeerManager/IPeerManager.h>

#include <priv/UploadManager.h>
#include <IUploadManager.h>

QSharedPointer<IUploadManager> Builder::newFileManager(QSharedPointer<FM::IFileManager> fileManager, QSharedPointer<PM::IPeerManager> peerManager)
{
   return QSharedPointer<IUploadManager>(new UploadManager(fileManager, peerManager));
}
