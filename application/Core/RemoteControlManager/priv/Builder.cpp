#include <Builder.h>
using namespace RCM;

#include <priv/RemoteControlManager.h>

QSharedPointer<IRemoteControlManager> Builder::newRemoteControlManager(
   QSharedPointer<FM::IFileManager> fileManager,
   QSharedPointer<PM::IPeerManager> peerManager,
   QSharedPointer<UM::IUploadManager> uploadManager,
   QSharedPointer<DM::IDownloadManager> downloadManager,
   QSharedPointer<NL::INetworkListener> networkListener
)
{
   return QSharedPointer<IRemoteControlManager>(new RemoteControlManager(fileManager, peerManager, uploadManager, downloadManager, networkListener));
}
