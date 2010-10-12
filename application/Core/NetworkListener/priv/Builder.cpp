#include <Builder.h>
using namespace NL;

#include <priv/NetworkListener.h>

QSharedPointer<INetworkListener> Builder::newNetworkListener(
   QSharedPointer<FM::IFileManager> fileManager,
   QSharedPointer<PM::IPeerManager> peerManager,
   QSharedPointer<DM::IDownloadManager> downloadManager
)
{
   return QSharedPointer<INetworkListener>(new NetworkListener(fileManager, peerManager, downloadManager));
}
