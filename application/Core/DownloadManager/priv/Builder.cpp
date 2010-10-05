#include <Builder.h>
using namespace DM;

#include <IDownloadManager.h>
#include <priv/DownloadManager.h>

QSharedPointer<IDownloadManager> Builder::newDownloadManager(QSharedPointer<FM::IFileManager> fileManager, QSharedPointer<PM::IPeerManager> peerManager)
{
   return QSharedPointer<IDownloadManager>(new DownloadManager(fileManager, peerManager));
}
