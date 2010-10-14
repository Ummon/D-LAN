#include <priv/NetworkListener.h>
using namespace NL;

#include <QTimer>

#include <Common/LogManager/Builder.h>

#include <priv/Chat.h>
#include <priv/Log.h>
#include <priv/Search.h>

NetworkListener::NetworkListener(
   QSharedPointer<FM::IFileManager> fileManager,
   QSharedPointer<PM::IPeerManager> peerManager,
   QSharedPointer<DM::IDownloadManager> downloadManager
) :
   fileManager(fileManager),
   peerManager(peerManager),
   downloadManager(downloadManager),
   tCPListener(peerManager),
   uDPListener(fileManager, peerManager, downloadManager, tCPListener.getCurrentPort()),
   chat(uDPListener)
{
}

IChat& NetworkListener::getChat()
{
   return this->chat;
}

QSharedPointer<ISearch> NetworkListener::newSearch()
{
   return QSharedPointer<ISearch>(new Search(this->uDPListener));
}

