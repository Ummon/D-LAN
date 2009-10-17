#include <Core.h>
using namespace Core;

#include <Common/LogManager/Builder.h>
#include <FileManager/Builder.h>
#include <NetworkListener/Builder.h>
#include <NetworkListener/IChat.h>
#include <NetworkListener/ISearch.h>
#include <PeerManager/Builder.h>
#include <QObject>

::Core::Core()
   : QObject(), logger(LogManager::Builder::newLogger("Core"))
{
   this->logger->log("Loading ..", LogManager::EndUser);

   this->fileManager = FileManager::Builder::newFileManager();
   this->peerManager = PeerManager::Builder::newPeerManager();
   this->peerManager->setNick("Test");
   this->networkListener = NetworkListener::Builder::newNetworkListener(this->peerManager);

   this->logger->log("Ready to serve", LogManager::EndUser);

}
