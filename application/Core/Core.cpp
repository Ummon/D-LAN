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
   : QObject(), logger(LM::Builder::newLogger("Core"))
{
   this->logger->log("Loading ..", LM::EndUser);

   this->fileManager = FM::Builder::newFileManager();
   this->peerManager = PeerManager::Builder::newPeerManager();
   this->peerManager->setNick("Test");
   this->networkListener = NetworkListener::Builder::newNetworkListener(this->peerManager);

   this->logger->log("Ready to serve", LM::EndUser);

      NetworkListener::ISearch* s = this->networkListener->search();
   s->search("coucou");
   s->search("coucou2");

}
