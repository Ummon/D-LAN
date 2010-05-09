#include <Core.h>
using namespace Core;

#include <QObject>
#include <QThread>

#include <Common/LogManager/Builder.h>
#include <Common/LogManager/ILogger.h>
#include <FileManager/Builder.h>
#include <FileManager/IFileManager.h>
#include <NetworkListener/Builder.h>
#include <NetworkListener/INetworkListener.h>
#include <NetworkListener/IChat.h>
#include <NetworkListener/ISearch.h>
#include <PeerManager/Builder.h>

::Core::Core() :
   QObject(),
   logger(LM::Builder::newLogger("Core"))
{
   GOOGLE_PROTOBUF_VERIFY_VERSION;

   LOG_USER(this->logger, "Loading ..");

   QThread::currentThread()->setObjectName("Core");

   this->fileManager = FM::Builder::newFileManager();
   this->peerManager = PM::Builder::newPeerManager();
   this->peerManager->setNick("Test");
   this->networkListener = NL::Builder::newNetworkListener(this->peerManager);

   LOG_USER(this->logger, "Ready to serve");

   NL::ISearch* s = this->networkListener->search();
   s->search("coucou");
   s->search("coucou2");
}

::Core::~Core()
{
   google::protobuf::ShutdownProtobufLibrary();
}
