#include <Core.h>
using namespace Core;

#include <QObject>
#include <QThread>

#include <FileManager/Builder.h>
#include <PeerManager/Builder.h>
#include <UploadManager/Builder.h>
#include <DownloadManager/Builder.h>
#include <NetworkListener/Builder.h>

#include <Log.h>

::Core::Core()
{
   GOOGLE_PROTOBUF_VERIFY_VERSION;

   L_USER("Loading ..");

   this->checkSettingsIntegrity();

   QThread::currentThread()->setObjectName("Core");

   this->fileManager = FM::Builder::newFileManager();
   this->peerManager = PM::Builder::newPeerManager(this->fileManager);
   this->uploadManager = UM::Builder::newUploadManager(this->fileManager, this->peerManager);
   this->downloadManager = DM::Builder::newDownloadManager(this->fileManager, this->peerManager);
   this->networkListener = NL::Builder::newNetworkListener(this->fileManager, this->peerManager, this->downloadManager);

   L_USER("Ready to serve");
}

::Core::~Core()
{
   google::protobuf::ShutdownProtobufLibrary();
}

/**
  * Check if each value settings is valid, for example buffer_size cannot be one byte or 3 TiB..
  */
void ::Core::checkSettingsIntegrity()
{
   //LOG_USER(this->logger, "Checking");
   // TODO..
}
