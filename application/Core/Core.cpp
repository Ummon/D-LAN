#include <Core.h>
using namespace Core;

#include <Common/LogManager/Builder.h>
#include <FileManager/Builder.h>
#include <NetworkListener/Builder.h>

::Core::Core()
      : logger(LogManager::Builder::newLogger("Core"))
{
   this->logger->log("Loading ..", LogManager::EndUser);

   this->fileManager = FileManager::Builder::newFileManager();

   this->networkListener = NetworkListener::Builder::newNetworkListener();

   this->logger->log("Ready to serve", LogManager::EndUser);
}
