#include <priv/NetworkListener.h>

using namespace NetworkListener;

#include <Common/LogManager/Builder.h>

::NetworkListener::NetworkListener()
      : logger(LogManager::Builder::newLogger("NetworkListener"))
{
   this->logger->log("Loading ..", LogManager::EndUser);
}
