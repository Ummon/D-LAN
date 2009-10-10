#include <priv/NetworkListener.h>

using namespace NetworkListener;

#include <Common/LogManager/Builder.h>
#include <priv/Chat.h>

::NetworkListener::NetworkListener()
      : logger(LogManager::Builder::newLogger("NetworkListener"))
{
   this->logger->log("Loading ..", LogManager::EndUser);

   this->chat = new Chat();
}

IChat* ::NetworkListener::getChat() {
    return this->chat;
}
