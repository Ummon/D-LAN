#include <priv/NetworkListener.h>

using namespace NetworkListener;

#include <Common/LogManager/Builder.h>
#include <priv/Chat.h>

/**
 * Constructor of NetworkListener, initialize subclasses
 *
 * @author mcuony
 */
::NetworkListener::NetworkListener() : logger(LogManager::Builder::newLogger("NetworkListener")) {

    this->logger->log("Loading ..", LogManager::EndUser);

    //We create a new UDPListener
    this->udpListener = new UDPListener();

    //And a new chat object
    this->chat = new Chat(this->udpListener);
}

/**
 * Return the current 'Chat' instance
 *
 * @author mcuony
 */
IChat* ::NetworkListener::getChat() {
    return this->chat;
}
