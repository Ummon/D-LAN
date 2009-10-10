#include <priv/Chat.h>

using namespace NetworkListener;

#include <Common/LogManager/Builder.h>

::Chat::Chat()
      : logger(LogManager::Builder::newLogger("NetworkListener::Chat"))
{
   this->logger->log("Loading ..", LogManager::EndUser);
}


void ::Chat::send(const QString& message) {

    this->logger->log("Message to send: " + message , LogManager::Debug);


    this->logger->log("Sending new message signal for " + message , LogManager::Debug);

    Protos::Core::ChatMessage chatMessage;



    chatMessage.set_message(message.toStdString());


    emit newMessage(chatMessage);
}
