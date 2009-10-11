#include <priv/Chat.h>

using namespace NetworkListener;

#include <Common/LogManager/Builder.h>
#include <priv/UDPListener.h>

/**
  * Create a new Chat object
  *
  * @param udpListener : An udpListener object
  * @author mcuony
  */
::Chat::Chat(UDPListener* udpListener_, QSharedPointer<PeerManager::IPeerManager> peerManager_) : logger(LogManager::Builder::newLogger("NetworkListener::Chat"))  {

   this->logger->log("Loading ..", LogManager::EndUser);

   //Referencing the udpListener
   this->udpListener = udpListener_;

   //Referencing the peerManager
   this->peerManager = peerManager_;

   //Listening for new messages
   Chat::connect(this->udpListener, SIGNAL(newChatMessage(const Protos::Core::ChatMessage&)), this, SLOT(newChatMessage(const Protos::Core::ChatMessage&)));

}

/**
  * Send a chat message
  *
  * @param message : The message to send
  * @author mcuony
  */
void ::Chat::send(const QString& message) {

    this->logger->log("Message to send: " + message , LogManager::Debug);

    //We put info in our chatMessage Proto
    Protos::Core::ChatMessage chatMessage;
    chatMessage.set_message(message.toStdString());


    Protos::Common::Hash peerId;
    peerId.set_hash(this->peerManager->getMyId()->toStdString());
    *chatMessage.mutable_peerid() = peerId;



    //We serialize the proto to a string
    std::string output;
    chatMessage.SerializeToString(&output);


    //We broadcast the data. @TODO: Ugly type of message system
    this->udpListener->sendMessage("C" + message.fromStdString(output));
}

/**
 * Reception of a new chat message form UDPListener
 * We forward the message to our listeners
 *
 * @author mcuony
 */
void ::Chat::newChatMessage(const Protos::Core::ChatMessage& message) {
    emit newMessage(message);
}
