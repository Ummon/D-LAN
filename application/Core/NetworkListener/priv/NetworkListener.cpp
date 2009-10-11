#include <priv/NetworkListener.h>

using namespace NetworkListener;

#include <Common/LogManager/Builder.h>
#include <priv/Chat.h>
#include <QTimer>

/**
 * Constructor of NetworkListener, initialize subclasses
 *
 * @author mcuony
 */
::NetworkListener::NetworkListener(QSharedPointer<PeerManager::IPeerManager> peerManager_) : logger(LogManager::Builder::newLogger("NetworkListener")) {

    this->logger->log("Loading ..", LogManager::EndUser);

    //We create a new UDPListener
    this->udpListener = new UDPListener();

    //Reference to the peerManager
    this->peerManager = peerManager_;

    //And a new chat object
    this->chat = new Chat(this->udpListener, peerManager_);

    //We create the timer who will send information about our presence
    timer = new QTimer(this);
    connect(timer, SIGNAL(timeout()), this, SLOT(presence()));
    timer->start(1/IMAliveFrequency*1000);

    presence(); //Send the information at the first time
}

/**
 * Return the current 'Chat' instance
 *
 * @author mcuony
 */
IChat* ::NetworkListener::getChat() {
    return this->chat;
}

/**
 * Function called at IMAliveFrequency to send to anothers user informations about us
 *
 * @author mcuony
 */
void ::NetworkListener::presence() {
    this->logger->log("Sending IIMALIME", LogManager::Debug);

    //We put info in our chatMessage Proto
    Protos::Core::HaveChunks IMAlimeMessage;

    IMAlimeMessage.set_amount(1);
    IMAlimeMessage.set_nick(this->peerManager->getNick()->toStdString());
    IMAlimeMessage.set_tag(99);
    IMAlimeMessage.set_version(1);

    Protos::Common::Hash peerId;
    peerId.set_hash(this->peerManager->getMyId()->toBase64());
    *IMAlimeMessage.mutable_peerid() = peerId;

    //We serialize the proto to a string
    std::string output;
    IMAlimeMessage.SerializeToString(&output);

    QString message = "A";

    //We broadcast the data. @TODO: Ugly type of message system
    this->udpListener->sendMessage("I" + message.fromStdString(output));
}
