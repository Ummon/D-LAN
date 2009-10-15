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
::NetworkListener::NetworkListener(QSharedPointer<PeerManager::IPeerManager> newPeerManager) : logger(LogManager::Builder::newLogger("NetworkListener"))
{

   this->logger->log("Loading ..", LogManager::EndUser);

   // References to needed classes.
   this->udpListener = new UDPListener(newPeerManager);
   this->peerManager = newPeerManager;
   this->chat = new Chat(this->udpListener, newPeerManager);

   // We create the timer who will send information about our presence.
   this->timer = new QTimer(this);
   connect(timer, SIGNAL(timeout()), this, SLOT(presence()));

   this->timer->start(static_cast<int>(1000 / IMAliveFrequency));

   this->presence(); // Send the information at the first time.
}

/**
 * Return the current 'Chat' instance
 *
 * @author mcuony
 */
IChat* ::NetworkListener::getChat()
{
   return this->chat;
}

/**
 * Function called at IMAliveFrequency to send to anothers user informations about us
 *
 * @author mcuony
 */
void ::NetworkListener::presence()
{
   this->logger->log("Sending <IAmAlive>", LogManager::Debug);

   // We put info in a IMAlimeMessage Proto.
   Protos::Core::HaveChunks IMAlimeMessage;

   IMAlimeMessage.set_amount(1);
   IMAlimeMessage.set_nick(this->peerManager->getNick()->toStdString());
   IMAlimeMessage.set_tag(99);
   IMAlimeMessage.set_version(1);
   IMAlimeMessage.mutable_peerid()->set_hash(this->peerManager->getMyId().data());

   // We serialize the proto to a string.
   std::string output;
   IMAlimeMessage.SerializeToString(&output);

   // We broadcast the data.
   this->udpListener->sendMessage(IAmAlivePacket + QString::fromStdString(output));
}
