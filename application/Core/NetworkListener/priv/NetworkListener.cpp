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
::NetworkListener::NetworkListener(QSharedPointer<PeerManager::IPeerManager> newPeerManager) : logger(LM::Builder::newLogger("NetworkListener"))
{

   this->logger->log("Loading ..", LM::EndUser);

   // References to needed classes.
   this->udpListener = new UDPListener(newPeerManager);
   this->peerManager = newPeerManager;
   this->chat = new Chat(this->udpListener, newPeerManager);

   this->tcpListener = new TCPListener(newPeerManager);

   // We create the timer who will send information about our presence.
   this->timer = new QTimer(this);
   connect(timer, SIGNAL(timeout()), this, SLOT(presence()));
   this->timer->start(static_cast<int>(1000 / IMAliveFrequency));

   this->presence(); // Send the information at the first time.

   // Listening for new search request.
   Chat::connect(this->udpListener, SIGNAL(newFindRequset(const Protos::Core::Find&, const QHostAddress&)), this, SLOT(newFindRequset(const Protos::Core::Find&, const QHostAddress&)));


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
   this->logger->log("Sending <IAmAlive>", LM::Debug);

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
   this->udpListener->sendMessage(QByteArray(output.data()).prepend(IAmAlivePacket));
}

/**
 * Return a new search object
 *
 * @author mcuony
 */
ISearch* ::NetworkListener::search()
{
   return new Search(this->udpListener, this->peerManager);
}

/**
 * Called when someone want to search something
 *
 * For the moment, we just reply with testing data
 *
 * @author mcuony
 */
void ::NetworkListener::newFindRequset(const Protos::Core::Find& request, const QHostAddress& peerAdress)
{
   Protos::Common::FindResult fr;


   fr.set_tag(request.tag());
   fr.mutable_peerid()->set_hash(this->peerManager->getMyId().data());

   std::string output;
   fr.SerializeToString(&output);

   // We broadcast the data.
   this->udpListener->sendMessageTo(QByteArray(output.data()).prepend(findResultPacket), peerAdress);

   this->logger->log("Stupid search answer for " + QString::number(request.tag()), LM::Debug);

}
