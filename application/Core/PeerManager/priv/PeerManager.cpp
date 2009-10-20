#include <priv/PeerManager.h>


#include <Common/LogManager/Builder.h>
#include <Protos/common.pb.h>
#include <Common/Hash.h>

/**
 * Return the peer id of our current instance
 *
 * @author mcuony
 */


Common::Hash PeerManager::PeerManager::getMyId()
{
    return this->ID;
}

using namespace PeerManager;

/**
 * Constructor of PeerManager, generate a random peer id for ourself
 *
 * @author mcuony
 */
::PeerManager::PeerManager() : logger(LogManager::Builder::newLogger("PeerManager"))
{

    this->logger->log("Loading ..", LogManager::EndUser);


    this->ID = Common::Hash::rand();

    this->logger->log("Our current id: " + this->ID.toStr(), LogManager::EndUser);

    // We create the timer to clean old peers.
    this->timer = new QTimer(this);
    connect(this->timer, SIGNAL(timeout()), this, SLOT(cleanUp()));
    this->timer->start(static_cast<int>(1000 / CleanUpFrequency));


}

/**
 * Set the current nick
 *
 * @author mcuony
 */
void ::PeerManager::setNick(const QString & newNick)
{
    this->nick = newNick;
}

/**
 * Get the current nick
 *
 * @author mcuony
 */
QString* ::PeerManager::getNick()
{
    return &this->nick;
}

/**
 * A peer just send a IAmAlive packet, we update information about it
 *
 * @author mcuony
 */
void ::PeerManager::updatePeer(const Common::Hash& peerID, const QHostAddress&  peerIP, const QString& peerNick, const quint64& peerAmount)
{
    // We probably know that WE are alive.
    if (peerID == this->ID)
        return;

    this->logger->log(peerID.toStr() + " is alive !", LogManager::Debug);

    Peer* thePeer = this->fromIdToPeer(peerID);

    thePeer->justSeen(peerIP, peerNick, peerAmount);

}


/**
 * Return the Peer* coresponding to ID_ in the peer list, and create one if he dosen't exist yet
 *
 * @author mcuony
 */
Peer* ::PeerManager::fromIdToPeer(const Common::Hash& peerID)
{

    for (int i = 0; i < this->peers.length(); i++)
    {
        if (this->peers.at(i)->getId() == peerID)
            return this->peers.at(i);

    }

    this->logger->log(peerID.toStr() + " wasn't seen before, creating a new peer.", LogManager::Debug);

    Peer* newPeer = new Peer(peerID);

    this->peers.append(newPeer);

    return newPeer;

}

/**
 * Clean up old peers
 *
 * @author mcuony
 */
void ::PeerManager::cleanUp()
{

    this->logger->log("Cleaning up peers", LogManager::Debug);

    for (int i = 0; i < peers.length(); i++)
    {
        if (this->peers.at(i)->isAlive() && this->peers.at(i)->haveYouToDie())
            this->logger->log(peers.at(i)->getId().toStr() + " is dead.", LogManager::Debug);


    }

}

void ::PeerManager::newSocket(const QHostAddress&  peerIP, QSharedPointer<QTcpSocket> socket)
{

   for (int i = 0; i < peers.length(); i++)
    {
        if (this->peers.at(i)->isAlive() && this->peers.at(i)->getIp() == peerIP)
        {
            this->logger->log(peers.at(i)->getId().toStr() + " want a connetion", LogManager::Debug);
            peers.at(i)->newSocket(socket);
        }


    }

}
