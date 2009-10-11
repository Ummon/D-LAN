#include <priv/PeerManager.h>

using namespace PeerManager;

#include <Common/LogManager/Builder.h>
#include <Protos/common.pb.h>

/**
 * Constructor of PeerManager, generate a random peer id for ourself
 *
 * @author mcuony
 */
::PeerManager::PeerManager() : logger(LogManager::Builder::newLogger("PeerManager")) {

    this->logger->log("Loading ..", LogManager::EndUser);

    QTime midnight(0, 0, 0);
    qsrand(midnight.secsTo(QTime::currentTime()));

    this->ID.setNum(QTime::currentTime().second() + QTime::currentTime().hour() * 900 + (qrand() % 9999) * 180000 );

    this->logger->log("Our current id: " + this->ID, LogManager::EndUser);

    //We create the timer to clean old peers
    timer = new QTimer(this);
    connect(timer, SIGNAL(timeout()), this, SLOT(cleanUp()));
    timer->start(1/CleanUpFrequency*1000);

}


/**
 * Return the peer id of our current instance
 *
 * @author mcuony
 */
Common::Hash* ::PeerManager::getMyId() {
    return &ID;
}

/**
 * Set the current nick
 *
 * @author mcuony
 */
void ::PeerManager::setNick(const QString &nick_) {
    nick = nick_;
}

/**
 * Get the current nick
 *
 * @author mcuony
 */
QString* ::PeerManager::getNick() {
    return &this->nick;
}

/**
 * A peer just send a IAmAlive packet, we update information about it
 *
 * @author mcuony
 */
void ::PeerManager::updatePeer(const Common::Hash& ID_, quint32 IP_, const QString& nick_, const quint64& amount_) {
    //We probably know that WE are alive
    if (ID_ == ID)
        return;

    this->logger->log(ID_ + " is alive !", LogManager::Debug);

    Peer* thePeer = fromIdToPeer(ID_);

    thePeer->justSeen();

}


/**
 * Return the Peer* coresponding to ID_ in the peer list, and create one if he dosen't exist yet
 *
 * @author mcuony
 */
Peer* ::PeerManager::fromIdToPeer(const Common::Hash& ID_) {

    for (int i = 0; i < peers.length(); i++) {
        if (peers.at(i)->getId()->toLong() == ID_.toLong())
            return peers.at(i);

    }

    this->logger->log(ID_ + " wasn't seen before, creating a new peer.", LogManager::Debug);

    Peer* newPeer = new Peer(ID_);

    this->peers.append(newPeer);

    return newPeer;

}

/**
 * Clean up old peers
 *
 * @author mcuony
 */
void ::PeerManager::cleanUp() {

    this->logger->log("Cleaning up peers", LogManager::Debug);

    for (int i = 0; i < peers.length(); i++) {
        if (peers.at(i)->isAlive() && peers.at(i)->haveYouToDie())
            this->logger->log(*peers.at(i)->getId() + " is dead.", LogManager::Debug);


    }

}
