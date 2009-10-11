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

    this->ID.setNum(QTime::currentTime().second() + QTime::currentTime().hour() * 900 + (qrand() % 9999) * 180000 + (qrand() % 745641234) * 456123475645);

    this->logger->log("Our current id: " + this->ID.toBase64(), LogManager::EndUser);


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
