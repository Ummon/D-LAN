#include <MockPeerManager.h>

MockPeerManager::MockPeerManager()
   : createPeerNbCall(0)
{

}

MockPeerManager::~MockPeerManager()
{

}

void MockPeerManager::setNick(const QString& nick)
{
   // Never called by the download manager.
}

PM::IPeer* MockPeerManager::getSelf()
{
   // Never called by the download manager.
   return nullptr;
}

int MockPeerManager::getNbOfPeers() const
{
   // Never called by the download manager.
   return 0;
}

QList<PM::IPeer*> MockPeerManager::getPeers() const
{
   // Never called by the download manager.
   return QList<PM::IPeer*>();
}

PM::IPeer* MockPeerManager::getPeer(const Common::Hash& ID)
{
   // Never called by the download manager.
   return nullptr;
}

PM::IPeer* MockPeerManager::createPeer(const Common::Hash& ID, const QString& nick)
{
   // Should never be called.
   this->createPeerNbCall++;
   return nullptr;
}

void MockPeerManager::updatePeer(
   const Common::Hash& ID,
   const QHostAddress& IP,
   quint16 port,
   const QString& nick,
   const quint64& sharingAmount,
   const QString& coreVersion,
   quint32 downloadRate,
   quint32 uploadRate,
   quint32 protocolVersion
)
{
   // Never called by the download manager.
}

void MockPeerManager::removePeer(const Common::Hash& ID, const QHostAddress& IP)
{
   // Never called by the download manager.
}

void MockPeerManager::removeAllPeers()
{
   // Never called by the download manager.
}

void MockPeerManager::newConnection(QTcpSocket* tcpSocket)
{
   // Never called by the download manager.
}
