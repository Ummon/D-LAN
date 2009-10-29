#include <priv/TCPListener.h>
using namespace NL;

#include <Common/LogManager/Builder.h>

TCPListener::TCPListener(QSharedPointer<PM::IPeerManager> newPeerManager) : logger(LM::Builder::newLogger("NetworkListener::TCPListener"))
{
   listen(QHostAddress::Any,55142);

   QObject:: connect(this, SIGNAL(newConnection()),this, SLOT(newConnexion()));

   this->logger->log("Listening..", LM::Debug);

   this->peerManager = newPeerManager;

}


void TCPListener::newConnexion()
{

   QTcpSocket* socket = nextPendingConnection();

   this->logger->log("New connexion form " + socket->peerAddress().toString(), LM::Debug);

   this->peerManager->newSocket(socket->peerAddress(), QSharedPointer<QTcpSocket>(socket));


}
