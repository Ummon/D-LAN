#include <priv/TCPListener.h>
using namespace NL;

#include <Common/LogManager/Builder.h>

TCPListener::TCPListener(QSharedPointer<PM::IPeerManager> newPeerManager) : logger(LM::Builder::newLogger("NetworkListener::TCPListener"))
{
   listen(QHostAddress::Any,55142);

   QObject:: connect(this, SIGNAL(newConnection()),this, SLOT(newConnexion()));

   LOG_DEBU(this->logger, "Listening..");

   this->peerManager = newPeerManager;

}


void TCPListener::newConnexion()
{

   QTcpSocket* socket = nextPendingConnection();

   LOG_DEBU(this->logger, "New connexion form " + socket->peerAddress().toString());

   this->peerManager->newSocket(socket->peerAddress(), QSharedPointer<QTcpSocket>(socket));


}
