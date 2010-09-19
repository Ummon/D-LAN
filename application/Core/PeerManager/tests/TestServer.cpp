#include <TestServer.h>

#include <QSharedPointer>

#include <Common/Constants.h>

TestServer::TestServer(QSharedPointer<IPeerManager> peerManager)
   : peerManager(peerManager)
{
   connect(&this->server, SIGNAL(newConnection()), this, SLOT(newConnection()));
   this->server.listen(QHostAddress::Any, Common::BASE_PORT);
}

void TestServer::newConnection()
{
   this->peerManager->newConnection(QSharedPointer<QTcpSocket>(this->server.nextPendingConnection()));
}
