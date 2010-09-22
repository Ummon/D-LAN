#include <TestServer.h>

#include <QSharedPointer>

#include <Common/Constants.h>
#include <Constants.h>

TestServer::TestServer(QSharedPointer<IPeerManager> peerManager)
   : peerManager(peerManager)
{
   connect(&this->server, SIGNAL(newConnection()), this, SLOT(newConnection()));
   this->server.listen(QHostAddress::Any, PORT);
}

void TestServer::newConnection()
{
   this->peerManager->newConnection(this->server.nextPendingConnection());
}
