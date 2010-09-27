#include <TestServer.h>

#include <QSharedPointer>
#include <QtDebug>
#include <QTest>

#include <Common/Constants.h>
#include <Constants.h>

/**
  * @class TestServer
  * Listen for new connection and forward them to the given peerManager
  */

TestServer::TestServer(QSharedPointer<IPeerManager> peerManager, int port)
   : peerManager(peerManager)
{
   connect(&this->server, SIGNAL(newConnection()), this, SLOT(newConnection()));
   QVERIFY(this->server.listen(QHostAddress::Any, port));
}

void TestServer::newConnection()
{
   qDebug() << "TestServer::newConnection()";
   QTcpSocket* socket = this->server.nextPendingConnection();
   connect(socket, SIGNAL(disconnected()), socket, SLOT(deleteLater()));
   this->peerManager->newConnection(socket);
}
