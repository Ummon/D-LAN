#include "TestServer.h"

#include <QSharedPointer>
#include <QtDebug>
#include <QTest>

#include <Common/Constants.h>

/**
  * @class TestServer
  * Listen for new connection and forward them to the given peerManager.
  * This class is also used by /Core/DownloadManager/tests
  */

TestServer::TestServer(QSharedPointer<PM::IPeerManager> peerManager, int port)
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
