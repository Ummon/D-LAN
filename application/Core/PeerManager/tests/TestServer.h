#ifndef TESTS_PEERMANAGER_TESTSERVER_H
#define TESTS_PEERMANAGER_TESTSERVER_H

#include <QtNetwork>

#include <IPeerManager.h>
using namespace PM;

class TestServer : QObject
{
   Q_OBJECT
public:
   TestServer(QSharedPointer<IPeerManager> peerManager, int port);

private slots:
   void newConnection();

private:
   QSharedPointer<IPeerManager> peerManager;

   QTcpServer server;
};

#endif
