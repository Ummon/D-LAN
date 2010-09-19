#ifndef PEER_MANAGER_TESTSERVER_H
#define PEER_MANAGER_TESTSERVER_H

#include <QtNetwork>

#include <IPeerManager.h>
using namespace PM;

class TestServer : QObject
{
   Q_OBJECT
public:
   TestServer(QSharedPointer<IPeerManager> peerManager);

private slots:
   void newConnection();

private:
   QSharedPointer<IPeerManager> peerManager;

   QTcpServer server;
};

#endif
