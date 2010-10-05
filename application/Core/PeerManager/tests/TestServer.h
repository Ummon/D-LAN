#ifndef TESTS_PEERMANAGER_TESTSERVER_H
#define TESTS_PEERMANAGER_TESTSERVER_H

#include <QtNetwork>

#include <Core/PeerManager/IPeerManager.h>

class TestServer : QObject
{
   Q_OBJECT
public:
   TestServer(QSharedPointer<PM::IPeerManager> peerManager, int port);

private slots:
   void newConnection();

private:
   QSharedPointer<PM::IPeerManager> peerManager;

   QTcpServer server;
};

#endif
