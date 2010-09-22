#ifndef TESTS_PEERMANAGER_TESTS_H
#define TESTS_PEERMANAGER_TESTS_H

#include <QTest>
#include <QSharedPointer>

#include <Protos/common.pb.h>

#include <Core/FileManager/Builder.h>
#include <Core/FileManager/IFileManager.h>

#include <Builder.h>
#include <IPeerManager.h>
using namespace PM;

#include <TestServer.h>
#include <PeerUpdater.h>

class Tests : public QObject
{
   Q_OBJECT
public:
   Tests();

private slots:
   void initTestCase();
   void getId();
   void setGetNick();
   void updatePeers();
   void getPeerFromID();
   void askForEntries();
   void cleanupTestCase();

   void socketError(QAbstractSocket::SocketError error);

private:
   void createInitialFiles();
   void deleteAllFiles();

   QSharedPointer<FM::IFileManager> fileManager;
   QSharedPointer<IPeerManager> peerManager;

   TestServer* server;
   PeerUpdater* peerUpdater;
};

#endif
