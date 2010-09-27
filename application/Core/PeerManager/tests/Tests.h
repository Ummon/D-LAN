#ifndef TESTS_PEERMANAGER_TESTS_H
#define TESTS_PEERMANAGER_TESTS_H

#include <QTest>
#include <QSharedPointer>
#include <QtNetwork>

#include <google/protobuf/message.h>

#include <Protos/common.pb.h>

#include <Core/FileManager/Builder.h>
#include <Core/FileManager/IFileManager.h>

#include <Builder.h>
#include <IPeerManager.h>
using namespace PM;

#include <TestServer.h>
#include <PeerUpdater.h>
#include <ResultListener.h>

class Tests : public QObject
{
   Q_OBJECT
public:
   Tests();

private slots:
   void initTestCase();
   void updatePeers();
   void getPeerFromID();
   void askForRootEntries();
   void askForSomeEntries();
   void askForHashes();
   void askForAChunk();
   void cleanupTestCase();

//   void socketError(QAbstractSocket::SocketError error);

private:
   void createInitialFiles();
   void deleteAllFiles();

   QList< QSharedPointer<FM::IFileManager> > fileManagers;
   QList< QSharedPointer<IPeerManager> > peerManagers;
   QList<TestServer*> servers;

   PeerUpdater* peerUpdater;

   ResultListener resultListener;
};

#endif
