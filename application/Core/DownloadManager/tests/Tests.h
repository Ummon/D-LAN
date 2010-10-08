#ifndef TESTS_DOWNLOADMANAGER_TESTS_H
#define TESTS_DOWNLOADMANAGER_TESTS_H

#include <QTest>
#include <QSharedPointer>
#include <QtNetwork>

#include <google/protobuf/message.h>

#include <Protos/common.pb.h>

#include <Core/FileManager/Builder.h>
#include <Core/FileManager/IFileManager.h>

#include <Core/PeerManager/Builder.h>
#include <Core/PeerManager/IPeerManager.h>

#include <Core/UploadManager/Builder.h>
#include <Core/UploadManager/IUploadManager.h>

#include <Builder.h>
#include <IDownloadManager.h>
using namespace DM;

#include <Core/PeerManager/tests/TestServer.h>
#include <Core/PeerManager/tests/PeerUpdater.h>

class Tests : public QObject
{
   Q_OBJECT
   static const int PORT;

public:
   Tests();

private slots:
   void initTestCase();
   void updatePeers();
   void addADirectoryToDownload();
   void cleanupTestCase();

private:
   void createInitialFiles();
   void deleteAllFiles();

   QList< QSharedPointer<FM::IFileManager> > fileManagers;
   QList< QSharedPointer<PM::IPeerManager> > peerManagers;
   QList< QSharedPointer<UM::IUploadManager> > uploadManagers;
   QList< QSharedPointer<DM::IDownloadManager> > downloadManagers;

   QList<TestServer*> servers;

   PeerUpdater* peerUpdater;
};

#endif
