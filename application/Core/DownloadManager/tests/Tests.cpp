#include <Tests.h>
using namespace DM;

#include <QtDebug>
#include <QStringList>

#include <Protos/core_protocol.pb.h>
#include <Protos/common.pb.h>

#include <Common/LogManager/Builder.h>
#include <Common/PersistantData.h>
#include <Common/Constants.h>
#include <Common/Global.h>
#include <Common/Network.h>
#include <Common/ZeroCopyStreamQIODevice.h>

const int Tests::PORT = 59487;

Tests::Tests()
{
}

void Tests::initTestCase()
{
   LM::Builder::initMsgHandler();
   qDebug() << "===== initTestCase() =====";

   Common::PersistantData::rmValue(Common::FILE_CACHE); // Reset the stored cache.
   this->createInitialFiles();

   this->fileManagers << FM::Builder::newFileManager() << FM::Builder::newFileManager();
   this->peerManagers << PM::Builder::newPeerManager(this->fileManagers[0]) << PM::Builder::newPeerManager(this->fileManagers[1]);

   this->fileManagers[0]->setSharedDirsReadWrite(QStringList() << QDir::currentPath().append("/sharedDirs/peer1/incoming"));
   this->fileManagers[0]->setSharedDirsReadOnly(QStringList() << QDir::currentPath().append("/sharedDirs/peer1/shared"));
   this->fileManagers[1]->setSharedDirsReadOnly(QStringList() << QDir::currentPath().append("/sharedDirs/peer2"));

   this->uploadManagers << UM::Builder::newUploadManager(this->fileManagers[0], this->peerManagers[0]) << UM::Builder::newUploadManager(this->fileManagers[1], this->peerManagers[1]);
   this->downloadManagers << Builder::newDownloadManager(this->fileManagers[0], this->peerManagers[0]) << Builder::newDownloadManager(this->fileManagers[1], this->peerManagers[1]);

   this->peerUpdater = new PeerUpdater(this->fileManagers, this->peerManagers, PORT);

   this->servers << new TestServer(this->peerManagers[0], PORT) << new TestServer(this->peerManagers[1], PORT + 1);
}

void Tests::updatePeers()
{
   qDebug() << "===== updatePeers() =====";

   this->peerUpdater->start();
   QTest::qWait(2000);
}

void Tests::cleanupTestCase()
{
   qDebug() << "===== cleanupTestCase() =====";

   for (QListIterator<TestServer*> i(this->servers); i.hasNext();)
      delete i.next();
   delete this->peerUpdater;
}

void Tests::createInitialFiles()
{
   this->deleteAllFiles();

   Common::Global::createFile("sharedDirs/peer1/incoming/");
   Common::Global::createFile("sharedDirs/peer1/shared/");

   Common::Global::createFile("sharedDirs/peer2/subdir/a.txt");
   Common::Global::createFile("sharedDirs/peer2/subdir/b.txt");
   Common::Global::createFile("sharedDirs/peer2/subdir/c.txt");
   Common::Global::createFile("sharedDirs/peer2/d.txt");
   Common::Global::createFile("sharedDirs/peer2/e.txt");
   Common::Global::createFile("sharedDirs/peer2/f.txt");
}

void Tests::deleteAllFiles()
{
   Common::Global::recursiveDeleteDirectory("sharedDirs");
}

