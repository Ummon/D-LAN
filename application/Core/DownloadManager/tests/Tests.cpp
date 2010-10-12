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

void Tests::addADirectoryToDownload()
{
   qDebug() << "===== addADirectoryToDownload() =====";

   Protos::Core::GetEntriesResult result = this->fileManagers[1]->getEntries();
   this->downloadManagers[0]->addDownload(this->peerManagers[1]->getID(), result.entry(0));
}

void Tests::addABigFileToDownload()
{
   qDebug() << "===== addABigFileToDownload() =====";

   const int FILE_SIZE = 128 * 1024 * 1024;
   {
      QFile file("sharedDirs/peer2/big.bin");
      file.open(QIODevice::WriteOnly);

      // To have four different hashes.
      for (int i = 0; i < 4 ; i++)
      {
         QByteArray randomData(32 * 1024 * 1024, i+1);
         file.write(randomData);
      }
   }
   QTest::qWait(4000);

   Protos::Core::GetEntriesResult rootEntry = this->fileManagers[1]->getEntries();

   Protos::Common::Entry entry;
   entry.set_type(Protos::Common::Entry_Type_FILE);
   entry.set_path("/");
   entry.set_name("big.bin");
   entry.set_size(FILE_SIZE);
   entry.mutable_shared_dir()->CopyFrom(rootEntry.entry(0).shared_dir());

   this->downloadManagers[0]->addDownload(this->peerManagers[1]->getID(), entry);
}

void Tests::cleanupTestCase()
{
   QTest::qWait(1000000);
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

