#include <Tests.h>
using namespace PM;

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

#include <ResultListener.h>
#include <IGetEntriesResult.h>
#include <IGetHashesResult.h>
#include <Constants.h>

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
   this->peerManagers << Builder::newPeerManager(this->fileManagers[0]) << Builder::newPeerManager(this->fileManagers[1]);

   this->fileManagers[0]->setSharedDirsReadOnly(QStringList() << QDir::currentPath().append("/sharedDirs/peer1"));
   this->fileManagers[1]->setSharedDirsReadOnly(QStringList() << QDir::currentPath().append("/sharedDirs/peer2"));

   this->peerUpdater = new PeerUpdater(this->fileManagers, this->peerManagers);

   this->servers << new TestServer(this->peerManagers[0], PORT) << new TestServer(this->peerManagers[1], PORT + 1);
}

void Tests::updatePeers()
{
   qDebug() << "===== updatePeers() =====";

   this->peerUpdater->start();

   QTest::qWait(2000);

   // Check if each peer know the other.
   QCOMPARE(this->peerManagers[0]->getPeers().size(), 1);
   QCOMPARE(this->peerManagers[1]->getPeers().size(), 1);

   QCOMPARE(this->peerManagers[1]->getPeers()[0]->getID(), this->peerManagers[0]->getID());
   QCOMPARE(this->peerManagers[1]->getPeers()[0]->getNick(), this->peerManagers[0]->getNick());
   QCOMPARE(this->peerManagers[1]->getPeers()[0]->getSharingAmount(), this->fileManagers[0]->getAmount());

   QCOMPARE(this->peerManagers[0]->getPeers()[0]->getID(), this->peerManagers[1]->getID());
   QCOMPARE(this->peerManagers[0]->getPeers()[0]->getNick(), this->peerManagers[1]->getNick());
   QCOMPARE(this->peerManagers[0]->getPeers()[0]->getSharingAmount(), this->fileManagers[1]->getAmount());
}

void Tests::getPeerFromID()
{
   qDebug() << "===== getPeerFromID() =====";

   QCOMPARE(this->peerManagers[0]->getID(), this->peerManagers[1]->getPeer(this->peerManagers[0]->getID())->getID());
   QCOMPARE(this->peerManagers[1]->getID(), this->peerManagers[0]->getPeer(this->peerManagers[1]->getID())->getID());
   QVERIFY(this->peerManagers[0]->getPeer(Common::Hash::rand()) == 0);
}

void Tests::askForRootEntries()
{
   qDebug() << "===== askForRootEntries() =====";

   Protos::Core::GetEntries getEntriesMessage;
   QSharedPointer<IGetEntriesResult> result = this->peerManagers[0]->getPeers()[0]->getEntries(getEntriesMessage);
   connect(result.data(), SIGNAL(result(Protos::Core::GetEntriesResult)), &this->resultListener, SLOT(entriesResult(Protos::Core::GetEntriesResult)));
   result->start();
   QTest::qWait(1000);
   QCOMPARE(this->resultListener.getNbEntriesResultReceived(), 1);
}

/**
  * Use the same socket as the previous request.
  */
void Tests::askForSomeEntries()
{
   qDebug() << "===== askForSomeEntries() =====";

   QVERIFY(!this->resultListener.getEntriesResultList().isEmpty());

   Protos::Core::GetEntries getEntriesMessage1;
   getEntriesMessage1.mutable_dir()->CopyFrom(this->resultListener.getEntriesResultList().last().entry(0));
   QSharedPointer<IGetEntriesResult> result1 = this->peerManagers[0]->getPeers()[0]->getEntries(getEntriesMessage1);
   connect(result1.data(), SIGNAL(result(Protos::Core::GetEntriesResult)), &this->resultListener, SLOT(entriesResult(Protos::Core::GetEntriesResult)));
   result1->start();
   QTest::qWait(1000);
   QCOMPARE(this->resultListener.getNbEntriesResultReceived(), 2);

   Protos::Core::GetEntries getEntriesMessage2;
   getEntriesMessage2.mutable_dir()->CopyFrom(this->resultListener.getEntriesResultList().last().entry(0));
   getEntriesMessage2.mutable_dir()->mutable_shared_dir()->CopyFrom(getEntriesMessage1.dir().shared_dir());
   QSharedPointer<IGetEntriesResult> result2 = this->peerManagers[0]->getPeers()[0]->getEntries(getEntriesMessage2);
   connect(result2.data(), SIGNAL(result(Protos::Core::GetEntriesResult)), &this->resultListener, SLOT(entriesResult(Protos::Core::GetEntriesResult)));
   result2->start();
   QTest::qWait(1000);
   QCOMPARE(this->resultListener.getNbEntriesResultReceived(), 3);
}

void Tests::askForHashes()
{
   qDebug() << "===== askForHashes() =====";

   // 1) Create a big file.
   {
      QFile file("sharedDirs/peer2/big.bin");
      file.open(QIODevice::WriteOnly);

      // To have four different hashes.
      for (int i = 0; i < 4; i++)
      {
         QByteArray randomData(32 * 1024 * 1024, i);
         file.write(randomData);
      }
   }
   QTest::qWait(200);

   Protos::Common::Entry fileEntry;
   fileEntry.set_type(Protos::Common::Entry_Type_FILE);
   fileEntry.set_path("/");
   fileEntry.set_name("big.bin");
   fileEntry.set_size(0);
   fileEntry.mutable_shared_dir()->CopyFrom(this->resultListener.getEntriesResultList().first().entry(0).shared_dir());

   QSharedPointer<IGetHashesResult> result = this->peerManagers[0]->getPeers()[0]->getHashes(fileEntry);
   connect(result.data(), SIGNAL(result(const Protos::Core::GetHashesResult&)), &this->resultListener, SLOT(result(const Protos::Core::GetHashesResult&)));
   connect(result.data(), SIGNAL(nextHash(const Common::Hash&)), &this->resultListener, SLOT(nextHash(const Common::Hash&)));
   result->start();
   QTest::qWait(5000);
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

   Common::Global::createFile("sharedDirs/peer1/subdir/a.txt");
   Common::Global::createFile("sharedDirs/peer1/subdir/b.txt");
   Common::Global::createFile("sharedDirs/peer1/subdir/c.txt");
   Common::Global::createFile("sharedDirs/peer1/d.txt");
   Common::Global::createFile("sharedDirs/peer1/e.txt");

   Common::Global::createFile("sharedDirs/peer2/subdir/f.txt");
   Common::Global::createFile("sharedDirs/peer2/subdir/g.txt");
   Common::Global::createFile("sharedDirs/peer2/subdir/h.txt");
   Common::Global::createFile("sharedDirs/peer2/i.txt");
   Common::Global::createFile("sharedDirs/peer2/j.txt");
   Common::Global::createFile("sharedDirs/peer2/k.txt");
}

void Tests::deleteAllFiles()
{
   Common::Global::recursiveDeleteDirectory("sharedDirs");
}

