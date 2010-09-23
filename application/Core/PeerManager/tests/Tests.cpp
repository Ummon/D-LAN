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
   this->fileManager = FM::Builder::newFileManager();

   QStringList sharedDirs;
   sharedDirs << QDir::currentPath().append("/sharedDirs/share1") << QDir::currentPath().append("/sharedDirs/share2");
   this->fileManager->setSharedDirsReadOnly(sharedDirs);

   this->peerManager = Builder::newPeerManager(this->fileManager);

   this->socket = new QTcpSocket();
   this->server = new TestServer(this->peerManager);
}

void Tests::getId()
{
   qDebug() << "===== getId() =====";

   Common::Hash ID = this->peerManager->getID();
   QVERIFY(!ID.isNull());
   qDebug() << "Current ID: " << ID.toStr();
}

void Tests::setGetNick()
{
   qDebug() << "===== setGetNick() =====";

   this->peerManager->setNick("Bob");
   QVERIFY(this->peerManager->getNick() == "Bob");
}

void Tests::updatePeers()
{
   qDebug() << "===== updatePeers() =====";

   const int NUMBER_OF_PEER = 3;
   this->peerUpdater = new PeerUpdater(this->peerManager, NUMBER_OF_PEER);
   QTest::qWait(1000);

   QList<PeerData> peers = this->peerUpdater->getPeers();
   QCOMPARE(peers.size(), NUMBER_OF_PEER);

   int n = 0;
   for (QListIterator<IPeer*> i(this->peerManager->getPeers()); i.hasNext();)
   {
      IPeer* peer = i.next();
      for (QMutableListIterator<PeerData> j(peers); j.hasNext();)
      {
         PeerData peerData = j.next();
         if (peerData.ID == peer->getID())
         {
            QCOMPARE(peerData.IP, peer->getIP());
            QCOMPARE(peerData.nick, peer->getNick());
            QCOMPARE(peerData.sharingAmount, peer->getSharingAmount());
            n += 1;
            j.remove();
            break;
         }
      }
   }

   QCOMPARE(n, NUMBER_OF_PEER);

   this->peerUpdater->stop();
}

void Tests::getPeerFromID()
{
   qDebug() << "===== getPeerFromID() =====";

   QList<PeerData> peers = this->peerUpdater->getPeers();
   for (QMutableListIterator<PeerData> j(peers); j.hasNext();)
   {
      PeerData peerData = j.next();
      IPeer* peer = this->peerManager->getPeer(peerData.ID);

      QVERIFY(peer);

      QCOMPARE(peerData.IP, peer->getIP());
      QCOMPARE(peerData.nick, peer->getNick());
      QCOMPARE(peerData.sharingAmount, peer->getSharingAmount());
   }

   QVERIFY(this->peerManager->getPeer(Common::Hash::rand()) == 0);
}

void Tests::connectToServer()
{
   qDebug() << "===== connectToServer() =====";

   connect(this->socket, SIGNAL(error(QAbstractSocket::SocketError)), this, SLOT(socketError(QAbstractSocket::SocketError)));
   this->socket->connectToHost(QHostAddress::LocalHost, PORT);
   QVERIFY(this->socket->waitForConnected());
}

void Tests::askForRootEntries()
{
   qDebug() << "===== askForRootEntries() =====";

   Protos::Core::GetEntries getEntriesMessage;
   this->sendMessage(this->peerUpdater->getPeers().first(), 0x31, getEntriesMessage);
   QVERIFY(!this->getEntriesResultList.isEmpty());
   qDebug() << QString::fromStdString(this->getEntriesResultList.last().DebugString());
}

/**
  * Use the same socket as the previous request.
  */
void Tests::askForSomeEntries()
{
   qDebug() << "===== askForSomeEntries() =====";

   QVERIFY(!this->getEntriesResultList.isEmpty());

   Protos::Core::GetEntries getEntriesMessage1;
   getEntriesMessage1.mutable_dir()->CopyFrom(this->getEntriesResultList.last().entry(0));
   this->sendMessage(this->peerUpdater->getPeers().first(), 0x31, getEntriesMessage1);
   QVERIFY(!this->getEntriesResultList.isEmpty());
   qDebug() << QString::fromStdString(this->getEntriesResultList.last().DebugString());

   Protos::Core::GetEntries getEntriesMessage2;
   getEntriesMessage2.mutable_dir()->CopyFrom(this->getEntriesResultList.last().entry(0));
   getEntriesMessage2.mutable_dir()->mutable_shared_dir()->CopyFrom(getEntriesMessage1.dir().shared_dir());
   this->sendMessage(this->peerUpdater->getPeers().first(), 0x31, getEntriesMessage2);
   QVERIFY(!this->getEntriesResultList.isEmpty());
   qDebug() << QString::fromStdString(this->getEntriesResultList.last().DebugString());
}

void Tests::cleanupTestCase()
{
   qDebug() << "===== cleanupTestCase() =====";

   delete server;
   delete peerUpdater;
}

void Tests::socketError(QAbstractSocket::SocketError error)
{
   qDebug() << "Error : " << error;
}


void Tests::sendMessage(const PeerData& peer, quint32 type, const google::protobuf::Message& message)
{
   Common::Network::writeHeader(*this->socket, Common::MessageHeader(type, message.ByteSize(), peer.ID));
   Common::ZeroCopyOutputStreamQIODevice outputStream(this->socket);
   message.SerializeToZeroCopyStream(&outputStream);

   QTest::qWait(500);
   QVERIFY(this->socket->bytesAvailable());
   QTest::qWait(500);

   Common::MessageHeader header = Common::Network::readHeader(*this->socket);
   qDebug() << "Data received : type = " << header.type << " size = " << header.size << " senderId = " << header.senderID.toStr();

   Common::ZeroCopyInputStreamQIODevice inputStream(this->socket);
   switch (type)
   {
   case 0x31 :
      Protos::Core::GetEntriesResult result;
      result.ParseFromZeroCopyStream(&inputStream);
      this->getEntriesResultList << result;
   }
}

void Tests::createInitialFiles()
{
   this->deleteAllFiles();

   Common::Global::createFile("sharedDirs/share1/subdir/a.txt");
   Common::Global::createFile("sharedDirs/share1/subdir/b.txt");
   Common::Global::createFile("sharedDirs/share1/subdir/c.txt");
   Common::Global::createFile("sharedDirs/share1/d.txt");
   Common::Global::createFile("sharedDirs/share1/e.txt");

   Common::Global::createFile("sharedDirs/share2/f.txt");
   Common::Global::createFile("sharedDirs/share2/g.txt");
   Common::Global::createFile("sharedDirs/share2/h.txt");
}

void Tests::deleteAllFiles()
{
   Common::Global::recursiveDeleteDirectory("sharedDirs");
}

