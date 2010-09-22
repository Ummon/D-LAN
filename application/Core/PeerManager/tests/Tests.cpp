#include <Tests.h>
using namespace PM;

#include <QtDebug>
#include <QtNetwork>

#include <Common/LogManager/Builder.h>
#include <Common/PersistantData.h>
#include <Common/Constants.h>
#include <Common/Global.h>
#include <Common/Network.h>

#include <Constants.h>

Tests::Tests()
{
}

void Tests::initTestCase()
{
   LM::Builder::initMsgHandler();
   qDebug() << "===== initTestCase() =====";

   Common::PersistantData::rmValue(Common::FILE_CACHE); // Reset the stored cache.
   qDebug() << Common::Global::availableDiskSpace("C:\\");

   this->fileManager = FM::Builder::newFileManager();
   this->peerManager = Builder::newPeerManager(this->fileManager);

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

void Tests::newConnection()
{
   qDebug() << "===== newConnection() =====";

   QTcpSocket socket;
   connect(&socket, SIGNAL(error(QAbstractSocket::SocketError)), this, SLOT(socketError(QAbstractSocket::SocketError)));
   socket.connectToHost(QHostAddress::LocalHost, PORT);
   if (socket.waitForConnected())
   {
      Common::MessageHeader header;
      header.type = 0;
      header.size = 0;
      header.senderID = Common::Hash::rand();
      Common::Network::writeHeader(socket, header);
      socket.flush();
      QTest::qWait(1000);
   }
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


