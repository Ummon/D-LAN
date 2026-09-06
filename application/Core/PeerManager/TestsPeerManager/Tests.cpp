/**
  * D-LAN - A decentralized LAN file sharing software.
  * Copyright (C) 2010-2012 Greg Burri <greg.burri@gmail.com>
  *
  * This program is free software: you can redistribute it and/or modify
  * it under the terms of the GNU General Public License as published by
  * the Free Software Foundation, either version 3 of the License, or
  * (at your option) any later version.
  *
  * This program is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  *
  * You should have received a copy of the GNU General Public License
  * along with this program.  If not, see <http://www.gnu.org/licenses/>.
  */

#include <Tests.h>
using namespace PM;

#include <QtDebug>
#include <QStringList>
#include <QScopeGuard>

#include <Protos/core_protocol.pb.h>
#include <Protos/core_settings.pb.h>
#include <Protos/common.pb.h>

#include <Common/LogManager/Builder.h>
#include <Common/PersistentData.h>
#include <Common/Constants.h>
#include <Common/Global.h>
#include <Common/Settings.h>

#include <ResultListener.h>
#include <IGetEntriesResult.h>
#include <IGetHashesResult.h>
#include <priv/PeerManager.h>
#include <priv/PeerMessageSocket.h>

const int Tests::PORT = 59487;

/**
  * @class Tests
  *
  * Create some fileManager and associated peerManager.
  */

Tests::Tests()
{
}

void Tests::initTestCase()
{
   LM::Builder::initMsgHandler();

   qDebug() << "===== initTestCase() =====";
   try
   {
      QString tempFolder = Common::Global::setCurrentDirToTemp("PeerManagerTests");
      qDebug() << "Application directory path (where the persistent data is put) : " <<
         Common::Global::getDataFolder(Common::Global::DataFolderType::LOCAL, false);
      qDebug() << "The file created during this test are put in : " << tempFolder;
   }
   catch (Common::Global::UnableToSetTempDirException& e)
   {
      QFAIL(e.errorMessage.toUtf8());
   }

   // Common::PersistentData::rmValue(Common::Constants::FILE_CACHE, Common::Global::DataFolderType::LOCAL); // Reset the stored cache.

   QVERIFY(this->createInitialFiles());

   this->hashCaches << QSharedPointer<HC::IHashCache>(new MockHashCache()) << QSharedPointer<HC::IHashCache>(new MockHashCache());
   this->fileManagers << FM::Builder::newFileManager(this->hashCaches[0]) << FM::Builder::newFileManager(this->hashCaches[1]);

   this->peerIDs <<
      Common::Hash::fromStr("11111111111111111111111111111111111111111111111111111111").value() <<
      Common::Hash::fromStr("22222222222222222222222222222222222222222222222222222222").value();

   this->peerSharedDirs << "/sharedDirs/peer1/" << "/sharedDirs/peer2/";

   // 1) Create each peer manager.
   for (int i = 0; i < this->peerIDs.size(); i++)
   {
      SETTINGS.set("peer_id", this->peerIDs[i]);
      QSharedPointer<IPeerManager> peerManager = Builder::newPeerManager(this->fileManagers[i]);
      peerManager->setNick(QString("peer#%1").arg(i + 1));
      this->peerManagers << peerManager;
   }

   // 2) Set the shared directories.
   for (int i = 0; i < this->peerIDs.size(); i++)
   {
      this->fileManagers[i]->setSharedPaths(
         QList<FM::IFileManager::SharedPath>() <<
            FM::IFileManager::SharedPath{ QString(), QDir::currentPath().append(this->peerSharedDirs[i]) }
      );
   }

   // 3) Create the peer update (to simulate the periodic update).
   this->peerUpdater = new PeerUpdater(this->fileManagers, this->peerManagers, PORT);

   // 4) Create the servers to listen new TCP connections and forward them to the right peer manager.
   for (int i = 0; i < this->peerIDs.size(); i++)
   {
      this->servers << new TestServer(this->peerManagers[i], PORT + i);
   }
}

void Tests::updatePeers()
{
   qDebug() << "===== updatePeers() =====";

   qDebug() << "Header size: " << Common::MessageHeader::HEADER_SIZE;

   this->peerUpdater->start();

   // This test shouldn't take less than ~3 s.
   QElapsedTimer timer;
   timer.start();

   // Check if each peer knows the other.
   for (int i = 0; i < this->peerIDs.size(); i++)
   {
      QList<IPeer*> peers = this->peerManagers[i]->getPeers();

      // Wait peer knows other peers.
      if (peers.size() != this->peerIDs.size() - 1)
      {
         i--;
         QTest::qWait(100);
         if (timer.elapsed() > 3000)
            QFAIL("Update peers failed..");
         continue;
      }

      for (int j = 0; j < this->peerIDs.size(); j++)
      {
         if (j == i)
            continue;

         bool found = false;
         for (int k = 0; k < peers.size(); k++)
            if (peers[k]->getID() == this->peerManagers[j]->getSelf()->getID())
            {
               found = true;
               QCOMPARE(peers[k]->getNick(), this->peerManagers[j]->getSelf()->getNick());

               // Wait peer j knows peer k amount (amount increase concurrently during the scanning process).
               if (peers[k]->getSharingAmount() != this->fileManagers[j]->getAmount())
               {
                  k--;
                  QTest::qWait(100);
                  if (timer.elapsed() > 3000)
                     QFAIL("Sharing amount not equals..");
                  continue;
               }
               break;
            }

         QVERIFY(found);
      }
   }
}

void Tests::getPeerFromID()
{
   qDebug() << "===== getPeerFromID() =====";

   for (int i = 0; i < this->peerIDs.size(); i++)
   {
      for (int j = 0; j < this->peerIDs.size(); j++)
      {
         if (j == i)
            continue;

         QCOMPARE(
            this->peerManagers[i]->getSelf()->getID(),
            this->peerManagers[j]->getPeer(this->peerManagers[i]->getSelf()->getID())->getID()
         );
      }

      QVERIFY(this->peerManagers[i]->getPeer(Common::Hash::rand()) == nullptr);
   }
}

/**
  * Peer#1 asking for the root entries of peer#2.
  */
void Tests::askForRootEntries()
{
   qDebug() << "===== askForRootEntries() =====";

   Protos::Core::GetEntries getEntriesMessage;
   QSharedPointer<IGetEntriesResult> result = this->peerManagers[0]->getPeers()[0]->getEntries(getEntriesMessage);

   QVERIFY(!result.isNull());

   connect(result.data(), &IGetEntriesResult::result, &this->resultListener, &ResultListener::entriesResult);
   result->start();

   QElapsedTimer timer;
   timer.start();

   int nbEntriesReceived;
   while ((nbEntriesReceived = this->resultListener.getNbEntriesResultReceived(0)) != 1)
   {
      QTest::qWait(100);
      if (timer.elapsed() > 5000)
         QFAIL(
            QString("We don't receive the right number of root entry. Number received: %1")
               .arg(nbEntriesReceived).toUtf8()
         );
   }
}

/**
  * Peer#1 browsing the content of the first shared directory of peer#2.
  * Uses the same socket as the previous request.
  */
void Tests::askForSomeEntries()
{
   qDebug() << "===== askForSomeEntries() =====";

   QElapsedTimer timer;

   QVERIFY(!this->resultListener.getEntriesResultList().isEmpty());

   Protos::Core::GetEntries getEntriesMessage1;
   getEntriesMessage1.mutable_dirs()->add_entries()->CopyFrom(
      this->resultListener.getEntriesResultList().constLast().results(0).entries().entries(0)
   );
   QSharedPointer<IGetEntriesResult> result1 = this->peerManagers[0]->getPeers()[0]->getEntries(getEntriesMessage1);
   QVERIFY(!result1.isNull());
   connect(result1.data(), &IGetEntriesResult::result, &this->resultListener, &ResultListener::entriesResult);
   result1->start();

   timer.start();
   while (this->resultListener.getNbEntriesResultReceived(0) != 4)
   {
      QTest::qWait(100);
      if (timer.elapsed() > 3000)
         QFAIL("We don't receive the right number of entry after sending 'getEntriesMessage1'.");
   }

   Protos::Core::GetEntries getEntriesMessage2;
   Protos::Common::Entry* entry = getEntriesMessage2.mutable_dirs()->add_entries();
   entry->CopyFrom(this->resultListener.getEntriesResultList().constLast().results(0).entries().entries(0));
   entry->mutable_shared_entry()->CopyFrom(getEntriesMessage1.dirs().entries(0).shared_entry());
   QSharedPointer<IGetEntriesResult> result2 = this->peerManagers[0]->getPeers()[0]->getEntries(getEntriesMessage2);
   QVERIFY(!result2.isNull());
   connect(result2.data(), &IGetEntriesResult::result, &this->resultListener, &ResultListener::entriesResult);
   result2->start();

   timer.start();
   while (this->resultListener.getNbEntriesResultReceived(0) != 3)
   {
      QTest::qWait(100);
      if (timer.elapsed() > 3000)
         QFAIL("We don't receive the right number of entry after sending 'getEntriesMessage2'.");
   }
}

void Tests::askForHashes()
{
   qDebug() << "===== askForHashes() =====";

   const quint32 NUMBER_OF_CHUNK = 4;
   const quint32 CHUNK_SIZE = Common::Constants::CHUNK_SIZE;

   // 1) Create a big file.
   {
      QString filename("sharedDirs/peer2/big.bin");
      QFile file(filename);
      if (!file.open(QIODevice::WriteOnly))
      {
         QFAIL(QString("Can't open file: %1").arg(filename).toUtf8());
      }

      // To have four different hashes.
      for (quint32 i = 0; i < NUMBER_OF_CHUNK; i++)
      {
         QByteArray randomData(CHUNK_SIZE, i);
         file.write(randomData);
      }
   }

   QElapsedTimer timer;

   // Wait until the peer#2 see the right size of 'big.bin'.
   timer.start();
   while (this->fileManagers[1]->getAmount() < NUMBER_OF_CHUNK * CHUNK_SIZE)
   {
      QTest::qWait(100);
      if (timer.elapsed() > 10000)
         QFAIL("After adding the big file 'big.bin' the amount of data must be greater the 32KiB");
   }

   // Ask the ashes of "big.bin" from the first peer to the second one.
   Protos::Common::Entry fileEntry;
   fileEntry.set_type(Protos::Common::Entry_Type_FILE);
   fileEntry.set_path("/");
   fileEntry.set_name("big.bin");
   fileEntry.set_size(0); // No obligation to set the correct size.
   for (quint32 i = 0; i < NUMBER_OF_CHUNK; i++)
      fileEntry.add_chunks();
   // Sets the root directory.
   fileEntry.mutable_shared_entry()->CopyFrom(
      this->resultListener.getEntriesResultList().constFirst().results(0).entries().entries(0).shared_entry()
   );

   QSharedPointer<IGetHashesResult> result = this->peerManagers[0]->getPeers()[0]->getHashes(fileEntry);
   QVERIFY(!result.isNull());
   connect(result.data(), &IGetHashesResult::result, &this->resultListener, &ResultListener::hashesResult);
   connect(result.data(), &IGetHashesResult::nextHash, &this->resultListener, &ResultListener::nextHashResult);
   result->start();

   timer.start();
   while (
      this->resultListener.getLastGetHashesResult().status() != Protos::Core::GetHashesResult::OK &&
      this->resultListener.getLastGetHashesResult().nb_hash() != NUMBER_OF_CHUNK
   )
   {
      QTest::qWait(100);
      if (timer.elapsed() > 5000)
         QFAIL("We don't receive the correct receive after asking for hashes");
   }

   // Wait to have all the hashes.
   timer.start();
   while (this->resultListener.getNbHashReceivedFromLastGetHashes() != NUMBER_OF_CHUNK)
   {
      QTest::qWait(100);
      if (timer.elapsed() > 30000)
         QFAIL("We don't receive all the hashes");
   }
}

void Tests::validateChunkOffsets()
{
   const Common::Hash hash = this->resultListener.getLastReceivedHash();
   const auto chunk = this->fileManagers[1]->getChunk(hash);
   QVERIFY(!chunk.isNull());
   const quint32 knownBytes = chunk->getKnownBytes();
   QVERIFY(knownBytes > 0);

   // A local context disconnects the temporary upload handler even if an assertion fails.
   QObject context;
   QList<PM::GetChunkParams> forwarded;
   connect(this->peerManagers[1].data(), &IPeerManager::getChunks, &context,
      [&](const QList<PM::GetChunkParams>& params, const QSharedPointer<PM::ISocket>& socket)
      {
         forwarded = params;
         socket->finished();
      });

   // Cover the data boundary, the signed boundary, and values that used to become negative.
   for (quint32 offset : {knownBytes + 1, quint32(0x7fffffff), quint32(0x80000000), quint32(0xffffffff)})
   {
      for (bool includeValidChunk : {false, true})
      {
         forwarded.clear();
         Protos::Core::GetChunks request;
         auto* requested = request.add_chunks();
         requested->mutable_hash()->set_hash(hash.getData(), Common::Hash::HASH_SIZE);
         requested->set_offset(offset);
         if (includeValidChunk)
         {
            auto* valid = request.add_chunks();
            valid->mutable_hash()->set_hash(hash.getData(), Common::Hash::HASH_SIZE);
            valid->set_offset(knownBytes); // A valid empty range needs no raw data.
         }

         Protos::Core::GetChunksResult response;
         bool received = false;
         auto result = this->peerManagers[0]->getPeers()[0]->getChunks(request);
         QVERIFY(!result.isNull());
         QObject requestContext;
         connect(result.data(), &IGetChunksResult::result, &requestContext,
            [&](const Protos::Core::GetChunksResult& value) { response = value; received = true; });
         result->start();
         QTRY_VERIFY_WITH_TIMEOUT(received, 10000);
         QCOMPARE(response.results_size(), includeValidChunk ? 2 : 1);
         QCOMPARE(response.results(0).status(), Protos::Core::GetChunksResult::ChunkResult::DONT_HAVE_DATA_FROM_OFFSET);
         QCOMPARE(forwarded.size(), includeValidChunk ? 1 : 0);
         if (includeValidChunk)
         {
            QCOMPARE(response.status(), Protos::Core::GetChunksResult::OK);
            QCOMPARE(response.results(1).status(), Protos::Core::GetChunksResult::ChunkResult::OK);
            QCOMPARE(response.results(1).chunk_size(), knownBytes);
            QCOMPARE(forwarded.first().getOffset(), static_cast<int>(knownBytes));
            QCOMPARE(forwarded.first().getEndOffset(), static_cast<int>(knownBytes));
         }
         else
            QVERIFY(response.status() != Protos::Core::GetChunksResult::OK);

         // Results release their socket through deleteLater(). Complete that cleanup before
         // the next request so it cannot reuse a socket with a pending close notification.
         result.clear();
         QCoreApplication::sendPostedEvents(nullptr, QEvent::DeferredDelete);
         QCoreApplication::processEvents();
      }
   }
}

void Tests::uploadReservations()
{
   const quint32 globalLimit = SETTINGS.get<quint32>("upload_max_nb_connections");
   const quint32 peerLimit = SETTINGS.get<quint32>("upload_max_nb_connections_per_peer");
   const auto restoreSettings = qScopeGuard([&] {
      SETTINGS.set("upload_max_nb_connections", globalLimit);
      SETTINGS.set("upload_max_nb_connections_per_peer", peerLimit);
   });
   SETTINGS.set("upload_max_nb_connections", quint32(2));
   SETTINGS.set("upload_max_nb_connections_per_peer", quint32(1));

   auto* manager = static_cast<PM::PeerManager*>(this->peerManagers[1].data());
   auto makeSocket = [&](const Common::Hash& peerID) {
      return QSharedPointer<PM::PeerMessageSocket>(
         new PM::PeerMessageSocket(manager, this->fileManagers[1], peerID, new QTcpSocket()));
   };
   auto first = makeSocket(this->peerIDs[0]);
   auto samePeer = makeSocket(this->peerIDs[0]);
   auto otherPeer = makeSocket(this->peerIDs[1]);
   auto thirdPeer = makeSocket(Common::Hash(QByteArray(Common::Hash::HASH_SIZE, '\x33')));

   QVERIFY(manager->tryReserveUpload(first.data()));
   QVERIFY(!manager->tryReserveUpload(first.data())); // No double reservation.
   QVERIFY(!manager->tryReserveUpload(samePeer.data())); // Per-peer limit.
   QVERIFY(manager->tryReserveUpload(otherPeer.data())); // Another peer still has capacity.
   QVERIFY(!manager->tryReserveUpload(thirdPeer.data())); // Global limit.

   first->close();
   QVERIFY(!manager->tryReserveUpload(samePeer.data())); // Closing must not free a running worker's slot.
   first->finished(true);
   QVERIFY(manager->tryReserveUpload(samePeer.data())); // Completion releases even an inactive socket.
   samePeer->finished();
   samePeer->finished(); // Releasing twice must not decrement another upload's count.
   QVERIFY(!manager->tryReserveUpload(makeSocket(this->peerIDs[1]).data()));
   QVERIFY(manager->tryReserveUpload(thirdPeer.data()));
   otherPeer.clear(); // Destruction is a fallback if no uploader calls finished().
   QVERIFY(manager->tryReserveUpload(first.data()));
}

void Tests::rejectExcessUploads()
{
   const quint32 globalLimit = SETTINGS.get<quint32>("upload_max_nb_connections");
   const quint32 peerLimit = SETTINGS.get<quint32>("upload_max_nb_connections_per_peer");
   const auto restoreSettings = qScopeGuard([&] {
      SETTINGS.set("upload_max_nb_connections", globalLimit);
      SETTINGS.set("upload_max_nb_connections_per_peer", peerLimit);
   });

   const Common::Hash hash = this->resultListener.getLastReceivedHash();
   const auto chunk = this->fileManagers[1]->getChunk(hash);
   QVERIFY(!chunk.isNull());
   Protos::Core::GetChunks request;
   auto* requested = request.add_chunks();
   requested->mutable_hash()->set_hash(hash.getData(), Common::Hash::HASH_SIZE);
   requested->set_offset(chunk->getKnownBytes());

   QList<QSharedPointer<PM::ISocket>> heldUploads;
   QObject context;
   connect(this->peerManagers[1].data(), &IPeerManager::getChunks, &context,
      [&](const QList<PM::GetChunkParams>&, const QSharedPointer<PM::ISocket>& socket) {
         heldUploads << socket;
      });
   const auto finishUploads = qScopeGuard([&] {
      for (const auto& socket : heldUploads)
         socket->finished(true);
   });

   // Exercise each limit independently through the wire protocol.
   for (bool global : {false, true})
   {
      SETTINGS.set("upload_max_nb_connections", quint32(global ? 1 : 2));
      SETTINGS.set("upload_max_nb_connections_per_peer", quint32(global ? 2 : 1));
      QList<QSharedPointer<IGetChunksResult>> results;
      for (int attempt = 0; attempt < 3; ++attempt)
      {
         bool received = false;
         bool streamReceived = false;
         Protos::Core::GetChunksResult response;
         QObject requestContext;
         auto result = this->peerManagers[0]->getPeers()[0]->getChunks(request);
         QVERIFY(!result.isNull());
         results << result;
         connect(result.data(), &IGetChunksResult::result, &requestContext,
            [&](const Protos::Core::GetChunksResult& value) { response = value; received = true; });
         connect(result.data(), &IGetChunksResult::stream, &requestContext,
            [&](const QSharedPointer<PM::ISocket>&) { streamReceived = true; });
         result->start();
         QTRY_VERIFY_WITH_TIMEOUT(received, 10000);
         if (attempt == 1)
         {
            QCOMPARE(response.status(), Protos::Core::GetChunksResult::TOO_MANY_CONNECTIONS);
            QVERIFY(!streamReceived);
            QCOMPARE(heldUploads.size(), 1); // No second uploader was dispatched.
            heldUploads.takeFirst()->finished(global); // Test normal and error completion.
         }
         else
         {
            QCOMPARE(response.status(), Protos::Core::GetChunksResult::OK);
            QVERIFY(streamReceived);
            QCOMPARE(heldUploads.size(), 1); // Capacity is available again on attempt 2.
         }
      }
      heldUploads.takeFirst()->finished(true);
      for (const auto& result : results)
         result->setStatus(true);
      results.clear();
      QCoreApplication::sendPostedEvents(nullptr, QEvent::DeferredDelete);
      QCoreApplication::processEvents();
   }
}

void Tests::askForAChunk()
{
   qDebug() << "===== askForAChunk() =====";

   connect(this->peerManagers[1].data(), &IPeerManager::getChunks, &this->resultListener, &ResultListener::getChunks);

   Protos::Core::GetChunks getChunksMessage;
   getChunksMessage.add_chunks()->mutable_hash()->set_hash(
      this->resultListener.getLastReceivedHash().getData(),
      Common::Hash::HASH_SIZE
   );
   getChunksMessage.mutable_chunks(0)->set_offset(0);
   QSharedPointer<IGetChunksResult> result = this->peerManagers[0]->getPeers()[0]->getChunks(getChunksMessage);
   QVERIFY(!result.isNull());
   connect(result.data(), &IGetChunksResult::result, &this->resultListener, &ResultListener::chunksResult);
   connect(result.data(), &IGetChunksResult::stream, &this->resultListener, &ResultListener::stream);
   result->start();

   QElapsedTimer timer;
   timer.start();
   while (!this->resultListener.isStreamReceived())
   {
      QTest::qWait(100);
      if (timer.elapsed() > 10000)
         QFAIL("We don't receive the stream");
   }
}

// TODO
// void Tests::askForMultipleChunks()
// {

// }

void Tests::cleanupTestCase()
{
   qDebug() << "===== cleanupTestCase() =====";

   for (QListIterator<TestServer*> i(this->servers); i.hasNext();)
      delete i.next();

   delete this->peerUpdater;
}

bool Tests::createInitialFiles()
{
   qDebug() << "Create the directories structure in" << QDir::currentPath();

   if (!this->deleteAllFiles())
      return false;

   return
      Common::Global::createFile("sharedDirs/peer1/subdir/a.txt") &&
      Common::Global::createFile("sharedDirs/peer1/subdir/b.txt") &&
      Common::Global::createFile("sharedDirs/peer1/subdir/c.txt") &&
      Common::Global::createFile("sharedDirs/peer1/d.txt") &&
      Common::Global::createFile("sharedDirs/peer1/e.txt") &&

      Common::Global::createFile("sharedDirs/peer2/subdir/f.txt") &&
      Common::Global::createFile("sharedDirs/peer2/subdir/g.txt") &&
      Common::Global::createFile("sharedDirs/peer2/subdir/h.txt") &&
      Common::Global::createFile("sharedDirs/peer2/i.txt") &&
      Common::Global::createFile("sharedDirs/peer2/j.txt") &&
      Common::Global::createFile("sharedDirs/peer2/k.txt");
}

bool Tests::deleteAllFiles()
{
   return Common::Global::recursiveDeleteDirectory("sharedDirs");
}
