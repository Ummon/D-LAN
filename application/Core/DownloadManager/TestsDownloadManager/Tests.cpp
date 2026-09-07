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
using namespace DM;

#include <QtDebug>
#include <QStringList>

#include <Protos/core_protocol.pb.h>
#include <Protos/core_settings.pb.h>
#include <Protos/common.pb.h>

#include <Common/LogManager/Builder.h>
#include <Common/Global.h>

#include <Builder.h>
#include <QTemporaryDir>
#include <Common/Constants.h>
#include <Core/FileManager/Exceptions.h>
#include <Core/FileManager/priv/Exceptions.h>
#include <Core/FileManager/priv/Cache/Cache.h>
#include <Core/FileManager/priv/Cache/File.h>
#include <Core/PeerManager/priv/Peer.h>
#include <priv/FileDownload.h>
#include <priv/DownloadQueue.h>
#include <priv/DownloadManager.h>
#include <Common/PersistentData.h>
#include <memory>

namespace
{
   class RetryDownload : public Download
   {
   public:
      RetryDownload(QSharedPointer<FM::IFileManager> files, PM::IPeer* peer) :
         Download(files, peer, Protos::Common::Entry(), Protos::Common::Entry()) {}
      void start() override { this->setStatus(Protos::Common::DownloadStatus::QUEUED); }
      using Download::setStatus;
   };

   class EmptyHashCache : public HC::IHashCache
   {
   public:
      QList<Common::Hash> getHashes(const QString&, QDateTime) override { return {}; }
      void setHashes(const QString&, const QList<Common::Hash>&, qint64, QDateTime) override {}
      void rmHashes(const QString&) override {}
   };

   class ResumeFileManager : public MockFileManager
   {
   public:
      QList<QSharedPointer<FM::IChunk>> chunks;
      QList<QSharedPointer<FM::IChunk>> getAllChunks(const Protos::Common::Entry&,
         const QList<Common::Hash>&) const override { return this->chunks; }
   };

   class ResumePeer : public PM::Peer
   {
   public:
      ResumePeer(QSharedPointer<FM::IFileManager> files) : Peer(nullptr, files, Common::Hash::rand(), "source") {}
      bool isAvailable() const override { return true; }
   };

   class DestinationFileManager : public ResumeFileManager
   {
   public:
      DestinationFileManager(FM::Cache& cache) : cache(cache) {}
      QList<QSharedPointer<FM::IChunk>> newFile(Protos::Common::Entry& entry) override
      {
         return this->cache.newFile(entry);
      }
   private:
      FM::Cache& cache;
   };

   class PendingChunksResult : public PM::IGetChunksResult
   {
   public:
      PendingChunksResult() : IGetChunksResult(60000) {}
      void start() override {} // Keep the transfer pending without opening a socket.
      void doDeleteLater() override { this->deleteLater(); }
      void setStatus(bool) override {}
   };

   class CheckpointPeer : public ResumePeer
   {
   public:
      using ResumePeer::ResumePeer;
      bool available = true;
      bool isAvailable() const override { return this->available; }
      QSharedPointer<PM::IGetChunksResult> getChunks(const Protos::Core::GetChunks&) override
      {
         return QSharedPointer<PM::IGetChunksResult>(new PendingChunksResult);
      }
   };
}

void Tests::resetPreservesDestination_data()
{
   QTest::addColumn<bool>("reloadQueue");
   QTest::newRow("resume") << false;
   QTest::newRow("reload-queue") << true;
}

void Tests::resetPreservesDestination()
{
   QFETCH(bool, reloadQueue);
   FM::Chunk::CHUNK_SIZE = Common::Constants::CHUNK_SIZE;
   QTemporaryDir temp;
   QVERIFY(temp.isValid());
   QVERIFY(QDir(temp.path()).mkdir("default"));
   QVERIFY(QDir(temp.path()).mkdir("chosen"));
   FM::Cache cache(QSharedPointer<HC::IHashCache>(new EmptyHashCache));
   cache.addASharedPath(temp.path() + "/default/");
   const auto chosen = cache.addASharedPath(temp.path() + "/chosen/");
   auto root = dynamic_cast<FM::SharedDirectory*>(cache.getSharedEntry(chosen.first.ID));
   QVERIFY(root);
   auto file = new FM::File(root, "destination.bin", 100, false, QDateTime::currentDateTime(),
      root->getRootDir(), { Common::Hash::rand() }, true);
   const auto chunk = file->getChunks().first();
   chunk->setKnownBytes(25);
   const auto originalPath = chunk->getFilePath().toString();
   Protos::Common::Entry entry;
   file->populateEntry(&entry, true);
   entry.set_name("destination.bin");
   QSharedPointer<DestinationFileManager> files(new DestinationFileManager(cache));
   files->chunks << chunk;
   ResumePeer peer(files);
   LinkedPeers links;
   OccupiedPeers asking, downloading;
   Common::ThreadPool pool(1);
   Common::TransferRateCalculator rate;
   auto download = std::make_unique<FileDownload>(files, links, asking, downloading, pool, &peer,
      entry, entry, rate, Protos::Queue::Queue::Entry::PAUSED);
   download->start();
   QCOMPARE(download->getDownloadedBytes(), quint64(25));

   // Simulate the scanner removing the cached file while its share remains available.
   file->removeUnfinishedFiles();
   file->del(false);
   delete file;
   files->chunks.clear();
   QVERIFY(chunk->getFilePath().isNull());
   QVERIFY(download->pause(false));
   QCOMPARE(download->getDownloadedBytes(), quint64(0));
   QVERIFY(!download->getLocalEntry().exists());

   Protos::Queue::Queue::Entry saved;
   download->populateQueueEntry(&saved);
   if (reloadQueue)
   {
      download.reset();
      download = std::make_unique<FileDownload>(files, links, asking, downloading, pool, &peer,
         saved.remote_entry(), saved.local_entry(), rate, saved.status());
      download->start();
   }
   const auto next = download->getAChunkToDownload();
   QVERIFY(next);
   QVERIFY(next->getChunk());
   QCOMPARE(next->getChunk()->getFilePath().toString(), originalPath);
   QCOMPARE(next->getChunk()->getKnownBytes(), 0);
   QVERIFY(QFileInfo::exists(originalPath));
   QCOMPARE(saved.local_entry().shared_entry().id().hash(), entry.shared_entry().id().hash());
}

void Tests::checkpointDownloadProgress_data()
{
   QTest::addColumn<bool>("complete");
   QTest::newRow("completed") << true;
   QTest::newRow("interrupted") << false;
}

void Tests::retryFailedQueueSave()
{
   QSharedPointer<MockFileManager> files(new MockFileManager);
   ResumePeer peer(files);
   // Persistence is isolated by main.cpp in a temporary directory.
   Common::PersistentData::rmValue(Common::Constants::FILE_QUEUE, Common::Global::DataFolderType::LOCAL);
   DownloadManager manager(files, this->peerManager);
   emit files->fileCacheScanningComplete();
   Protos::Common::Entry entry;
   entry.set_type(Protos::Common::Entry::FILE);
   entry.set_name("paused.bin");
   entry.set_size(100);
   auto download = manager.addDownload(entry, entry, &peer, Protos::Queue::Queue::Entry::PAUSED);
   QVERIFY(download);
   QVERIFY(QMetaObject::invokeMethod(&manager, "saveQueueToFile", Qt::DirectConnection));
   QCOMPARE(DownloadQueue::loadFromFile().entries_size(), 1);

   manager.removeDownloads({ download->getID() });
   // A directory at the temporary file path reliably prevents writing on all platforms,
   // without depending on filesystem permissions or exhausting disk space.
   const auto blockedPath = Common::Global::getDataFolder(Common::Global::DataFolderType::LOCAL)
      + '/' + Common::Constants::FILE_QUEUE + ".temp";
   QVERIFY(QDir().mkdir(blockedPath));
   const bool firstAttempt = QMetaObject::invokeMethod(&manager, "saveQueueToFile", Qt::DirectConnection);
   const bool secondAttempt = QMetaObject::invokeMethod(&manager, "saveQueueToFile", Qt::DirectConnection);
   QVERIFY(QDir().rmdir(blockedPath));
   QVERIFY(firstAttempt);
   QVERIFY(secondAttempt);
   QCOMPARE(DownloadQueue::loadFromFile().entries_size(), 1); // The previous checkpoint survived.

   // No new queue edits or active transfers: only the retained dirty flag can trigger this save.
   QVERIFY(QMetaObject::invokeMethod(&manager, "saveQueueToFile", Qt::DirectConnection));
   QCOMPARE(DownloadQueue::loadFromFile().entries_size(), 0);
}

void Tests::checkpointDownloadProgress()
{
   QFETCH(bool, complete);
   FM::Chunk::CHUNK_SIZE = Common::Constants::CHUNK_SIZE;
   QTemporaryDir temp;
   QVERIFY(temp.isValid());
   FM::Cache cache(QSharedPointer<HC::IHashCache>(new EmptyHashCache));
   const auto shared = cache.addASharedPath(temp.path() + '/');
   auto root = dynamic_cast<FM::SharedDirectory*>(cache.getSharedEntry(shared.first.ID));
   QVERIFY(root);
   auto file = new FM::File(root, "checkpoint.bin", 100, false, QDateTime::currentDateTime(),
      root->getRootDir(), { Common::Hash::rand() }, true);
   const auto chunk = file->getChunks().first();
   QSharedPointer<ResumeFileManager> files(new ResumeFileManager);
   files->chunks << chunk;
   CheckpointPeer peer(files);
   // The test executable redirects persistence into its own temporary directory.
   Common::PersistentData::rmValue(Common::Constants::FILE_QUEUE, Common::Global::DataFolderType::LOCAL);
   DownloadManager manager(files, this->peerManager);
   emit files->fileCacheScanningComplete();
   Protos::Common::Entry entry;
   file->populateEntry(&entry, true);
   auto download = manager.addDownload(entry, entry, &peer, Protos::Queue::Queue::Entry::QUEUED);
   QVERIFY(download);
   QCOMPARE(download->getStatus(), Protos::Common::DownloadStatus::DOWNLOADING);
   const auto unfinished = manager.getTheFirstUnfinishedChunks(1);
   QCOMPARE(unfinished.size(), 1);
   const auto downloader = qSharedPointerDynamicCast<ChunkDownloader>(unfinished.first());
   QVERIFY(downloader);

   // Exercise the timer's slot directly, without waiting a minute per checkpoint.
   for (int bytes : { 10, 20, 30 })
   {
      chunk->setKnownBytes(bytes);
      QVERIFY(QMetaObject::invokeMethod(&manager, "saveQueueToFile", Qt::DirectConnection));
      const auto saved = DownloadQueue::loadFromFile();
      QCOMPARE(saved.entries_size(), 1);
      QCOMPARE(saved.entries(0).known_bytes_size(), 1);
      QCOMPARE(saved.entries(0).known_bytes(0), bytes);
      QCOMPARE(saved.entries(0).local_entry().shared_entry().id().hash(), entry.shared_entry().id().hash());
   }

   const int finalBytes = complete ? 100 : 40;
   chunk->setKnownBytes(finalBytes);
   peer.available = false; // An interrupted transfer must not immediately restart.
   downloader->stop();
   QVERIFY(!downloader->isDownloading());
   QVERIFY(QMetaObject::invokeMethod(&manager, "saveQueueToFile", Qt::DirectConnection));
   const auto saved = DownloadQueue::loadFromFile();
   QCOMPARE(saved.entries_size(), 1);
   QCOMPARE(saved.entries(0).known_bytes_size(), 1);
   QCOMPARE(saved.entries(0).known_bytes(0), finalBytes);
   QCOMPARE(saved.entries(0).status(), complete ? Protos::Queue::Queue::Entry::COMPLETE : Protos::Queue::Queue::Entry::QUEUED);
}

void Tests::erroneousDownloadsAreUnique()
{
   ResumePeer peer(this->fileManager);
   DownloadQueue queue;
   auto first = new RetryDownload(this->fileManager, &peer);
   auto second = new RetryDownload(this->fileManager, &peer);
   for (auto download : { first, second })
   {
      queue.insert(queue.size(), download);
      connect(download, &Download::becomeErroneous, &queue, &DownloadQueue::setDownloadAsErroneous);
      download->setStatus(Protos::Common::DownloadStatus::TRANSFER_ERROR);
   }

   // Recovery followed by another error before the retry timer consumes the entry.
   first->start();
   first->setStatus(Protos::Common::DownloadStatus::TRANSFER_ERROR);
   QCOMPARE(queue.getAnErroneousDownload(), first);
   QCOMPARE(queue.getAnErroneousDownload(), second);
   QVERIFY(!queue.getAnErroneousDownload());

   // Once consumed, a failed retry must be eligible for scheduling again.
   first->start();
   first->setStatus(Protos::Common::DownloadStatus::TRANSFER_ERROR);
   QCOMPARE(queue.getAnErroneousDownload(), first);
   QVERIFY(!queue.getAnErroneousDownload());
}

void Tests::removeErroneousDownload_data()
{
   QTest::addColumn<bool>("bulkRemoval");
   QTest::newRow("single") << false;
   QTest::newRow("bulk") << true;
}

void Tests::removeErroneousDownload()
{
   QFETCH(bool, bulkRemoval);
   ResumePeer peer(this->fileManager);
   DownloadQueue queue;
   auto download = new RetryDownload(this->fileManager, &peer);
   queue.insert(0, download);
   connect(download, &Download::becomeErroneous, &queue, &DownloadQueue::setDownloadAsErroneous);
   download->setStatus(Protos::Common::DownloadStatus::TRANSFER_ERROR);
   download->start();
   download->setStatus(Protos::Common::DownloadStatus::TRANSFER_ERROR);

   if (bulkRemoval)
      QVERIFY(queue.removeDownloads(IsContainedInAList({ download->getID() })));
   else
   {
      queue.remove(0);
      delete download;
   }

   QCOMPARE(queue.size(), 0);
   // Never dereference the result: before the fix it points to the deleted download.
   QVERIFY(!queue.getAnErroneousDownload());
}

void Tests::oldestChunksSkipUnavailableDownloads_data()
{
   QTest::addColumn<bool>("paused");
   QTest::newRow("paused") << true;
   QTest::newRow("unknown-hashes") << false;
}

void Tests::oldestChunksSkipUnavailableDownloads()
{
   QFETCH(bool, paused);
   ResumePeer peer(this->fileManager);
   LinkedPeers links;
   OccupiedPeers asking, downloading;
   Common::ThreadPool pool(1);
   Common::TransferRateCalculator rate;
   DownloadQueue queue;
   const auto activeHash = Common::Hash::rand();
   const auto blockedHash = Common::Hash::rand();
   auto addFile = [&](const char* name, int chunks, const Common::Hash& hash, bool pause)
   {
      Protos::Common::Entry entry;
      entry.set_type(Protos::Common::Entry::FILE);
      entry.set_name(name);
      entry.set_size(quint64(chunks) * Common::Constants::CHUNK_SIZE);
      for (int i = 0; i < chunks; ++i)
      {
         auto chunk = entry.add_chunks();
         if (!hash.isNull())
            chunk->set_hash(hash.getData(), Common::Hash::HASH_SIZE);
      }
      auto file = new FileDownload(this->fileManager, links, asking, downloading, pool, &peer,
         entry, entry, rate, pause ? Protos::Queue::Queue::Entry::PAUSED : Protos::Queue::Queue::Entry::QUEUED);
      queue.insert(queue.size(), file);
      return file;
   };

   auto active = addFile("active", 2, activeHash, false);
   // Advance the active file's timestamp so every unavailable file is older,
   // without relying on the ordering of equal timestamp keys.
   QList<QSharedPointer<IChunkDownloader>> initialChunks;
   active->getUnfinishedChunks(initialChunks, 2);
   QCOMPARE(initialChunks.size(), 2);
   const auto activeTime = active->getLastTimeGetAllUnfinishedChunks();
   auto blocked = addFile("blocked", 1, paused ? blockedHash : Common::Hash(), paused);
   addFile("blocked-too", 1, paused ? blockedHash : Common::Hash(), paused);

   QVERIFY(queue.getTheOldestUnfinishedChunks(0).isEmpty());
   QVERIFY(queue.getTheOldestUnfinishedChunks(-1).isEmpty());
   QCOMPARE(active->getLastTimeGetAllUnfinishedChunks(), activeTime);
   QCOMPARE(blocked->getLastTimeGetAllUnfinishedChunks(), qint64(0));

   for (int attempt = 0; attempt < 2; ++attempt)
   {
      const auto chunks = queue.getTheOldestUnfinishedChunks(2);
      QCOMPARE(chunks.size(), 2);
      QCOMPARE(chunks[0]->getHash(), activeHash);
      QCOMPARE(chunks[1]->getHash(), activeHash);
      QVERIFY(chunks[0] != chunks[1]);
   }

   if (paused)
   {
      // Paused files must remain indexed so resuming makes them discoverable again.
      QCOMPARE(blocked->getLastTimeGetAllUnfinishedChunks(), qint64(0));
      QVERIFY(blocked->pause(false));
      const auto chunks = queue.getTheOldestUnfinishedChunks(1);
      QCOMPARE(chunks.size(), 1);
      QCOMPARE(chunks[0]->getHash(), blockedHash);
   }
}

void Tests::resumeMissingFile_data()
{
   QTest::addColumn<bool>("removeFile");
   QTest::newRow("deleted") << true;
   QTest::newRow("still-present") << false;
}

void Tests::resumeMissingFile()
{
   QFETCH(bool, removeFile);
   FM::Chunk::CHUNK_SIZE = Common::Constants::CHUNK_SIZE;
   QTemporaryDir temp;
   QVERIFY(temp.isValid());
   FM::Cache cache(QSharedPointer<HC::IHashCache>(new EmptyHashCache));
   const auto shared = cache.addASharedPath(temp.path() + '/');
   auto root = dynamic_cast<FM::SharedDirectory*>(cache.getSharedEntry(shared.first.ID));
   QVERIFY(root);
   const qint64 size = qint64(FM::Chunk::CHUNK_SIZE) + 10;
   const QList<Common::Hash> hashes { Common::Hash::rand(), Common::Hash::rand() };
   auto file = new FM::File(root, "resumed.bin", size, false, QDateTime::currentDateTime(),
      root->getRootDir(), hashes, true);
   const auto chunks = file->getChunks();
   chunks[0]->setKnownBytes(FM::Chunk::CHUNK_SIZE); // A previously completed chunk must reset too.
   chunks[1]->setKnownBytes(5);
   const QString path = file->getAbsolutePath();
   QSharedPointer<ResumeFileManager> files(new ResumeFileManager);
   for (const auto& chunk : chunks)
      files->chunks << chunk;
   ResumePeer peer(files);
   DM::LinkedPeers links;
   DM::OccupiedPeers asking, downloading;
   Common::ThreadPool pool(1);
   Common::TransferRateCalculator rate;
   Protos::Common::Entry entry;
   file->populateEntry(&entry, true);
   entry.set_name("resumed.bin");
   DM::FileDownload download(files, links, asking, downloading, pool, &peer, entry, entry,
      rate, Protos::Queue::Queue::Entry::PAUSED);
   download.start();
   QCOMPARE(download.getDownloadedBytes(), quint64(size - 5));
   if (removeFile)
      QVERIFY(QFile::remove(path));
   QVERIFY(download.pause(false));
   QVERIFY(QFileInfo::exists(path));
   QCOMPARE(QFileInfo(path).size(), size);
   QCOMPARE(download.getDownloadedBytes(), removeFile ? quint64(0) : quint64(size - 5));
   auto next = download.getAChunkToDownload();
   QVERIFY(next);
   QCOMPARE(next->getChunk()->getKnownBytes(), removeFile ? 0 : 5);
   if (removeFile)
      for (const auto& chunk : chunks)
      {
         QCOMPARE(chunk->getKnownBytes(), 0);
         QVERIFY(!chunk->isComplete());
      }
   Protos::Queue::Queue::Entry saved;
   download.populateQueueEntry(&saved);
   QCOMPARE(saved.local_entry().shared_entry().id().hash(), entry.shared_entry().id().hash());
   if (removeFile)
      for (auto knownBytes : saved.known_bytes())
         QCOMPARE(knownBytes, 0);
}

/**
  * @class Tests
  *
  */

Tests::Tests()
{
}

void Tests::initTestCase()
{
   qDebug() << Common::Global::getDataFolder(Common::Global::DataFolderType::LOCAL, false);

   LM::Builder::initMsgHandler();
   qDebug() << "===== initTestCase() =====";

   this->fileManager = QSharedPointer<MockFileManager>(new MockFileManager());
   this->peerManager = QSharedPointer<MockPeerManager>(new MockPeerManager());
   this->downloadManager = Builder::newDownloadManager(this->fileManager, this->peerManager);
}

void Tests::cleanupTestCase()
{
   qDebug() << "===== cleanupTestCase() =====";
}
