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
#include <QSignalSpy>

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
      bool closeRequested = false;
      void start() override {} // Keep the transfer pending without opening a socket.
      void doDeleteLater() override { this->deleteLater(); }
      void setStatus(bool close) override { this->closeRequested = close; }
   };

   class PendingHashesResult : public PM::IGetHashesResult
   {
   public:
      PendingHashesResult() : IGetHashesResult(60000) {}
      void start() override {}
      void doDeleteLater() override { this->deleteLater(); }
   };

   class PendingEntriesResult : public PM::IGetEntriesResult
   {
   public:
      PendingEntriesResult() : IGetEntriesResult(60000) {}
      void start() override {}
      void doDeleteLater() override { this->deleteLater(); }
   };

   class DirectoryPeer : public ResumePeer
   {
   public:
      using ResumePeer::ResumePeer;
      int requests = 0;
      QSharedPointer<PM::IGetEntriesResult> entries;
      QSharedPointer<PM::IGetEntriesResult> getEntries(const Protos::Core::GetEntries&) override
      {
         ++this->requests;
         this->entries.reset(new PendingEntriesResult);
         return this->entries;
      }
   };

   class DirectoryFileManager : public MockFileManager
   {
   public:
      DirectoryFileManager(FM::Cache& cache) : cache(cache) {}
      int failure = 0;
      int creations = 0;
      void newDirectory(Protos::Common::Entry& entry) override
      {
         ++this->creations;
         switch (this->failure)
         {
         case 1: throw FM::NoWriteableDirectoryException();
         case 2: throw FM::UnableToCreateNewDirException();
         case 3: throw FM::ScanningException();
         }
         this->cache.newDirectory(entry);
      }
   private:
      FM::Cache& cache;
   };

   class HashPeer : public ResumePeer
   {
   public:
      using ResumePeer::ResumePeer;
      Protos::Common::Entry requestedEntry;
      QSharedPointer<PM::IGetHashesResult> hashes = QSharedPointer<PendingHashesResult>::create();
      QSharedPointer<PM::IGetHashesResult> getHashes(const Protos::Common::Entry& entry) override
      {
         this->requestedEntry = entry;
         return this->hashes;
      }
   };

   class CheckpointPeer : public ResumePeer
   {
   public:
      using ResumePeer::ResumePeer;
      bool available = true;
      QSharedPointer<PM::IGetChunksResult> lastChunksResult;
      bool isAvailable() const override { return this->available; }
      QSharedPointer<PM::IGetChunksResult> getChunks(const Protos::Core::GetChunks&) override
      {
         this->lastChunksResult.reset(new PendingChunksResult);
         return this->lastChunksResult;
      }
   };

   class FailingChunk : public FM::IChunk
   {
   public:
      FailingChunk(int num, Common::Hash hash) : num(num), hash(hash) {}
      QSharedPointer<FM::IDataReader> getDataReader() override { return {}; }
      QSharedPointer<FM::IDataWriter> getDataWriter() override { throw FM::IOErrorException(); }
      void removeItsIncompleteFile() override {}
      bool populateEntry(Protos::Common::Entry*) const override { return false; }
      Common::Path getFilePath() const override { return {}; }
      int getNum() const override { return this->num; }
      int getNbTotalChunk() const override { return 2; }
      Common::Hash getHash() const override { return this->hash; }
      void setHash(const Common::Hash& hash) override { this->hash = hash; }
      int getKnownBytes() const override { return 0; }
      void setKnownBytes(int) override {}
      int getChunkSize() const override { return Common::Constants::CHUNK_SIZE; }
      bool isComplete() const override { return false; }
      QString toStringLog() const override { return "failing test chunk"; }
   private:
      int num;
      Common::Hash hash;
   };

   class UnusedSocket : public PM::ISocket
   {
   public:
      // The writer fails before any network IO; only ownership/buffer cleanup is exercised.
      int bufferChanges = 0;
      void setReadBufferSize(qint64) override { ++this->bufferChanges; }
      qint64 bytesAvailable() const override { return 0; }
      qint64 read(char*, qint64) override { return -1; }
      QByteArray readAll() override { return {}; }
      bool waitForReadyRead(int) override { return false; }
      qint64 bytesToWrite() const override { return 0; }
      qint64 write(const char*, qint64) override { return -1; }
      qint64 write(const QByteArray&) override { return -1; }
      bool waitForBytesWritten(int) override { return false; }
      void moveToThread(QThread*) override {}
      QString errorString() const override { return "unused test socket"; }
      Common::Hash getRemotePeerID() const override { return {}; }
      void finished(bool) override {}
   };
}

void Tests::directoryBecomesEmpty_data()
{
   QTest::addColumn<bool>("explicitEntries");
   QTest::addColumn<int>("failure");
   for (bool explicitEntries : { false, true })
      for (int failure : { 0, 1, 2, 3 })
      {
         const auto name = QString("%1-failure-%2").arg(explicitEntries ? "empty-entries" : "omitted-entries").arg(failure).toUtf8();
         QTest::newRow(name.constData()) << explicitEntries << failure;
      }
}

void Tests::directoryBecomesEmpty()
{
   QFETCH(bool, explicitEntries);
   QFETCH(int, failure);
   QTemporaryDir temp;
   QVERIFY(temp.isValid());
   FM::Cache cache(QSharedPointer<HC::IHashCache>(new EmptyHashCache));
   const auto shared = cache.addASharedPath(temp.path() + '/');
   QSharedPointer<DirectoryFileManager> files(new DirectoryFileManager(cache));
   files->failure = failure;
   DirectoryPeer peer(files);
   DownloadManager manager(files, this->peerManager);
   Protos::Common::Entry entry;
   entry.set_type(Protos::Common::Entry::DIR);
   entry.set_name("became-empty");
   entry.set_path("/");
   entry.set_is_empty(false); // The browse result was obtained before its children disappeared.
   entry.mutable_shared_entry()->mutable_id()->set_hash(shared.first.ID.getData(), Common::Hash::HASH_SIZE);
   auto download = manager.addDownload(entry, entry, &peer, Protos::Queue::Queue::Entry::QUEUED);
   QVERIFY(download);
   QCOMPARE(files->creations, 0);
   QCOMPARE(peer.requests, 1);
   Protos::Core::GetEntriesResult response;
   auto result = response.add_results();
   result->set_status(Protos::Core::GetEntriesResult::EntryResult::OK);
   if (explicitEntries)
      result->mutable_entries();
   const QString destination = temp.path() + "/became-empty";
   emit peer.entries->result(response);

   QCOMPARE(files->creations, 1);
   if (failure)
   {
      QCOMPARE(manager.getDownloads().size(), 1);
      QCOMPARE(manager.getDownloads().first()->getID(), download->getID());
      const auto expected = failure == 1 ? Protos::Common::DownloadStatus::NO_SHARED_DIRECTORY_TO_WRITE
         : failure == 2 ? Protos::Common::DownloadStatus::UNABLE_TO_CREATE_THE_DIRECTORY
         : Protos::Common::DownloadStatus::LOCAL_SCANNING_IN_PROGRESS;
      QCOMPARE(download->getStatus(), expected);
      QVERIFY(!QFileInfo::exists(destination));
      // A failed retry must keep the entry; a later retry must recreate the directory.
      QVERIFY(QMetaObject::invokeMethod(&manager, "restartErroneousDownloads", Qt::DirectConnection));
      QCOMPARE(files->creations, 2);
      QCOMPARE(manager.getDownloads().size(), 1);
      QCOMPARE(peer.requests, 1);
      files->failure = 0;
      QVERIFY(QMetaObject::invokeMethod(&manager, "restartErroneousDownloads", Qt::DirectConnection));
      QCOMPARE(peer.requests, 2); // The first failed creation released the peer.
      emit peer.entries->result(response);
   }
   QVERIFY(manager.getDownloads().isEmpty());
   QVERIFY(QFileInfo(destination).isDir());
}

void Tests::validateChunkResponse_data()
{
   QTest::addColumn<int>("localSize");
   QTest::addColumn<int>("offset");
   QTest::addColumn<quint32>("reportedSize");
   QTest::addColumn<int>("resultCount");
   QTest::addColumn<bool>("accepted");
   const int fullSize = Common::Constants::CHUNK_SIZE;
   QTest::newRow("full-chunk") << fullSize << 0 << quint32(fullSize) << 1 << true;
   QTest::newRow("short-final-chunk") << 100 << 0 << quint32(100) << 1 << true;
   QTest::newRow("resumed-chunk") << 100 << 25 << quint32(100) << 1 << true;
   QTest::newRow("missing-result") << 100 << 0 << quint32(100) << 0 << false;
   QTest::newRow("extra-result") << 100 << 0 << quint32(100) << 2 << false;
   QTest::newRow("zero-size") << 100 << 0 << quint32(0) << 1 << false;
   QTest::newRow("smaller-than-local") << 100 << 0 << quint32(99) << 1 << false;
   QTest::newRow("larger-than-local") << 100 << 0 << quint32(101) << 1 << false;
   QTest::newRow("size-below-offset") << 100 << 25 << quint32(20) << 1 << false;
   QTest::newRow("above-protocol-limit") << fullSize << 0 << quint32(fullSize + 1) << 1 << false;
   QTest::newRow("unsigned-overflow") << 100 << 0 << quint32(0xffffffffu) << 1 << false;
   QTest::newRow("offset-at-end") << 100 << 100 << quint32(100) << 1 << false;
   QTest::newRow("offset-beyond-end") << 100 << 101 << quint32(100) << 1 << false;
   QTest::newRow("negative-offset") << 100 << -1 << quint32(100) << 1 << false;
}

void Tests::validateChunkResponse()
{
   QFETCH(int, localSize);
   QFETCH(int, offset);
   QFETCH(quint32, reportedSize);
   QFETCH(int, resultCount);
   QFETCH(bool, accepted);
   class SizedChunk : public FailingChunk
   {
   public:
      SizedChunk(Common::Hash hash, int size, int offset) : FailingChunk(0, hash), size(size), offset(offset) {}
      int getChunkSize() const override { return this->size; }
      int getKnownBytes() const override { return this->offset; }
   private:
      int size;
      int offset;
   };
   CheckpointPeer peer(this->fileManager);
   LinkedPeers links;
   OccupiedPeers downloading;
   Common::ThreadPool pool(1);
   Common::TransferRateCalculator rate;
   const auto hash = Common::Hash::rand();
   auto downloader = (new ChunkDownloader(links, downloading, rate, pool, hash))->grabStrongRef();
   downloader->setChunk(QSharedPointer<FM::IChunk>(new SizedChunk(hash, localSize, offset)));
   downloader->setPeerSource(&peer);
   QSignalSpy finished(downloader.data(), &ChunkDownloader::downloadFinished);
   QCOMPARE(downloader->startDownloading(), &peer);
   auto request = qSharedPointerDynamicCast<PendingChunksResult>(peer.lastChunksResult);
   QVERIFY(request);
   Protos::Core::GetChunksResult response;
   response.set_status(Protos::Core::GetChunksResult::OK);
   for (int i = 0; i < resultCount; ++i)
      response.add_results()->set_chunk_size(reportedSize);
   emit request->result(response);

   QCOMPARE(downloader->isDownloading(), accepted);
   QCOMPARE(downloading.isPeerFree(&peer), !accepted);
   QCOMPARE(finished.count(), accepted ? 0 : 1);
   QCOMPARE(request->closeRequested, !accepted);
   QCOMPARE(downloader->getDownloadedBytes(), offset);
   if (!accepted)
   {
      QCOMPARE(downloader->getLastTransferStatus(), Protos::Common::DownloadStatus::TRANSFER_ERROR);
      // Rejected responses must disconnect their stream and timeout callbacks.
      auto socket = QSharedPointer<UnusedSocket>::create();
      emit request->stream(socket);
      emit request->timeout();
      QCOMPARE(socket->bufferChanges, 0);
      QCOMPARE(finished.count(), 1);
   }
   downloader->stop();
}

void Tests::rejectInvalidChunkHashes_data()
{
   QTest::addColumn<bool>("inEntry");
   QTest::addColumn<QByteArray>("invalidHash");
   for (bool inEntry : { true, false })
      for (int length : { 0, 1, Common::Hash::HASH_SIZE - 1, Common::Hash::HASH_SIZE, Common::Hash::HASH_SIZE + 1 })
      {
         const auto name = QString("%1-length-%2").arg(inEntry ? "entry" : "response").arg(length).toUtf8();
         QTest::newRow(name.constData()) << inEntry
            << QByteArray(length, length == Common::Hash::HASH_SIZE ? '\0' : 'x');
      }
}

void Tests::rejectInvalidChunkHashes()
{
   QFETCH(bool, inEntry);
   QFETCH(QByteArray, invalidHash);
   HashPeer peer(this->fileManager);
   LinkedPeers links;
   OccupiedPeers asking, downloading;
   Common::ThreadPool pool(1);
   Common::TransferRateCalculator rate;
   const auto knownHash = Common::Hash::rand();
   const auto replacementHash = Common::Hash::rand();
   Protos::Common::Entry entry;
   entry.set_type(Protos::Common::Entry::FILE);
   entry.set_name("hashes.bin");
   entry.set_size(quint64(2) * Common::Constants::CHUNK_SIZE);
   entry.add_chunks()->set_hash(inEntry ? invalidHash.toStdString() : std::string());
   entry.add_chunks()->set_hash(knownHash.getData(), Common::Hash::HASH_SIZE);
   FileDownload download(this->fileManager, links, asking, downloading, pool, &peer, entry, entry,
      rate, Protos::Queue::Queue::Entry::QUEUED);
   QVERIFY(download.retrieveHashes());
   QVERIFY(!asking.isPeerFree(&peer));
   QSignalSpy newHashes(&download, &FileDownload::newHashKnown);
   if (!inEntry)
   {
      Protos::Core::HashResult invalid;
      invalid.set_num(0);
      invalid.mutable_hash()->set_hash(invalidHash.toStdString());
      emit peer.hashes->nextHash(invalid);
   }
   QCOMPARE(newHashes.count(), 0);
   QCOMPARE(download.getStatus(), Protos::Common::DownloadStatus::GETTING_THE_HASHES);
   QVERIFY(!asking.isPeerFree(&peer));
   QList<QSharedPointer<IChunkDownloader>> chunks;
   download.getUnfinishedChunks(chunks, 2, false);
   QCOMPARE(chunks.size(), 1);
   QCOMPARE(chunks.first()->getHash(), knownHash);
   Protos::Queue::Queue::Entry saved;
   download.populateQueueEntry(&saved);
   QVERIFY(saved.remote_entry().chunks(0).hash().empty());
   QVERIFY(saved.local_entry().chunks(0).hash().empty());
   QVERIFY(peer.requestedEntry.chunks(0).hash().empty());

   // The rejected hash must neither occupy its slot nor count towards completing the request.
   Protos::Core::HashResult valid;
   valid.set_num(0);
   valid.mutable_hash()->set_hash(replacementHash.getData(), Common::Hash::HASH_SIZE);
   emit peer.hashes->nextHash(valid);
   QCOMPARE(newHashes.count(), 1);
   QVERIFY(asking.isPeerFree(&peer));
   chunks.clear();
   download.getUnfinishedChunks(chunks, 2, false);
   QCOMPARE(chunks.size(), 2);
   QCOMPARE(chunks[0]->getHash(), replacementHash);
   QCOMPARE(chunks[1]->getHash(), knownHash);
   download.populateQueueEntry(&saved);
   QCOMPARE(saved.remote_entry().chunks(0).hash(), valid.hash().hash());
   QCOMPARE(saved.local_entry().chunks(0).hash(), valid.hash().hash());
}

void Tests::chunkErrorTakesPrecedence_data()
{
   QTest::addColumn<int>("errorIndex");
   QTest::addColumn<bool>("otherHasPeer");
   QTest::addColumn<bool>("otherDownloading");
   QTest::newRow("error-first-ready") << 0 << true << false;
   QTest::newRow("error-last-ready") << 1 << true << false;
   QTest::newRow("error-first-no-source") << 0 << false << false;
   QTest::newRow("error-last-no-source") << 1 << false << false;
   QTest::newRow("defer-until-last-transfer-ends") << 0 << true << true;
}

void Tests::chunkErrorTakesPrecedence()
{
   QFETCH(int, errorIndex);
   QFETCH(bool, otherHasPeer);
   QFETCH(bool, otherDownloading);
   QSharedPointer<ResumeFileManager> files(new ResumeFileManager);
   Protos::Common::Entry entry;
   entry.set_type(Protos::Common::Entry::FILE);
   entry.set_name("errors.bin");
   entry.set_size(quint64(2) * Common::Constants::CHUNK_SIZE);
   for (int i = 0; i < 2; ++i)
   {
      const auto hash = Common::Hash::rand();
      entry.add_chunks()->set_hash(hash.getData(), Common::Hash::HASH_SIZE);
      files->chunks << QSharedPointer<FM::IChunk>(new FailingChunk(i, hash));
   }
   CheckpointPeer peer(files), otherPeer(files);
   LinkedPeers links;
   OccupiedPeers asking, downloading;
   Common::ThreadPool pool(1);
   Common::TransferRateCalculator rate;
   FileDownload download(files, links, asking, downloading, pool, &peer, entry, entry,
      rate, Protos::Queue::Queue::Entry::QUEUED);
   download.start();
   QList<QSharedPointer<IChunkDownloader>> chunks;
   download.getUnfinishedChunks(chunks, 2);
   QCOMPARE(chunks.size(), 2);
   auto failed = qSharedPointerDynamicCast<ChunkDownloader>(chunks[errorIndex]);
   auto other = qSharedPointerDynamicCast<ChunkDownloader>(chunks[1 - errorIndex]);
   QVERIFY(failed);
   QVERIFY(other);
   if (!otherHasPeer || otherDownloading)
      other->rmPeer(&peer);
   if (otherDownloading)
   {
      other->addPeer(&otherPeer);
      QCOMPARE(other->startDownloading(), &otherPeer);
   }
   QSignalSpy errors(&download, &Download::becomeErroneous);
   QCOMPARE(failed->startDownloading(), &peer);
   // Run the actual worker and completion callback, failing when it opens the data writer.
   emit peer.lastChunksResult->stream(QSharedPointer<PM::ISocket>(new UnusedSocket));
   QTRY_VERIFY(!failed->isDownloading());
   if (otherDownloading)
   {
      QCOMPARE(download.getStatus(), Protos::Common::DownloadStatus::DOWNLOADING);
      QCOMPARE(errors.count(), 0);
      QCOMPARE(failed->getLastTransferStatus(), Protos::Common::DownloadStatus::FILE_IO_ERROR);
      other->stop();
   }
   QCOMPARE(download.getStatus(), Protos::Common::DownloadStatus::FILE_IO_ERROR);
   QCOMPARE(errors.count(), 1);
   QCOMPARE(failed->getLastTransferStatus(), Protos::Common::DownloadStatus::QUEUED);
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
