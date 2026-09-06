#include <CacheTest.h>

#include <QTemporaryDir>
#include <QTest>
#include <QSemaphore>
#include <QThread>
#include <QSignalSpy>
#include <QScopeGuard>

#include <atomic>
#include <exception>
#include <functional>
#include <thread>
#include <memory>

#include <Common/Constants.h>
#include <Common/Settings.h>
#include <MockHashCache.h>
#include <IDataReader.h>
#include <IDataWriter.h>
#include <priv/Cache/Cache.h>
#include <priv/Cache/Chunk.h>
#include <priv/Cache/Directory.h>
#include <priv/Cache/SharedEntry.h>
#include <priv/GetEntriesResult.h>
#include <priv/FileManager.h>

/**
  * @class CacheTest
  *
  * To test the class 'FM::Cache'.
  */

CacheTest::CacheTest(QObject *parent) :
   QObject(parent)
{
}

void CacheTest::unfinishedFilesStayOutOfSearch_data()
{
   QTest::addColumn<int>("replacementSize");
   QTest::newRow("smaller") << 4;
   QTest::newRow("same-size") << 8;
   QTest::newRow("larger") << 12;
}

void CacheTest::unfinishedFilesStayOutOfSearch()
{
   QFETCH(int, replacementSize);
   QTemporaryDir temp;
   QVERIFY(temp.isValid());
   const auto savedShares = SETTINGS.getRepeated<Protos::Common::SharedEntry>("shared_entries");
   const auto restoreShares = qScopeGuard([&] { SETTINGS.set("shared_entries", savedShares); });
   SETTINGS.rm("shared_entries");
   FM::FileManager manager(QSharedPointer<HC::IHashCache>(new MockHashCache));
   const auto shared = manager.addASharedPath(temp.path() + '/');
   QTRY_COMPARE(manager.getCacheStatus(), FM::IFileManager::UP_TO_DATE);

   const auto makeEntry = [&](const QByteArray& data) {
      Protos::Common::Entry entry;
      entry.set_type(Protos::Common::Entry::FILE);
      entry.set_path("/");
      entry.set_name("resizedownload.txt");
      entry.set_size(data.size());
      entry.mutable_shared_entry()->mutable_id()->set_hash(shared.first.ID.getData(), Common::Hash::HASH_SIZE);
      Common::Hasher hasher;
      hasher.addData(std::span<const char>(data));
      const auto hash = hasher.getResult();
      entry.add_chunks()->set_hash(hash.getData(), Common::Hash::HASH_SIZE);
      return entry;
   };
   const auto search = [&](qint64 size, const QString& words = QString(), const QList<QString>& extensions = {}) {
      QStringList names;
      for (const auto& result : manager.find(words, extensions, size, size,
         Protos::Common::FindPattern::FILE, 100, 65536, true))
         for (const auto& entry : result.entries())
            names << QString::fromStdString(entry.entry().name());
      return names;
   };

   const QByteArray original(8, 'a');
   auto originalEntry = makeEntry(original);
   auto chunks = manager.newFile(originalEntry);
   QCOMPARE(chunks.size(), 1);
   QVERIFY(search(original.size()).isEmpty());
   {
      auto writer = chunks.first()->getDataWriter();
      QVERIFY(writer->write(original.constData(), original.size()));
   }
   QCOMPARE(search(original.size()), QStringList { "resizedownload.txt" });

   const QByteArray replacement(replacementSize, 'b');
   auto replacementEntry = makeEntry(replacement);
   chunks = manager.newFile(replacementEntry);
   QCOMPARE(chunks.size(), 1);
   QVERIFY(search(original.size()).isEmpty());
   QVERIFY(search(replacementSize).isEmpty());
   QVERIFY(search(replacementSize, "resizedownload").isEmpty());
   QVERIFY(search(replacementSize, "", { "txt" }).isEmpty());
   {
      auto writer = chunks.first()->getDataWriter();
      QVERIFY(!writer->write(replacement.constData(), 1));
      QVERIFY(search(replacementSize).isEmpty());
      QVERIFY(writer->write(replacement.constData() + 1, replacement.size() - 1));
   }
   QCOMPARE(search(replacementSize), QStringList { "resizedownload.txt" });
   QCOMPARE(search(replacementSize, "resizedownload"), QStringList { "resizedownload.txt" });
   QCOMPARE(search(replacementSize, "", { "txt" }), QStringList { "resizedownload.txt" });
   if (replacementSize != original.size())
      QVERIFY(search(original.size()).isEmpty());
}

void CacheTest::retainedChunksAreDetached_data()
{
   QTest::addColumn<bool>("redownload");
   QTest::newRow("changed-on-disk") << false;
   QTest::newRow("redownload") << true;
}

void CacheTest::retainedChunksAreDetached()
{
   QFETCH(bool, redownload);
   FM::Chunk::CHUNK_SIZE = Common::Constants::CHUNK_SIZE;
   QTemporaryDir temp;
   QVERIFY(temp.isValid());
   const QString path = temp.filePath("file.bin");
   const QByteArray original("original data");
   {
      QFile physical(path);
      QVERIFY(physical.open(QIODevice::WriteOnly));
      QCOMPARE(physical.write(original), original.size());
   }

   QSharedPointer<FM::Chunk> retired;
   QSharedPointer<FM::Chunk> replacement;
   QSharedPointer<FM::IDataReader> oldReader;
   QSharedPointer<FM::IDataWriter> oldWriter;
   QByteArray buffer(SETTINGS.get<quint32>("buffer_size_reading"), Qt::Uninitialized);
   {
      FM::Cache cache(QSharedPointer<HC::IHashCache>(new MockHashCache));
      const auto shared = cache.addASharedPath(temp.path() + '/');
      auto root = dynamic_cast<FM::SharedDirectory*>(cache.getSharedEntry(shared.first.ID));
      QVERIFY(root);
      auto file = new FM::File(root, "file.bin", original.size(), false,
         QFileInfo(path).lastModified(), root->getRootDir());
      retired = file->getChunks().first();
      oldReader = retired->getDataReader();
      oldWriter = retired->getDataWriter();
      QCOMPARE(oldReader->read(buffer.data(), 0), original.size());

      if (redownload)
         file->setToUnfinished(original.size());
      else
         file->fileHasChangedOnDisk(QFileInfo(path));

      QVERIFY(!retired->isOwnedBy(file));
      QVERIFY(retired->getFilePath().isNull());
      QVERIFY(!retired->isComplete());
      QCOMPARE(retired->getNbTotalChunk(), 0);
      QVERIFY(retired->getOtherChunks().isEmpty());
      Protos::Common::Entry entry;
      QVERIFY(!retired->populateEntry(&entry));
      QVERIFY_THROWS_EXCEPTION(FM::ChunkDeletedException, oldReader->read(buffer.data(), 0));
      QVERIFY_THROWS_EXCEPTION(FM::ChunkDeletedException, retired->write("x", 1));

      replacement = file->getChunks().first();
      QVERIFY(replacement != retired);
      QVERIFY(replacement->isOwnedBy(file));
      auto reader = replacement->getDataReader();
      auto writer = replacement->getDataWriter();
      // Destroy old adapters while new adapters are alive: they must not close the replacement handles.
      oldReader.clear();
      oldWriter.clear();
      if (redownload)
      {
         QVERIFY(!writer->write("x", 1));
         QCOMPARE(reader->read(buffer.data(), 0), 1);
         QCOMPARE(buffer[0], 'x');
         QFile physical(path);
         QVERIFY(physical.open(QIODevice::ReadOnly));
         QCOMPARE(physical.readAll(), original);
      }
      else
      {
         QCOMPARE(reader->read(buffer.data(), 0), original.size());
         QCOMPARE(buffer.first(original.size()), original);
      }
   }

   // Both generations survive destruction of the cache and its File.
   QVERIFY(retired->getFilePath().isNull());
   QVERIFY(replacement->getFilePath().isNull());
   QVERIFY_THROWS_EXCEPTION(FM::ChunkDeletedException, retired->read(buffer.data(), 0));
   QVERIFY_THROWS_EXCEPTION(FM::ChunkDeletedException, replacement->read(buffer.data(), 0));
   retired->removeItsIncompleteFile();
}

namespace
{
   class ShortWritingFile : public FM::File
   {
   public:
      using FM::File::File;
      int writesBeforeFailure = -1;
      qint64 failureResult = -1;
      int writeCalls = 0;

   protected:
      qint64 writePhysicalFile(const char* buffer, qint64 nbBytes) override
      {
         ++this->writeCalls;
         if (this->writesBeforeFailure == 0)
         {
            this->writesBeforeFailure = -1; // A transient failure; a later retry can succeed.
            return this->failureResult;
         }
         if (this->writesBeforeFailure > 0)
            --this->writesBeforeFailure;
         return FM::File::writePhysicalFile(buffer, qMin(nbBytes, qint64(2)));
      }
   };

   // Pause handle creation before it resolves the path, while it already holds its I/O lock.
   class PausedOpeningFile : public FM::File
   {
   public:
      using FM::File::File;

      Common::Path getAbsolutePath() const override
      {
         if (this->pauseOpening.exchange(false))
         {
            this->opening.release();
            this->resume.acquire();
         }
         return FM::File::getAbsolutePath();
      }

      bool canEnterCompletion()
      {
         if (!this->mutex.tryLock())
            return false;
         this->mutex.unlock();
         return true;
      }

      mutable std::atomic<bool> pauseOpening { false };
      mutable QSemaphore opening;
      mutable QSemaphore resume;
   };

   class PausedChunkFile : public PausedOpeningFile
   {
   public:
      using PausedOpeningFile::PausedOpeningFile;
      bool abortWrite = false;

   protected:
      qint64 writePhysicalFile(const char* buffer, qint64 nbBytes) override
      {
         this->opening.release();
         this->resume.acquire();
         if (this->abortWrite)
            throw FM::IOErrorException();
         return FM::File::writePhysicalFile(buffer, nbBytes);
      }
   };

   class InspectableDirectory : public FM::Directory
   {
   public:
      using FM::Directory::Directory;
      bool canLock()
      {
         if (!this->mutex.tryLock())
            return false;
         this->mutex.unlock();
         return true;
      }
   };

   class RetiringFile : public PausedChunkFile
   {
   public:
      using PausedChunkFile::PausedChunkFile;
      QSemaphore retiring;
      QSemaphore continueRetirement;

      void del(bool invokeDelete = true) override
      {
         this->retiring.release();
         this->continueRetirement.acquire();
         FM::File::del(invokeDelete);
      }

      void removeUnfinishedFiles() override
      {
         this->retiring.release();
         this->continueRetirement.acquire();
         FM::File::removeUnfinishedFiles();
      }
   };
}

void CacheTest::directoryCleanupAllowsCompletion_data()
{
   QTest::addColumn<bool>("remove");
   QTest::addColumn<bool>("nested");
   QTest::newRow("delete-direct") << true << false;
   QTest::newRow("delete-nested") << true << true;
   QTest::newRow("unfinished-direct") << false << false;
   QTest::newRow("unfinished-nested") << false << true;
}

void CacheTest::directoryCleanupAllowsCompletion()
{
   QFETCH(bool, remove);
   QFETCH(bool, nested);
   FM::Chunk::CHUNK_SIZE = Common::Constants::CHUNK_SIZE;
   QTemporaryDir temp;
   QVERIFY(temp.isValid());
   FM::Cache cache(QSharedPointer<HC::IHashCache>(new MockHashCache));
   const auto shared = cache.addASharedPath(temp.path() + '/');
   auto root = dynamic_cast<FM::SharedDirectory*>(cache.getSharedEntry(shared.first.ID));
   QVERIFY(root);
   auto parent = new InspectableDirectory(root, "parent", root->getRootDir(), true);
   auto leaf = nested ? new InspectableDirectory(root, "child", parent, true) : parent;
   auto file = new RetiringFile(root, "download.bin", 1, false, QDateTime::currentDateTime(),
      leaf, QList<Common::Hash>(), true);
   const QString completedPath = leaf->getAbsolutePath().toString() + "download.bin";
   auto chunk = file->getChunks().first();
   auto writer = chunk->getDataWriter();
   std::exception_ptr writeError;
   std::thread downloading([&] {
      try { chunk->write("x", 1); }
      catch (...) { writeError = std::current_exception(); }
   });
   const bool reachedWrite = file->opening.tryAcquire(1, 5000);
   std::thread cleanup([&] {
      if (remove)
         parent->del(false);
      else
         parent->removeUnfinishedFiles();
   });
   const bool reachedRetirement = file->retiring.tryAcquire(1, 5000);
   const bool parentAvailable = parent->canLock();
   const bool leafAvailable = leaf->canLock();
   // On a regression, abort the write instead of completing under an inverted parent lock,
   // so both workers can finish and the test reports a failure rather than deadlocking.
   file->abortWrite = !parentAvailable || !leafAvailable;
   file->continueRetirement.release();
   file->resume.release();
   downloading.join();
   cleanup.join();
   if (remove)
   {
      // Children are queued before their parent; keep the parent alive until they are destroyed.
      QCoreApplication::sendPostedEvents(&cache, QEvent::MetaCall);
      cache.deleteEntry(parent);
   }

   QVERIFY(reachedWrite);
   QVERIFY(reachedRetirement);
   QVERIFY(parentAvailable);
   QVERIFY(leafAvailable);
   QVERIFY(!writeError);
   QVERIFY(QFileInfo::exists(completedPath));
   if (remove)
      QVERIFY(chunk->getFilePath().isNull());
   else
   {
      QVERIFY(chunk->isFileComplete());
      QCOMPARE(leaf->getFiles().size(), 1);
      QCOMPARE(leaf->getSize(), qint64(1));
   }
}

void CacheTest::directoryDestructionReleasesParentLocks()
{
   class ProbeFile : public FM::File
   {
   public:
      using FM::File::File;
      std::function<void()> probe;
      ~ProbeFile() override { this->probe(); }
   };
   QTemporaryDir temp;
   QVERIFY(temp.isValid());
   bool parentAvailable = false;
   bool leafAvailable = false;
   bool reinserted = false;
   {
      FM::Cache cache(QSharedPointer<HC::IHashCache>(new MockHashCache));
      const auto shared = cache.addASharedPath(temp.path() + '/');
      auto root = dynamic_cast<FM::SharedDirectory*>(cache.getSharedEntry(shared.first.ID));
      QVERIFY(root);
      auto parent = new InspectableDirectory(root, "parent", root->getRootDir());
      auto leaf = new InspectableDirectory(root, "child", parent);
      auto file = new ProbeFile(root, "file.bin", 0, false, QDateTime::currentDateTime(), leaf);
      file->probe = [&, parent, leaf, file] {
         std::thread callback([&] {
            parentAvailable = parent->canLock();
            leafAvailable = leaf->canLock();
            // A completion/rename callback must not reinsert a child detached by the destructor.
            if (leafAvailable)
            {
               leaf->fileNameChanged(file);
               reinserted = !leaf->getFiles().isEmpty();
            }
         });
         callback.join();
      };
      // Cache destruction deletes the attached subtree synchronously, without a prior del().
   }
   QVERIFY(parentAvailable);
   QVERIFY(leafAvailable);
   QVERIFY(!reinserted);
}

void CacheTest::directoryTraversalDefersDeletion()
{
   class ProbeFile : public FM::File
   {
   public:
      using FM::File::File;
      std::shared_ptr<bool> destroyed;
      ~ProbeFile() override { *this->destroyed = true; }
   };
   FM::Chunk::CHUNK_SIZE = Common::Constants::CHUNK_SIZE;
   QTemporaryDir temp;
   QVERIFY(temp.isValid());
   FM::Cache cache(QSharedPointer<HC::IHashCache>(new MockHashCache));
   const auto shared = cache.addASharedPath(temp.path() + '/');
   auto root = dynamic_cast<FM::SharedDirectory*>(cache.getSharedEntry(shared.first.ID));
   QVERIFY(root);
   auto file = new RetiringFile(root, "a.bin", 1, false, QDateTime::currentDateTime(),
      root->getRootDir(), QList<Common::Hash>(), true);
   auto later = new ProbeFile(root, "z.bin", 0, false, QDateTime::currentDateTime(), root->getRootDir());
   const auto destroyed = std::make_shared<bool>(false);
   later->destroyed = destroyed;
   std::thread cleanup([&] { root->getRootDir()->removeUnfinishedFiles(); });
   const bool reachedRetirement = file->retiring.tryAcquire(1, 5000);
   // This file is already in the traversal's snapshot, but has not been visited yet.
   later->del();
   QCoreApplication::sendPostedEvents(&cache, QEvent::MetaCall);
   const bool destroyedDuringTraversal = *destroyed;
   file->continueRetirement.release();
   cleanup.join();
   QCoreApplication::sendPostedEvents(&cache, QEvent::MetaCall);
   QVERIFY(reachedRetirement);
   QVERIFY(!destroyedDuringTraversal);
   QVERIFY(*destroyed);
}

void CacheTest::chunkAccessExcludesRetirement_data()
{
   QTest::addColumn<bool>("writing");
   QTest::addColumn<bool>("replacing");
   QTest::newRow("path-delete") << false << false;
   QTest::newRow("write-delete") << true << false;
   QTest::newRow("path-replace") << false << true;
   QTest::newRow("write-replace") << true << true;
}

void CacheTest::chunkAccessExcludesRetirement()
{
   QFETCH(bool, writing);
   QFETCH(bool, replacing);
   FM::Chunk::CHUNK_SIZE = Common::Constants::CHUNK_SIZE;
   QTemporaryDir temp;
   QVERIFY(temp.isValid());
   QSharedPointer<FM::Chunk> chunk;
   QSharedPointer<FM::IDataWriter> writer;
   {
      FM::Cache cache(QSharedPointer<HC::IHashCache>(new MockHashCache));
      const auto shared = cache.addASharedPath(temp.path() + '/');
      auto root = dynamic_cast<FM::SharedDirectory*>(cache.getSharedEntry(shared.first.ID));
      QVERIFY(root);
      auto file = new PausedChunkFile(root, "download.bin", 8, false, QDateTime::currentDateTime(),
         root->getRootDir(), QList<Common::Hash>(), true);
      chunk = file->getChunks().first();
      writer = chunk->getDataWriter();
      const auto path = file->getAbsolutePath();
      const QFileInfo info(path);
      file->pauseOpening = !writing;
      std::exception_ptr accessError;
      std::exception_ptr retireError;
      std::thread access([&] {
         try
         {
            if (writing)
               chunk->write("x", 1); // Leave the file incomplete; no rename or hash notification.
            else
               chunk->getFilePath();
         }
         catch (...) { accessError = std::current_exception(); }
      });
      const bool reachedAccess = file->opening.tryAcquire(1, 5000);
      const bool retirementCouldEnter = reachedAccess && file->canEnterCompletion();
      QSemaphore retiring;
      QSemaphore retired;
      std::thread retire([&] {
         retiring.release();
         try
         {
            if (replacing)
               file->fileHasChangedOnDisk(info);
            else
               file->del(false); // Actual QObject-tree destruction stays on the cache thread.
         }
         catch (...) { retireError = std::current_exception(); }
         retired.release();
      });
      retiring.acquire();
      const bool retiredDuringAccess = retired.tryAcquire(1, 100);
      file->resume.release();
      access.join();
      retire.join();
      if (!replacing)
         cache.deleteEntry(file);

      QVERIFY(reachedAccess);
      QVERIFY(!retirementCouldEnter);
      QVERIFY(!retiredDuringAccess);
      QVERIFY(!accessError);
      QVERIFY(!retireError);
      QVERIFY(chunk->getFilePath().isNull());
      QCOMPARE(chunk->getKnownBytes(), writing ? 1 : 0);
      QVERIFY_THROWS_EXCEPTION(FM::ChunkDeletedException, chunk->write("y", 1));
   }
   // A waiter/retained chunk owns its mutex independently of the destroyed File.
   QVERIFY(chunk->getFilePath().isNull());
   QVERIFY(!chunk->isComplete());
   char buffer[1];
   QVERIFY_THROWS_EXCEPTION(FM::ChunkDeletedException, chunk->read(buffer, 0));
   writer.clear();
}

void CacheTest::concurrentChunkMetadata()
{
   FM::Chunk chunk(nullptr, 0, 0);
   const Common::Hash first = Common::Hash::rand();
   const Common::Hash second = Common::Hash::rand();
   chunk.setHash(first);
   std::atomic<bool> invalidSnapshot { false };
   QSemaphore start;
   std::thread updater([&] {
      start.acquire();
      for (int i = 0; i < 10000; ++i)
      {
         chunk.setHash(i % 2 ? first : second);
         chunk.setKnownBytes(i % 2 ? 123 : 456);
      }
   });
   start.release();
   for (int i = 0; i < 10000; ++i)
   {
      const auto hash = chunk.getHash();
      const int bytes = chunk.getKnownBytes();
      if ((hash != first && hash != second) || !chunk.hasHash() || (bytes != 0 && bytes != 123 && bytes != 456))
         invalidSnapshot = true;
   }
   updater.join();
   QVERIFY(!invalidSnapshot);
}

void CacheTest::partialWrites_data()
{
   QTest::addColumn<int>("failure");
   QTest::addColumn<bool>("recreateWriter");
   QTest::addColumn<bool>("checkIntegrity");
   for (bool checkIntegrity : { false, true })
      for (bool recreateWriter : { false, true })
         for (int failure : { 1, 0, -1 })
         {
            const QByteArray name = QString("failure=%1,recreate=%2,integrity=%3")
               .arg(failure).arg(recreateWriter).arg(checkIntegrity).toLatin1();
            QTest::newRow(name.constData()) << failure << recreateWriter << checkIntegrity;
         }
}

void CacheTest::partialWrites()
{
   QFETCH(int, failure);
   QFETCH(bool, recreateWriter);
   QFETCH(bool, checkIntegrity);
   FM::Chunk::CHUNK_SIZE = Common::Constants::CHUNK_SIZE;
   QTemporaryDir temp;
   QVERIFY(temp.isValid());
   FM::Cache cache(QSharedPointer<HC::IHashCache>(new MockHashCache));
   const auto shared = cache.addASharedPath(temp.path() + '/');
   auto root = dynamic_cast<FM::SharedDirectory*>(cache.getSharedEntry(shared.first.ID));
   QVERIFY(root);
   const QByteArray content("abcdefghij");
   Common::Hasher hasher;
   hasher.addData(std::span<const char>(content));
   auto file = new ShortWritingFile(root, "download.bin", content.size(), false,
      QDateTime::currentDateTime(), root->getRootDir(), { hasher.getResult() }, true);
   auto chunk = file->getChunks().first();

   const bool originalIntegrity = SETTINGS.get<bool>("check_received_data_integrity");
   SETTINGS.set("check_received_data_integrity", checkIntegrity);
   auto writer = chunk->getDataWriter();
   SETTINGS.set("check_received_data_integrity", originalIntegrity);
   QVERIFY(!writer->write(content.constData(), 3));
   QCOMPARE(chunk->getKnownBytes(), 3);
   QCOMPARE(file->writeCalls, 2); // The first request needed two physical writes.

   int completedChunks = 0;
   connect(&cache, &FM::Cache::chunkHashKnown, &cache, [&](const auto&) { ++completedChunks; });
   if (failure <= 0)
   {
      file->writesBeforeFailure = 1; // Write two bytes, then fail or make no progress.
      file->failureResult = failure;
      QVERIFY_THROWS_EXCEPTION(FM::IOErrorException, writer->write(content.constData() + 3, 7));
      QCOMPARE(chunk->getKnownBytes(), 3);
      QVERIFY(!chunk->isComplete());
      QVERIFY(!file->isComplete());
      QCOMPARE(completedChunks, 0);
      QVERIFY(!QFileInfo::exists(temp.filePath("download.bin")));
   }

   if (recreateWriter)
   {
      writer.clear();
      SETTINGS.set("check_received_data_integrity", checkIntegrity);
      writer = chunk->getDataWriter();
      SETTINGS.set("check_received_data_integrity", originalIntegrity);
   }
   QVERIFY(writer->write(content.constData() + 3, 7));
   QCOMPARE(chunk->getKnownBytes(), content.size());
   QVERIFY(file->isComplete());
   QCOMPARE(completedChunks, 1);
   QFile downloaded(temp.filePath("download.bin"));
   QVERIFY(downloaded.open(QIODevice::ReadOnly));
   QCOMPARE(downloaded.readAll(), content);
}

void CacheTest::openingHandlesExcludesCompletion_data()
{
   QTest::addColumn<bool>("writer");
   QTest::newRow("reader") << false;
   QTest::newRow("writer") << true;
}

void CacheTest::openingHandlesExcludesCompletion()
{
   QFETCH(bool, writer);
   FM::Chunk::CHUNK_SIZE = Common::Constants::CHUNK_SIZE;
   QTemporaryDir temp;
   QVERIFY(temp.isValid());
   FM::Cache cache(QSharedPointer<HC::IHashCache>(new MockHashCache));
   const auto shared = cache.addASharedPath(temp.path() + '/');
   auto root = dynamic_cast<FM::SharedDirectory*>(cache.getSharedEntry(shared.first.ID));
   QVERIFY(root);
   auto file = new PausedOpeningFile(root, "download.bin", 1, false, QDateTime::currentDateTime(),
      root->getRootDir(), QList<Common::Hash>(), true);
   auto chunk = file->getChunks().first();

   std::exception_ptr error;
   file->pauseOpening = true;
   std::thread opener([&] {
      try
      {
         if (writer)
            file->newDataWriterCreated();
         else
            file->newDataReaderCreated();
      }
      catch (...)
      {
         error = std::current_exception();
      }
   });

   const bool reachedOpening = file->opening.tryAcquire(1, 5000);
   // Completion must not acquire the file mutex while an opener holds an I/O lock:
   // otherwise completion waits for that I/O lock and the opener waits for the file mutex.
   const bool completionCouldEnter = reachedOpening && file->canEnterCompletion();
   file->resume.release();
   opener.join(); // Release the worker before any assertion can return from this test.

   QVERIFY(reachedOpening);
   QVERIFY(!error);
   QVERIFY(!completionCouldEnter);

   chunk->setKnownBytes(1);
   file->chunkComplete(chunk.data());
   QVERIFY(file->isComplete());
   QVERIFY(QFileInfo::exists(temp.filePath("download.bin")));
   QVERIFY(!QFileInfo::exists(temp.filePath("download.bin.unfinished")));
   if (writer)
      file->dataWriterDeleted();
   else
      file->dataReaderDeleted();

   auto reader = chunk->getDataReader();
   QByteArray buffer(SETTINGS.get<quint32>("buffer_size_reading"), Qt::Uninitialized);
   QCOMPARE(reader->read(buffer.data(), 0), 1);
}

namespace
{
   class BrowseThreadDirectory : public FM::Directory
   {
   public:
      BrowseThreadDirectory(FM::SharedEntry* root, FM::Directory* parent, std::atomic<bool>& wrongThread) :
         FM::Directory(root, "child", parent), ownerThread(QThread::currentThread()), wrongThread(wrongThread) {}

      void populateEntry(Protos::Common::Entry* entry, bool setSharedDir = false) const override
      {
         if (QThread::currentThread() != this->ownerThread)
            this->wrongThread = true;
         FM::Directory::populateEntry(entry, setSharedDir);
      }

   private:
      QThread* ownerThread;
      std::atomic<bool>& wrongThread;
   };
}

void CacheTest::browseDirectoryLifetime_data()
{
   QTest::addColumn<QString>("scenario");
   for (const auto& scenario : { "scanned", "scan-finishes", "removed-before-start",
      "removed-while-waiting", "scan-then-remove", "cache-destroyed", "cache-destroyed-while-waiting", "timeout" })
      QTest::newRow(scenario) << QString(scenario);
}

void CacheTest::browseDirectoryLifetime()
{
   QFETCH(QString, scenario);
   FM::Chunk::CHUNK_SIZE = Common::Constants::CHUNK_SIZE;
   QTemporaryDir temp;
   QVERIFY(temp.isValid());
   std::atomic<bool> wrongThread { false };
   auto cache = std::make_unique<FM::Cache>(QSharedPointer<HC::IHashCache>(new MockHashCache));
   const auto shared = cache->addASharedPath(temp.path() + '/');
   auto root = dynamic_cast<FM::SharedDirectory*>(cache->getSharedEntry(shared.first.ID));
   QVERIFY(root);
   auto dir = root->getRootDir()->createSubDir("browsed");
   new BrowseThreadDirectory(root, dir, wrongThread);
   new FM::File(root, "complete.bin", 1, false, QDateTime::currentDateTime(), dir,
      { Common::Hash::rand() });
   new FM::File(root, "partial.unfinished", 1, false, QDateTime::currentDateTime(), dir);
   dir->setScanned(scenario == "scanned");
   Protos::Common::Entry directory;
   dir->populateEntry(&directory, true);

   const auto oldTimeout = SETTINGS.get<quint32>("get_entries_timeout");
   SETTINGS.set("get_entries_timeout", quint32(10));
   FM::GetEntriesResult request(*cache, directory, 0);
   SETTINGS.set("get_entries_timeout", oldTimeout);
   int deliveries = 0;
   Protos::Core::GetEntriesResult::EntryResult response;
   connect(&request, &FM::IGetEntriesResult::result, &request, [&](const auto& result) {
      ++deliveries;
      response = result;
   });
   QSignalSpy timeouts(&request, &Common::Timeoutable::timeout);

   if (scenario == "removed-before-start")
   {
      dir->del();
      QCoreApplication::sendPostedEvents(cache.get(), QEvent::MetaCall);
   }
   else if (scenario == "cache-destroyed")
      cache.reset();

   request.start();
   if (scenario == "cache-destroyed-while-waiting")
      cache.reset();
   if (scenario == "timeout")
      QTRY_COMPARE(timeouts.count(), 1);

   if (scenario == "scan-finishes" || scenario == "scan-then-remove" ||
      scenario == "removed-while-waiting" || scenario == "timeout")
   {
      QCOMPARE(deliveries, 0);
      std::thread updater([&] {
         if (scenario != "removed-while-waiting")
            dir->setScanned(true);
         if (scenario == "scan-then-remove" || scenario == "removed-while-waiting")
            dir->del();
      });
      updater.join();
      // Delete the directory and children before delivering their pending browse notifications.
      QCoreApplication::sendPostedEvents(cache.get(), QEvent::MetaCall);
   }
   QCoreApplication::sendPostedEvents(&request, QEvent::MetaCall);
   QVERIFY(!wrongThread);
   if (scenario == "timeout")
      QCOMPARE(deliveries, 0);
   else
   {
      QCOMPARE(deliveries, 1);
      const bool found = scenario == "scanned" || scenario == "scan-finishes";
      QCOMPARE(response.status(), found ? Protos::Core::GetEntriesResult::EntryResult::OK
         : Protos::Core::GetEntriesResult::EntryResult::DONT_HAVE);
      if (found)
      {
         QCOMPARE(response.entries().entries_size(), 2); // Excludes the unfinished file.
         QCOMPARE(response.entries().entries(0).name(), std::string("child"));
         const auto& file = response.entries().entries(1);
         QCOMPARE(file.name(), std::string("complete.bin"));
         QCOMPARE(file.chunks_size(), 1);
         QVERIFY(file.chunks(0).hash().empty()); // maxNbHashesPerEntry is preserved.
      }
      request.start();
      QCoreApplication::sendPostedEvents(&request, QEvent::MetaCall);
      QCOMPARE(deliveries, 1);
      QCOMPARE(timeouts.count(), 0);
   }
}
