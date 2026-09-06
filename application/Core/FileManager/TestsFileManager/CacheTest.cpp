#include <CacheTest.h>

#include <QTemporaryDir>
#include <QTest>
#include <QSemaphore>

#include <atomic>
#include <exception>
#include <thread>

#include <Common/Constants.h>
#include <Common/Settings.h>
#include <MockHashCache.h>
#include <IDataReader.h>
#include <IDataWriter.h>
#include <priv/Cache/Cache.h>
#include <priv/Cache/Chunk.h>
#include <priv/Cache/Directory.h>
#include <priv/Cache/SharedEntry.h>

/**
  * @class CacheTest
  *
  * To test the class 'FM::Cache'.
  */

CacheTest::CacheTest(QObject *parent) :
   QObject(parent)
{
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
