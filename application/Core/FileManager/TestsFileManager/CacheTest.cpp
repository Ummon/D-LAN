#include <CacheTest.h>

#include <QTemporaryDir>
#include <QTest>
#include <QSemaphore>
#include <QThread>
#include <QSignalSpy>
#include <QScopeGuard>

#include <algorithm>
#include <atomic>
#include <exception>
#include <cstddef>
#include <functional>
#include <thread>
#include <memory>

#include <Common/Constants.h>
#include <Common/Settings.h>
#include <MockHashCache.h>
#include <Exceptions.h>
#include <IDataReader.h>
#include <IDataWriter.h>
#include <priv/Cache/Cache.h>
#include <priv/Cache/Chunk.h>
#include <priv/Cache/FileHasher.h>
#include <priv/Cache/Directory.h>
#include <priv/Cache/SharedEntry.h>
#include <priv/GetEntriesResult.h>
#include <priv/GetHashesResult.h>
#include <priv/FileManager.h>
#ifdef Q_OS_WIN32
#include <priv/FileUpdater/DirWatcherWin.h>
#endif

namespace
{
   class RecordingHashCache : public MockHashCache
   {
   public:
      QList<QList<Common::Hash>> writes;
      QDateTime savedDate;
      void setHashes(const QString&, const QList<Common::Hash>& hashes, qint64, QDateTime date) override
      {
         this->writes.append(hashes);
         this->savedDate = date;
      }
   };
}

/**
  * @class CacheTest
  *
  * To test the class 'FM::Cache'.
  */

CacheTest::CacheTest(QObject *parent) :
   QObject(parent)
{
}

void CacheTest::fittestDirectoryMatchesExistingPaths()
{
   QTemporaryDir temp;
   QVERIFY(temp.isValid());
   QVERIFY(QDir(temp.path()).mkdir("shared"));
   FM::Cache cache(QSharedPointer<HC::IHashCache>(new MockHashCache));
   const QString sharedPath = temp.filePath("shared/");
   const auto shared = cache.addASharedPath(sharedPath);
   auto root = dynamic_cast<FM::SharedDirectory*>(cache.getSharedEntry(shared.first.ID));
   QVERIFY(root);
   auto nested = root->getRootDir()->createSubDir("nested");
   auto leaf = nested->createSubDir("leaf");

   QCOMPARE(cache.getFittestDirectory(sharedPath), root->getRootDir());
   QCOMPARE(cache.getFittestDirectory(Common::Path(sharedPath + "nested/")), nested);
   QCOMPARE(cache.getFittestDirectory(Common::Path(sharedPath + "nested/leaf/")), leaf);
   // Watcher notifications also supply directory paths without a trailing slash.
   QCOMPARE(cache.getFittestDirectory(Common::Path(sharedPath + "nested/leaf")), leaf);
   QCOMPARE(cache.getFittestDirectory(Common::Path(sharedPath + "nested/leaf/file.bin")), leaf);
   QCOMPARE(cache.getFittestDirectory(Common::Path(sharedPath + "nested/missing/deeper/")), nested);
   QCOMPARE(cache.getFittestDirectory(Common::Path(sharedPath + "missing/")), root->getRootDir());
   QVERIFY(!cache.getFittestDirectory(temp.filePath("outside/")));
   QVERIFY(!cache.getFittestDirectory(temp.filePath("shared-other/")));
}

void CacheTest::watchedFileRename_data()
{
   QTest::addColumn<QString>("renamed");
   QTest::newRow("new-name") << QString("renamed.txt");
   QTest::newRow("case-only") << QString("ORIGINAL.txt");
}

void CacheTest::watchedFileRename()
{
#ifdef Q_OS_WIN32
   QFETCH(QString, renamed);
   QTemporaryDir temp;
   QVERIFY(temp.isValid());
   const QString original = temp.filePath("original.txt");
   const QString destination = temp.filePath(renamed);
   const QString unrelated = temp.filePath("unrelated.txt");
   for (const auto& path : { original, unrelated })
   {
      QFile physical(path);
      QVERIFY(physical.open(QIODevice::WriteOnly));
      QCOMPARE(physical.write("x", 1), qint64(1));
   }
   FM::DirWatcherWin watcher;
   QVERIFY(watcher.addPath(temp.path() + '/', "original.txt"));
   QVERIFY(QFile::rename(unrelated, temp.filePath("other.txt")));
   // Unrelated rename pairs must not escape the individual-file filter.
   const auto unrelatedEvents = watcher.waitEvent(1000);
   for (const auto& event : unrelatedEvents)
      QCOMPARE(event.type, FM::WatcherEvent::TIMEOUT);

   QList<FM::WatcherEvent> events;
   const auto waitFor = [&](FM::WatcherEvent::Type type, const QString& path) {
      QElapsedTimer timer;
      timer.start();
      while (timer.elapsed() < 3000)
      {
         const auto batch = watcher.waitEvent(100);
         events.append(batch);
         for (const auto& event : batch)
            if (event.type == type && (type == FM::WatcherEvent::MOVE ? event.path2 : event.path1) == path)
               return true;
      }
      return false;
   };
   // Use the native operation so a case-only rename cannot be treated as a no-op.
   QVERIFY(MoveFileExW(reinterpret_cast<LPCWSTR>(original.utf16()),
      reinterpret_cast<LPCWSTR>(destination.utf16()), 0));
   QVERIFY(waitFor(FM::WatcherEvent::MOVE, destination));
   int moves = 0;
   for (const auto& event : events)
      if (event.type == FM::WatcherEvent::MOVE)
      {
         ++moves;
         QCOMPARE(event.path1, original);
         QCOMPARE(event.path2, destination);
         QVERIFY(event.isWatchedFile);
      }
   QCOMPARE(moves, 1);

   // Follow the new filename even before the updater has replaced the registration.
   {
      QFile physical(destination);
      QVERIFY(physical.open(QIODevice::Append));
      QCOMPARE(physical.write("y", 1), qint64(1));
   }
   QVERIFY(waitFor(FM::WatcherEvent::CONTENT_CHANGED, destination));

   // FileUpdater removes by the original registration and adds the new path.
   watcher.rmPath(temp.path() + '/', "original.txt");
   QVERIFY(watcher.addPath(temp.path() + '/', renamed));
   watcher.waitEvent(0);
   QCOMPARE(watcher.nbWatchedPath(), 1);
   QVERIFY(QFile::remove(destination));
   QVERIFY(waitFor(FM::WatcherEvent::DELETED, destination));
#else
   QSKIP("Windows notification regression");
#endif
}

void CacheTest::updaterWaitsForEarliestTask_data()
{
   QTest::addColumn<bool>("unwatchable");
   QTest::addColumn<bool>("retry");
   QTest::addColumn<qint64>("elapsed");
   QTest::addColumn<int>("expected");
   QTest::newRow("idle") << false << false << qint64(0) << -1;
   QTest::newRow("retry-only") << false << true << qint64(0) << 3000;
   QTest::newRow("rescan-only") << true << false << qint64(0) << 30000;
   QTest::newRow("retry-before-rescan") << true << true << qint64(0) << 3000;
   QTest::newRow("rescan-before-retry") << true << true << qint64(29000) << 1000;
   QTest::newRow("rescan-remaining") << true << false << qint64(29000) << 1000;
   QTest::newRow("rescan-due") << true << false << qint64(30000) << 0;
   QTest::newRow("rescan-overdue-with-retry") << true << true << qint64(35000) << 0;
   QTest::newRow("no-rescan-after-removal") << false << false << qint64(35000) << -1;
}

void CacheTest::updaterWaitsForEarliestTask()
{
   QFETCH(bool, unwatchable);
   QFETCH(bool, retry);
   QFETCH(qint64, elapsed);
   QFETCH(int, expected);
   const auto previousPeriod = SETTINGS.get<quint32>("scan_period_unwatchable_dirs");
   const auto restore = qScopeGuard([&] { SETTINGS.set("scan_period_unwatchable_dirs", previousPeriod); });
   SETTINGS.set("scan_period_unwatchable_dirs", quint32(30000));
   FM::FileUpdater updater(nullptr);
   // Only membership matters to the idle timeout; no filesystem or worker is needed.
   if (unwatchable)
      updater.unwatchableEntries.append(nullptr);
   if (retry)
   {
      updater.hashingQueue.enqueue(nullptr, 100);
      updater.hashingQueue.finishPass(nullptr, 100, true, 0, 3000);
   }
   QCOMPARE(updater.nextWaitTimeout(elapsed, 0), expected);
}

void CacheTest::failedHashingIsQueuedOnce_data()
{
   QTest::addColumn<bool>("prioritize");
   QTest::addColumn<bool>("removeDirectory");
   for (bool prioritize : { false, true })
      for (bool removeDirectory : { false, true })
         QTest::newRow(qPrintable(QString("priority=%1,directory=%2").arg(prioritize).arg(removeDirectory)))
            << prioritize << removeDirectory;
}

void CacheTest::failedHashingIsQueuedOnce()
{
   QFETCH(bool, prioritize);
   QFETCH(bool, removeDirectory);
   FM::Chunk::CHUNK_SIZE = Common::Constants::CHUNK_SIZE;
   QTemporaryDir temp;
   QVERIFY(temp.isValid());
   FM::Cache cache(QSharedPointer<HC::IHashCache>(new MockHashCache));
   const auto shared = cache.addASharedPath(temp.path() + '/');
   auto root = dynamic_cast<FM::SharedDirectory*>(cache.getSharedEntry(shared.first.ID));
   QVERIFY(root);
   const QString path = temp.filePath("retry.bin");
   {
      QFile physical(path);
      QVERIFY(physical.open(QIODevice::WriteOnly));
      QVERIFY(physical.resize(100));
   }
   const QFileInfo info(path);
   auto file = new FM::File(root, "retry.bin", info.size(), false, info.lastModified(), root->getRootDir());
   FM::FileUpdater updater(nullptr); // Drive scheduling synchronously; only failing I/O runs here.
   updater.addScannedFile(info, file);
   QVERIFY(QFile::remove(path));
   updater.computeSomeHashes();
   QCOMPARE(updater.hashingQueue.size(), qsizetype(1));
   QVERIFY(!updater.hashingQueue.next());
   QVERIFY(updater.isHashing()); // Pending retries are not an up-to-date cache.

   // Neither repeated scans nor priority requests duplicate work or bypass backoff.
   updater.addScannedFile(info, file);
   updater.addScannedFile(info, file);
   if (prioritize)
   {
      updater.prioritizeAFileToHash(file);
      updater.prioritizeAFileToHash(file);
   }
   QCOMPARE(updater.hashingQueue.size(), qsizetype(1));
   QCOMPARE(updater.hashingQueue.remainingBytes(), qint64(100));
   QVERIFY(!updater.hashingQueue.next());
   const qint64 later = updater.schedulerClock.elapsed() + 3000;
   updater.hashingQueue.releaseDueRetries(later);
   updater.hashingQueue.releaseDueRetries(later);
   QCOMPARE(updater.hashingQueue.next(), file);
   QCOMPARE(updater.hashingQueue.size(), qsizetype(1));

   updater.removeFromHashingQueue(removeDirectory ? static_cast<FM::Entry*>(root->getRootDir()) : file);
   file->del(false);
   cache.deleteEntry(file);
   QVERIFY(updater.hashingQueue.isEmpty());
   QCOMPARE(updater.hashingQueue.remainingBytes(), qint64(0));
   updater.computeSomeHashes(); // No retained pointer may be visited after deletion.
}

void CacheTest::hashingSchedulerTransitions()
{
   FM::Chunk::CHUNK_SIZE = Common::Constants::CHUNK_SIZE;
   QTemporaryDir temp;
   QVERIFY(temp.isValid());
   FM::Cache cache(QSharedPointer<HC::IHashCache>(new MockHashCache));
   const auto shared = cache.addASharedPath(temp.path() + '/');
   auto root = dynamic_cast<FM::SharedDirectory*>(cache.getSharedEntry(shared.first.ID));
   QVERIFY(root);
   auto first = new FM::File(root, "first", 100, false, QDateTime(), root->getRootDir());
   auto second = new FM::File(root, "second", 200, false, QDateTime(), root->getRootDir());
   auto third = new FM::File(root, "third", 300, false, QDateTime(), root->getRootDir());
   FM::HashingQueue queue;
   queue.enqueue(first, 100);
   queue.enqueue(second, 200);
   queue.enqueue(third, 300);
   queue.enqueue(second, 200, true);
   queue.enqueue(third, 300, true);
   QCOMPARE(queue.remainingBytes(), qint64(600));
   QCOMPARE(queue.next(), second);

   queue.finishPass(second, 150, false, 0, 3000);
   QCOMPARE(queue.next(), third); // Prioritized requests alternate after each chunk.
   QCOMPARE(queue.remainingBytes(), qint64(550));
   queue.finishPass(third, 300, true, 100, 3000);
   queue.finishPass(second, 150, true, 200, 3000);
   QCOMPARE(queue.next(), first); // A failed priority request must not block ordinary work.
   QCOMPARE(queue.retryTimeout(1000), 2100);

   queue.enqueue(third, 300, true);
   queue.releaseDueRetries(3099);
   QCOMPARE(queue.next(), first);
   QCOMPARE(queue.retryTimeout(3099), 1);
   queue.releaseDueRetries(3100);
   QCOMPARE(queue.next(), third);
   QCOMPARE(queue.retryTimeout(3100), 100);
   queue.releaseDueRetries(3200);
   QCOMPARE(queue.retryTimeout(3200), -1);

   queue.finishPass(third, 0, false, 3200, 3000);
   QCOMPARE(queue.next(), second);
   queue.remove(second); // Remove only the 150 bytes still pending, not its original 200.
   QCOMPARE(queue.remainingBytes(), qint64(100));
   queue.finishPass(second, 150, true, 3300, 3000); // A late result cannot resurrect removed work.
   QCOMPARE(queue.size(), qsizetype(1));
   queue.enqueue(first, 400); // A changed file replaces its previous contribution.
   QCOMPARE(queue.remainingBytes(), qint64(400));
   queue.enqueue(second, 200, true);
   queue.enqueue(third, 300);
   queue.finishPass(third, 300, true, 3400, 3000);
   queue.removeIf([first](FM::File* file) { return file != first; });
   QCOMPARE(queue.remainingBytes(), qint64(400));
   QCOMPARE(queue.retryTimeout(3400), -1);
   queue.remove(first);
   QVERIFY(queue.isEmpty());
   QVERIFY(!queue.next());
   QCOMPARE(queue.remainingBytes(), qint64(0));
}

void CacheTest::hashingWorkFollowsFileChanges()
{
   FM::Chunk::CHUNK_SIZE = Common::Constants::CHUNK_SIZE;
   const auto previousDuration = SETTINGS.get<quint32>("minimum_duration_when_hashing");
   const auto restore = qScopeGuard([&] { SETTINGS.set("minimum_duration_when_hashing", previousDuration); });
   SETTINGS.set("minimum_duration_when_hashing", quint32(0));
   QTemporaryDir temp;
   QVERIFY(temp.isValid());
   FM::Cache cache(QSharedPointer<HC::IHashCache>(new MockHashCache));
   const auto shared = cache.addASharedPath(temp.path() + '/');
   auto root = dynamic_cast<FM::SharedDirectory*>(cache.getSharedEntry(shared.first.ID));
   QVERIFY(root);
   const QString path = temp.filePath("changing.bin");
   const qint64 chunkSize = FM::Chunk::CHUNK_SIZE;
   {
      QFile physical(path);
      QVERIFY(physical.open(QIODevice::WriteOnly));
      QVERIFY(physical.resize(3 * chunkSize + 7));
   }
   const QFileInfo info(path);
   auto file = new FM::File(root, "changing.bin", info.size(), false, info.lastModified(), root->getRootDir());
   // Simulate a restored hash: these bytes must never enter the remaining total.
   file->getChunks().first()->setHash(Common::Hash::rand(), false);
   FM::FileUpdater updater(nullptr);
   updater.addScannedFile(info, file);
   QCOMPARE(updater.hashingQueue.remainingBytes(), 2 * chunkSize + 7);
   updater.computeSomeHashes();
   QCOMPARE(updater.hashingQueue.remainingBytes(), chunkSize + 7);
   updater.addScannedFile(info, file);
   QCOMPARE(updater.hashingQueue.remainingBytes(), chunkSize + 7);

   {
      QFile physical(path);
      QVERIFY(physical.open(QIODevice::ReadWrite));
      QVERIFY(physical.resize(4 * chunkSize + 11));
   }
   updater.addScannedFile(QFileInfo(path), file);
   QCOMPARE(updater.hashingQueue.size(), qsizetype(1));
   QCOMPARE(updater.hashingQueue.remainingBytes(), 4 * chunkSize + 11);
   updater.computeSomeHashes();
   QCOMPARE(updater.hashingQueue.remainingBytes(), 3 * chunkSize + 11);
   updater.removeFromHashingQueue(file);
   QCOMPARE(updater.hashingQueue.remainingBytes(), qint64(0));
   QVERIFY(!updater.isHashing());

   updater.addScannedFile(QFileInfo(path), file);
   file->setToUnfinished(100, { Common::Hash::rand() });
   updater.addScannedFile(QFileInfo(file->getAbsolutePath()), file);
   QVERIFY(updater.hashingQueue.isEmpty());
   QCOMPARE(file->getRemainingBytesToHash(), qint64(0));
}

void CacheTest::directoryTotalsFollowFileResizing()
{
   FM::Chunk::CHUNK_SIZE = Common::Constants::CHUNK_SIZE;
   QTemporaryDir temp;
   QVERIFY(temp.isValid());
   FM::Cache cache(QSharedPointer<HC::IHashCache>(new MockHashCache));
   const auto shared = cache.addASharedPath(temp.path() + '/');
   auto root = dynamic_cast<FM::SharedDirectory*>(cache.getSharedEntry(shared.first.ID));
   QVERIFY(root);
   auto rootDir = root->getRootDir();
   auto parent = rootDir->createSubDir("parent", true);
   auto leaf = parent->createSubDir("leaf", true);
   auto destination = rootDir->createSubDir("destination", true);
   new FM::File(root, "root.bin", 17, false, QDateTime::currentDateTime(), rootDir);
   new FM::File(root, "parent.bin", 11, false, QDateTime::currentDateTime(), parent);
   new FM::File(root, "sibling.bin", 23, false, QDateTime::currentDateTime(), leaf);
   const QString path = leaf->getAbsolutePath().toString() + "changing.bin";
   {
      QFile physical(path);
      QVERIFY(physical.open(QIODevice::WriteOnly));
      QVERIFY(physical.resize(100));
   }
   auto file = new FM::File(root, "changing.bin", 100, false, QFileInfo(path).lastModified(), leaf);
   QCOMPARE(leaf->getSize(), qint64(123));
   QCOMPARE(parent->getSize(), qint64(134));
   QCOMPARE(cache.getAmount(), qint64(151));

   // Exercise the scan's disk-change path: shrink, grow, unchanged size, empty,
   // and grow again. Unrelated files must keep contributing to every ancestor.
   for (qint64 size : { 40, 160, 160, 0, 75 })
   {
      QFile physical(path);
      QVERIFY(physical.resize(size));
      file->fileHasChangedOnDisk(QFileInfo(path));
      QCOMPARE(file->getSize(), size);
      QCOMPARE(leaf->getSize(), size + 23);
      QCOMPARE(parent->getSize(), size + 34);
      QCOMPARE(rootDir->getSize(), size + 51);
      QCOMPARE(cache.getAmount(), size + 51);
   }

   file->moveInto(destination);
   QCOMPARE(leaf->getSize(), qint64(23));
   QCOMPARE(parent->getSize(), qint64(34));
   QCOMPARE(destination->getSize(), qint64(75));
   QCOMPARE(cache.getAmount(), qint64(126));
   file->del(false);
   cache.deleteEntry(file);
   QCOMPARE(destination->getSize(), qint64(0));
   QCOMPARE(cache.getAmount(), qint64(51));
}

void CacheTest::directoryMovesAllowCompletion_data()
{
   QTest::addColumn<bool>("merge");
   QTest::addColumn<bool>("nested");
   for (bool merge : { false, true })
      for (bool nested : { false, true })
         QTest::newRow(qPrintable(QString("merge=%1,nested=%2").arg(merge).arg(nested))) << merge << nested;
}

void CacheTest::directoryMovesAllowCompletion()
{
   QFETCH(bool, merge);
   QFETCH(bool, nested);
   class CompletingFile : public FM::File
   {
   public:
      using FM::File::File;
      QSemaphore completionLocked, resumeCompletion, changingRoot;
      void complete()
      {
         QMutexLocker locker(&this->mutex);
         this->completionLocked.release();
         this->resumeCompletion.acquire();
         this->chunkComplete(this->getChunks().first().data());
      }
   protected:
      void setRootRecursively(FM::SharedEntry* root) override
      {
         this->changingRoot.release();
         FM::File::setRootRecursively(root);
      }
   };

   FM::Chunk::CHUNK_SIZE = Common::Constants::CHUNK_SIZE;
   QTemporaryDir temp;
   QVERIFY(temp.isValid());
   QVERIFY(QDir(temp.path()).mkdir("source"));
   QVERIFY(QDir(temp.path()).mkdir("destination"));
   FM::Cache cache(QSharedPointer<HC::IHashCache>(new MockHashCache));
   const auto sourceShare = cache.addASharedPath(temp.filePath("source") + '/');
   const auto targetShare = cache.addASharedPath(temp.filePath("destination") + '/');
   auto source = dynamic_cast<FM::SharedDirectory*>(cache.getSharedEntry(sourceShare.first.ID));
   auto target = dynamic_cast<FM::SharedDirectory*>(cache.getSharedEntry(targetShare.first.ID));
   QVERIFY(source && target);
   auto moved = source->getRootDir()->createSubDir("moved");
   auto parent = nested ? moved->createSubDir("nested") : moved;
   auto file = new CompletingFile(source, "finishing.unfinished", 1, false, QDateTime::currentDateTime(),
      parent, { Common::Hash::rand() });
   file->getChunks().first()->setKnownBytes(1);

   // The filesystem move precedes the cache event. Completion will rename the
   // unfinished file at its new location once its parent link has been transferred.
   const QString relative = nested ? "moved/nested/" : "moved/";
   const QString destination = temp.filePath("destination/") + relative;
   QVERIFY(QDir().mkpath(destination));
   {
      QFile physical(destination + "finishing.unfinished");
      QVERIFY(physical.open(QIODevice::WriteOnly));
      QCOMPARE(physical.write("x", 1), qint64(1));
   }

   QSemaphore completionDone, moveDone;
   std::thread completing([&] { file->complete(); completionDone.release(); });
   if (!file->completionLocked.tryAcquire(1, 5000))
      qFatal("Completion did not acquire the file lock");
   std::thread moving([&] {
      if (merge)
         target->getRootDir()->stealContent(source->getRootDir());
      else
         moved->moveInto(target->getRootDir());
      moveDone.release();
   });
   if (!file->changingRoot.tryAcquire(1, 5000))
      qFatal("Move did not reach file root propagation");
   file->resumeCompletion.release();
   if (!completionDone.tryAcquire(1, 5000) || !moveDone.tryAcquire(1, 5000))
      qFatal("Directory move deadlocked with download completion");
   completing.join();
   moving.join();

   QVERIFY(file->isComplete());
   QCOMPARE(file->getRoot(), target);
   QCOMPARE(parent->getRoot(), target);
   QCOMPARE(file->getAbsolutePath().toString(), destination + "finishing");
   QVERIFY(QFileInfo::exists(destination + "finishing"));
   QVERIFY(!source->getRootDir()->getSubDir("moved"));
   QCOMPARE(target->getRootDir()->getSubDir("moved"), moved);
   QCOMPARE(parent->getFile("finishing"), file);
   QCOMPARE(source->getRootDir()->getSize(), qint64(0));
   QCOMPARE(target->getRootDir()->getSize(), qint64(1));
   QCOMPARE(cache.getAmount(), qint64(1));
}

void CacheTest::hashingInvalidatesChangedFiles_data()
{
   QTest::addColumn<bool>("duringCall");
   QTest::addColumn<int>("sizeDelta");
   for (bool duringCall : { false, true })
      for (int sizeDelta : { 0, 1, -1 })
         QTest::newRow(qPrintable(QString("during=%1,sizeDelta=%2").arg(duringCall).arg(sizeDelta)))
            << duringCall << sizeDelta;
}

void CacheTest::hashingInvalidatesChangedFiles()
{
   QFETCH(bool, duringCall);
   QFETCH(int, sizeDelta);
   FM::Chunk::CHUNK_SIZE = Common::Constants::CHUNK_SIZE;
   const int chunkSize = FM::Chunk::CHUNK_SIZE;
   QTemporaryDir temp;
   QVERIFY(temp.isValid());
   const QString path = temp.filePath("changing.bin");
   QByteArray content(2 * chunkSize + 137, 'a');
   {
      QFile physical(path);
      QVERIFY(physical.open(QIODevice::WriteOnly));
      QCOMPARE(physical.write(content), content.size());
   }
   const QDateTime originalDate = QFileInfo(path).lastModified();
   auto hashCache = QSharedPointer<RecordingHashCache>::create();
   FM::Cache cache(hashCache);
   const auto shared = cache.addASharedPath(temp.path() + '/');
   auto root = dynamic_cast<FM::SharedDirectory*>(cache.getSharedEntry(shared.first.ID));
   QVERIFY(root);
   auto file = new FM::File(root, "changing.bin", content.size(), false, originalDate, root->getRootDir());
   const auto oldChunks = file->getChunks();
   QSignalSpy removed(&cache, &FM::Cache::chunkRemoved);
   FM::FileHasher hasher;

   const auto edit = [&] {
      content[0] = 'b'; // Change data in the chunk that has already been hashed.
      content.resize(content.size() + sizeDelta, 'c');
      QFile physical(path);
      if (!physical.open(QIODevice::ReadWrite) || physical.write(content) != content.size() ||
          !physical.resize(content.size()) || !physical.flush())
         return false;
      // Avoid timestamp-resolution/timing assumptions. Size-only changes deliberately
      // retain the original timestamp, and stay within the same number of chunks.
      return physical.setFileTime(sizeDelta ? originalDate : originalDate.addSecs(10), QFileDevice::FileModificationTime);
   };

   if (duringCall)
   {
      bool edited = false;
      const auto connection = connect(&cache, &FM::Cache::chunkHashKnown, &cache,
         [&](const auto& chunk) { if (chunk == oldChunks.first()) edited = edit(); }, Qt::DirectConnection);
      const auto disconnectCallback = qScopeGuard([&] { disconnect(connection); });
      QVERIFY(!hasher.start(file, 1, nullptr, true));
      QVERIFY(edited);
   }
   else
   {
      QVERIFY(!hasher.start(file, 1, nullptr, true));
      QVERIFY(oldChunks.first()->hasHash());
      QVERIFY(edit());
      QVERIFY(!hasher.start(file, 1, nullptr, true));
   }

   QCOMPARE(removed.count(), oldChunks.size());
   for (const auto& chunk : oldChunks)
      QVERIFY(!chunk->isOwnedBy(file));
   for (const auto& chunk : file->getChunks())
      QVERIFY(!chunk->hasHash());
   hasher.flushHashes();
   QVERIFY(hashCache->writes.isEmpty()); // Retired progress must not be persisted.
   QCOMPARE(file->getSize(), content.size());
   QCOMPARE(file->getDateLastModified(), QFileInfo(path).lastModified());

   QVERIFY(hasher.start(file));
   QCOMPARE(hashCache->writes.size(), 1);
   QCOMPARE(hashCache->savedDate, QFileInfo(path).lastModified());
   const auto chunks = file->getChunks();
   for (int i = 0; i < chunks.size(); ++i)
   {
      const int length = qMin(chunkSize, int(content.size()) - i * chunkSize);
      Common::Hasher expected;
      expected.addData(std::span<const char>(content).subspan(i * chunkSize, length));
      QCOMPARE(chunks[i]->getHash(), expected.getResult());
      QCOMPARE(hashCache->writes.first()[i], expected.getResult());
   }
}

void CacheTest::redownloadStopsActiveHashing()
{
   FM::Chunk::CHUNK_SIZE = Common::Constants::CHUNK_SIZE;
   QTemporaryDir temp;
   QVERIFY(temp.isValid());
   const QString path = temp.filePath("redownload.bin");
   const qint64 size = qint64(2) * FM::Chunk::CHUNK_SIZE;
   {
      QFile physical(path);
      QVERIFY(physical.open(QIODevice::WriteOnly));
      QVERIFY(physical.resize(size));
   }
   auto hashCache = QSharedPointer<RecordingHashCache>::create();
   FM::Cache cache(hashCache);
   const auto shared = cache.addASharedPath(temp.path() + '/');
   auto root = dynamic_cast<FM::SharedDirectory*>(cache.getSharedEntry(shared.first.ID));
   QVERIFY(root);
   auto file = new FM::File(root, "redownload.bin", size, false, QFileInfo(path).lastModified(), root->getRootDir());
   const auto oldChunks = file->getChunks();
   const QList<Common::Hash> replacementHashes { Common::Hash::rand(), Common::Hash::rand() };
   FM::FileHasher hasher;
   QSemaphore hashingReached, removalReached, hashingDone, resetDone;
   std::exception_ptr hashingError, resetError;

   // Connect before start() installs the hasher's removal callback. The worker then
   // attempts a file lock while setToUnfinished() is delivering the removal signal.
   const auto removalConnection = connect(&cache, &FM::Cache::entryRemoved, &cache, [&](FM::Entry* entry) {
      if (entry == file)
         removalReached.release();
   }, Qt::DirectConnection);
   const auto hashingConnection = connect(&cache, &FM::Cache::chunkHashKnown, &cache, [&](const QSharedPointer<FM::Chunk>& chunk) {
      if (chunk == oldChunks.first())
      {
         hashingReached.release();
         if (!removalReached.tryAcquire(1, 5000))
            qFatal("Re-download did not reach the removal notification");
         file->getChunks();
      }
   }, Qt::DirectConnection);

   const auto disconnectCallbacks = qScopeGuard([&] {
      disconnect(removalConnection);
      disconnect(hashingConnection);
   });

   std::thread hashing([&] {
      try { hasher.start(file, 0, nullptr, true); }
      catch (...) { hashingError = std::current_exception(); }
      hashingDone.release();
   });
   if (!hashingReached.tryAcquire(1, 5000))
      qFatal("Hasher did not reach the first chunk");
   std::thread resetting([&] {
      try { file->setToUnfinished(size, replacementHashes); }
      catch (...) { resetError = std::current_exception(); }
      resetDone.release();
   });
   if (!hashingDone.tryAcquire(1, 5000) || !resetDone.tryAcquire(1, 5000))
      qFatal("Re-download deadlocked with active hashing");
   hashing.join();
   resetting.join();

   QVERIFY(!hashingError);
   QVERIFY(!resetError);
   QVERIFY(!file->isComplete());
   QCOMPARE(file->getName(), QString("redownload.bin.unfinished"));
   QCOMPARE(QFileInfo(file->getAbsolutePath()).size(), size);
   for (const auto& chunk : oldChunks)
      QVERIFY(!chunk->isOwnedBy(file));

   // A scheduler may already have selected this file before it became unfinished.
   // A fresh hasher has no stop flag to mask an incorrect acceptance of that work.
   FM::FileHasher lateHasher;
   QVERIFY(!lateHasher.start(file));
   const auto newChunks = file->getChunks();
   QCOMPARE(newChunks.size(), replacementHashes.size());
   for (int i = 0; i < newChunks.size(); ++i)
   {
      QCOMPARE(newChunks[i]->getHash(), replacementHashes[i]);
      QCOMPARE(newChunks[i]->getKnownBytes(), 0);
   }
   hasher.flushHashes();
   QVERIFY(hashCache->writes.isEmpty());
}

void CacheTest::deferredHashPersistence_data()
{
   QTest::addColumn<QString>("finish");
   for (const char* finish : { "flush", "stop", "error", "remove", "replace", "destructor" })
      QTest::newRow(finish) << QString::fromLatin1(finish);
}

void CacheTest::deferredHashPersistence()
{
   QFETCH(QString, finish);
   FM::Chunk::CHUNK_SIZE = Common::Constants::CHUNK_SIZE;
   QTemporaryDir temp;
   QVERIFY(temp.isValid());
   const QString path = temp.filePath("batch.bin");
   const qint64 size = qint64(2) * FM::Chunk::CHUNK_SIZE + 1;
   {
      QFile physical(path);
      QVERIFY(physical.open(QIODevice::WriteOnly));
      QVERIFY(physical.resize(size));
   }
   auto hashCache = QSharedPointer<RecordingHashCache>::create();
   FM::Cache cache(hashCache);
   const auto shared = cache.addASharedPath(temp.path() + '/');
   auto root = dynamic_cast<FM::SharedDirectory*>(cache.getSharedEntry(shared.first.ID));
   QVERIFY(root);
   auto file = new FM::File(root, "batch.bin", size, false, QFileInfo(path).lastModified(), root->getRootDir());
   auto hasher = std::make_unique<FM::FileHasher>();
   QSignalSpy notifications(&cache, &FM::Cache::chunkHashKnown);
   QVERIFY(!hasher->start(file, 1, nullptr, true));
   QVERIFY(!hasher->start(file, 1, nullptr, true));
   QCOMPARE(notifications.count(), 2); // Delivery remains immediate even while persistence is deferred.
   QVERIFY(hashCache->writes.isEmpty());

   if (finish == "remove")
   {
      file->del(false);
      cache.deleteEntry(file);
      hasher->flushHashes();
      QVERIFY(hashCache->writes.isEmpty());
      return;
   }
   if (finish == "replace")
   {
      file->fileHasChangedOnDisk(QFileInfo(path));
      hasher->flushHashes();
      QVERIFY(hashCache->writes.isEmpty());
      return;
   }
   if (finish == "stop")
   {
      hasher->stop();
      QCOMPARE(hashCache->writes.size(), 1);
      QVERIFY(!hasher->start(file, 1, nullptr, true));
   }
   else if (finish == "error")
   {
      auto missing = new FM::File(root, "missing.bin", 1, false, QDateTime::currentDateTime(), root->getRootDir());
      QVERIFY_THROWS_EXCEPTION(FM::IOErrorException, hasher->start(missing, 1, nullptr, true));
   }
   else if (finish == "destructor")
      hasher.reset();
   else
      hasher->flushHashes();
   QCOMPARE(hashCache->writes.size(), 1);
   QCOMPARE(hashCache->writes.first().size(), 3);
   QVERIFY(!hashCache->writes.first()[0].isNull());
   QVERIFY(!hashCache->writes.first()[1].isNull());
   QVERIFY(hashCache->writes.first()[2].isNull());
   if (hasher)
   {
      hasher->flushHashes();
      QCOMPARE(hashCache->writes.size(), 1); // No redundant save of unchanged progress.
      QVERIFY(hasher->start(file, 1, nullptr, true));
      QCOMPARE(hashCache->writes.size(), 2); // Completion saves immediately, even in deferred mode.
      QVERIFY(!hashCache->writes.last()[2].isNull());
   }
}

void CacheTest::hashResultsOnlySendOutstandingChunks()
{
   FM::Chunk::CHUNK_SIZE = Common::Constants::CHUNK_SIZE;
   QTemporaryDir temp;
   QVERIFY(temp.isValid());
   FM::Cache cache(QSharedPointer<HC::IHashCache>(new MockHashCache));
   const auto shared = cache.addASharedPath(temp.path() + '/');
   auto root = dynamic_cast<FM::SharedDirectory*>(cache.getSharedEntry(shared.first.ID));
   QVERIFY(root);
   const QList<Common::Hash> hashes { Common::Hash::rand(), Common::Hash::rand(), Common::Hash::rand(), Common::Hash::rand() };
   auto file = new FM::File(root, "hashes.bin", qint64(4) * FM::Chunk::CHUNK_SIZE, false,
      QDateTime::currentDateTime(), root->getRootDir(), { hashes[0], hashes[1], Common::Hash(), Common::Hash() });
   const auto chunks = file->getChunks();
   Protos::Common::Entry entry;
   file->populateEntry(&entry, true);
   entry.mutable_chunks(1)->clear_hash(); // Requester knows chunk 0; chunk 1 can be sent immediately.
   FM::FileUpdater updater(nullptr); // Keep the worker stopped; deliver hash notifications explicitly.
   FM::GetHashesResult result(entry, cache, updater);
   QSignalSpy received(&result, &FM::IGetHashesResult::nextHash);
   const auto response = result.start();
   QCOMPARE(response.status(), Protos::Core::GetHashesResult_Status_OK);
   QCOMPARE(response.nb_hash(), 3);
   QCOMPARE(received.size(), 1);

   cache.onChunkHashKnown(chunks[0]); // Not requested.
   cache.onChunkHashKnown(chunks[1]); // Already sent by start(), while others remain pending.
   QCOMPARE(received.size(), 1);

   auto unrelated = new FM::File(root, "other.bin", FM::Chunk::CHUNK_SIZE, false,
      QDateTime::currentDateTime(), root->getRootDir(), { hashes[0] });
   cache.onChunkHashKnown(unrelated->getChunks().first());
   QCOMPARE(received.size(), 1);

   chunks[3]->setHash(hashes[3]); // Requested hashes may arrive out of order.
   cache.onChunkHashKnown(chunks[3]);
   QCOMPARE(received.size(), 2);
   cache.onChunkHashKnown(chunks[3]); // Duplicate while chunk 2 is still pending.
   QCOMPARE(received.size(), 2);

   chunks[2]->setHash(hashes[2]);
   cache.onChunkHashKnown(chunks[2]);
   QCOMPARE(received.size(), response.nb_hash());
   for (const auto& chunk : chunks)
      cache.onChunkHashKnown(chunk); // Late notifications after completion must also be ignored.
   QCOMPARE(received.size(), response.nb_hash());

   const QList<int> expectedOrder { 1, 3, 2 };
   for (int i = 0; i < received.size(); ++i)
   {
      const auto value = qvariant_cast<Protos::Core::HashResult>(received[i][0]);
      QCOMPARE(value.num(), expectedOrder[i]);
      QCOMPARE(QByteArray::fromStdString(value.hash().hash()), QByteArray(hashes[expectedOrder[i]].getData(), Common::Hash::HASH_SIZE));
   }
}

void CacheTest::writerRegistrationSurvivesFileReset()
{
   FM::Chunk::CHUNK_SIZE = Common::Constants::CHUNK_SIZE;
   const bool savedIntegrity = SETTINGS.get<bool>("check_received_data_integrity");
   const auto restore = qScopeGuard([&] { SETTINGS.set("check_received_data_integrity", savedIntegrity); });
   // Reach the write-open reset path rather than failing earlier when rehashing the missing prefix.
   SETTINGS.set("check_received_data_integrity", false);
   QTemporaryDir temp;
   QVERIFY(temp.isValid());
   FM::Cache cache(QSharedPointer<HC::IHashCache>(new MockHashCache));
   const auto shared = cache.addASharedPath(temp.path() + '/');
   auto root = dynamic_cast<FM::SharedDirectory*>(cache.getSharedEntry(shared.first.ID));
   QVERIFY(root);
   const QByteArray content("download");
   auto file = new FM::File(root, "download.bin", content.size(), false, QDateTime::currentDateTime(),
      root->getRootDir(), QList<Common::Hash>(), true);
   auto chunk = file->getChunks().first();
   const QString path = file->getAbsolutePath();
   QSignalSpy resets(&cache, &FM::Cache::chunkRemoved);

   for (int attempt = 0; attempt < 2; ++attempt)
   {
      {
         auto writer = chunk->getDataWriter();
         QVERIFY(!writer->write(content.constData(), 1));
      }
      // Simulate deletion after the last writer released its pooled handle.
      cache.getFilePool().forceReleaseAll(path);
      QVERIFY(QFile::remove(path));
      QVERIFY_THROWS_EXCEPTION(FM::FileResetException, chunk->getDataWriter());
      QCOMPARE(chunk->getKnownBytes(), 0);
      QCOMPARE(resets.count(), attempt + 1);
      QCOMPARE(QFileInfo(path).size(), content.size());
      const auto failedHandles = cache.getFilePool().takeAll(path);
      qDeleteAll(failedHandles);
      QVERIFY(failedHandles.isEmpty()); // A failed constructor must not retain an acquired handle.

      {
         auto writer = chunk->getDataWriter();
         QVERIFY(!writer->write(content.constData(), 1));
      }
      // The last successful adapter must release its handle. A leaked registration would make
      // open() allocate a second handle instead of reusing the released one.
      QFile* pooled = cache.getFilePool().open(path, QIODevice::ReadWrite | QIODevice::Unbuffered);
      const auto handles = cache.getFilePool().takeAll(path);
      const bool opened = pooled != nullptr;
      qDeleteAll(handles);
      QVERIFY(opened);
      QCOMPARE(handles.size(), 1);
      chunk->setKnownBytes(0);
   }
   {
      auto writer = chunk->getDataWriter();
      QVERIFY(writer->write(content.constData(), content.size()));
   }
   QVERIFY(file->isComplete());
   QFile downloaded(temp.filePath("download.bin"));
   QVERIFY(downloaded.open(QIODevice::ReadOnly));
   QCOMPARE(downloaded.readAll(), content);
}

void CacheTest::hashingRespectsChunkBoundaries_data()
{
   QTest::addColumn<int>("bufferSize");
   QTest::addColumn<bool>("partialTail");
   QTest::addColumn<bool>("resume");
   for (int bufferSize : { 4 * 1024 * 1024, 3 * 1024 * 1024, 2 * Common::Constants::CHUNK_SIZE })
      for (bool partialTail : { false, true })
         for (bool resume : { false, true })
            QTest::newRow(qPrintable(QString("buffer=%1,partial=%2,resume=%3").arg(bufferSize).arg(partialTail).arg(resume)))
               << bufferSize << partialTail << resume;
}

void CacheTest::hashingRespectsChunkBoundaries()
{
   QFETCH(int, bufferSize);
   QFETCH(bool, partialTail);
   QFETCH(bool, resume);
   const int savedChunkSize = FM::Chunk::CHUNK_SIZE;
   const quint32 savedBufferSize = SETTINGS.get<quint32>("buffer_size_reading");
   const auto restore = qScopeGuard([&] {
      FM::Chunk::CHUNK_SIZE = savedChunkSize;
      SETTINGS.set("buffer_size_reading", savedBufferSize);
   });
   const int chunkSize = Common::Constants::CHUNK_SIZE;
   FM::Chunk::CHUNK_SIZE = chunkSize;
   SETTINGS.set("buffer_size_reading", quint32(bufferSize));
   QTemporaryDir temp;
   QVERIFY(temp.isValid());
   QByteArray content(2 * chunkSize + (partialTail ? 137 : 0), Qt::Uninitialized);
   for (qsizetype offset = 0; offset < content.size(); offset += chunkSize)
      std::fill_n(content.data() + offset, qMin(qsizetype(chunkSize), content.size() - offset), char('a' + offset / chunkSize));
   const QString path = temp.filePath("hash.bin");
   {
      QFile physical(path);
      QVERIFY(physical.open(QIODevice::WriteOnly));
      QCOMPARE(physical.write(content), content.size());
   }
   auto hashCache = QSharedPointer<RecordingHashCache>::create();
   FM::Cache cache(hashCache);
   const auto shared = cache.addASharedPath(temp.path() + '/');
   auto root = dynamic_cast<FM::SharedDirectory*>(cache.getSharedEntry(shared.first.ID));
   QVERIFY(root);
   auto file = new FM::File(root, "hash.bin", content.size(), false, QFileInfo(path).lastModified(), root->getRootDir());
   const auto chunks = file->getChunks();
   QCOMPARE(chunks.size(), partialTail ? 3 : 2);
   QList<int> notifiedChunks;
   connect(&cache, &FM::Cache::chunkHashKnown, &cache,
      [&](const auto& chunk) { notifiedChunks.append(chunk->getNum()); });
   FM::FileHasher hasher;
   int amountHashed = 0;
   if (resume)
   {
      QVERIFY(!hasher.start(file, 1, &amountHashed));
      QCOMPARE(amountHashed, chunkSize);
      QVERIFY(chunks.first()->hasHash());
      QVERIFY(!chunks[1]->hasHash());
   }
   QVERIFY(hasher.start(file, 0, &amountHashed));
   QCOMPARE(hashCache->writes.size(), resume ? 2 : 1);
   QCOMPARE(hashCache->savedDate, QFileInfo(path).lastModified());
   QCOMPARE(amountHashed, content.size()); // Resuming must skip the already hashed full chunk.
   QCOMPARE(file->getSize(), content.size());
   QCOMPARE(notifiedChunks.size(), chunks.size());
   for (int i = 0; i < chunks.size(); ++i)
   {
      const int size = qMin(chunkSize, int(content.size()) - i * chunkSize);
      Common::Hasher expected;
      expected.addData(std::span<const char>(content).subspan(i * chunkSize, size));
      QCOMPARE(chunks[i]->getHash(), expected.getResult());
      QCOMPARE(chunks[i]->getKnownBytes(), size);
      QCOMPARE(notifiedChunks[i], i);
   }
}

void CacheTest::handlesReopenAfterCompletion_data()
{
   QTest::addColumn<bool>("oldAdaptersFirst");
   QTest::addColumn<bool>("resumeOldReader");
   QTest::newRow("old-first-new-reader") << true << false;
   QTest::newRow("new-first-new-reader") << false << false;
   QTest::newRow("old-first-retained-reader") << true << true;
   QTest::newRow("new-first-retained-reader") << false << true;
}

void CacheTest::handlesReopenAfterCompletion()
{
   QFETCH(bool, oldAdaptersFirst);
   QFETCH(bool, resumeOldReader);
   FM::Chunk::CHUNK_SIZE = Common::Constants::CHUNK_SIZE;
   QTemporaryDir temp;
   QVERIFY(temp.isValid());
   FM::Cache cache(QSharedPointer<HC::IHashCache>(new MockHashCache));
   const auto shared = cache.addASharedPath(temp.path() + '/');
   auto root = dynamic_cast<FM::SharedDirectory*>(cache.getSharedEntry(shared.first.ID));
   QVERIFY(root);
   const QByteArray content("completed download");
   Common::Hasher hasher;
   hasher.addData(std::span<const char>(content));
   auto file = new FM::File(root, "download.bin", content.size(), false, QDateTime::currentDateTime(),
      root->getRootDir(), { hasher.getResult() }, true);
   auto chunk = file->getChunks().first();
   auto oldReader = chunk->getDataReader();
   auto oldWriter = chunk->getDataWriter();
   QVERIFY(!oldWriter->write(content.constData(), 1));
   QByteArray buffer(SETTINGS.get<quint32>("buffer_size_reading"), Qt::Uninitialized);
   QCOMPARE(oldReader->read(buffer.data(), 0), 1);
   QVERIFY(oldWriter->write(content.constData() + 1, content.size() - 1));
   QVERIFY(file->isComplete());
   QVERIFY(!QFileInfo::exists(temp.filePath("download.bin.unfinished")));

   if (resumeOldReader)
   {
      QCOMPARE(oldReader->read(buffer.data(), 1), content.size() - 1);
      QCOMPARE(buffer.first(content.size() - 1), content.sliced(1));
   }
   auto newReader = chunk->getDataReader();
   QCOMPARE(newReader->read(buffer.data(), 0), content.size());
   QCOMPARE(buffer.first(content.size()), content);
   auto newWriter = chunk->getDataWriter();
   // The public chunk is already complete; exercise the reopened physical writer without changing its data.
   QCOMPARE(file->write(content.constData(), content.size(), 0), content.size());

   if (oldAdaptersFirst)
   {
      oldReader.clear();
      oldWriter.clear();
   }
   else
   {
      newReader.clear();
      newWriter.clear();
   }
   auto& survivingReader = oldAdaptersFirst ? newReader : oldReader;
   QCOMPARE(survivingReader->read(buffer.data(), 0), content.size());
   QCOMPARE(buffer.first(content.size()), content);
   QCOMPARE(file->write(content.constData(), content.size(), 0), content.size());
   oldReader.clear();
   oldWriter.clear();
   newReader.clear();
   newWriter.clear();

   // Both generations have gone: a fresh pair must still open and close normally.
   auto reader = chunk->getDataReader();
   auto writer = chunk->getDataWriter();
   QCOMPARE(reader->read(buffer.data(), 0), content.size());
   QCOMPARE(file->write(content.constData(), content.size(), 0), content.size());
}

void CacheTest::readerReopenFailureAfterCompletion()
{
   FM::Chunk::CHUNK_SIZE = Common::Constants::CHUNK_SIZE;
   QTemporaryDir temp;
   QVERIFY(temp.isValid());
   FM::Cache cache(QSharedPointer<HC::IHashCache>(new MockHashCache));
   const auto shared = cache.addASharedPath(temp.path() + '/');
   auto root = dynamic_cast<FM::SharedDirectory*>(cache.getSharedEntry(shared.first.ID));
   QVERIFY(root);
   auto file = new FM::File(root, "download.bin", 1, false, QDateTime::currentDateTime(),
      root->getRootDir(), QList<Common::Hash>(), true);
   auto chunk = file->getChunks().first();
   auto oldReader = chunk->getDataReader();
   auto writer = chunk->getDataWriter();
   QVERIFY(chunk->write("x", 1));
   const QString path = temp.filePath("download.bin");
   const QString movedPath = temp.filePath("moved.bin");
   QVERIFY(QFile::rename(path, movedPath)); // Completion closed both handles.
   QByteArray buffer(SETTINGS.get<quint32>("buffer_size_reading"), Qt::Uninitialized);
   QVERIFY_THROWS_EXCEPTION(FM::IOErrorException, oldReader->read(buffer.data(), 0));
   QVERIFY_THROWS_EXCEPTION(FM::UnableToOpenFileInReadModeException, chunk->getDataReader());
   QVERIFY(QFile::rename(movedPath, path));
   auto reader = chunk->getDataReader();
   oldReader.clear(); // The failed constructor must not have changed the adapter count.
   QCOMPARE(reader->read(buffer.data(), 0), 1);
   QCOMPARE(buffer[0], 'x');
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

void CacheTest::directoryLookupAllowsSizePropagation_data()
{
   QTest::addColumn<bool>("create");
   QTest::newRow("lookup") << false;
   QTest::newRow("create-path") << true;
}

void CacheTest::directoryLookupAllowsSizePropagation()
{
   QFETCH(bool, create);
   class LockedDirectory : public InspectableDirectory
   {
   public:
      using InspectableDirectory::InspectableDirectory;
      void lock() { this->mutex.lock(); }
      void unlock() { this->mutex.unlock(); }
   };
   QTemporaryDir temp;
   QVERIFY(temp.isValid());
   FM::Cache cache(QSharedPointer<HC::IHashCache>(new MockHashCache));
   const auto shared = cache.addASharedPath(temp.path() + '/');
   auto root = dynamic_cast<FM::SharedDirectory*>(cache.getSharedEntry(shared.first.ID));
   QVERIFY(root);
   auto parent = new InspectableDirectory(root, "parent", root->getRootDir());
   auto child = new LockedDirectory(root, "child", parent);
   auto leaf = child->createSubDir("leaf");
   QSemaphore started, done;
   FM::Entry* result = nullptr;
   child->lock();
   std::thread traversal([&] {
      started.release();
      result = create ? parent->createSubDirs({ "child", "leaf" }, false)
                      : parent->getEntry(Common::Path(QStringList { "child", "leaf" }));
      done.release();
   });
   const bool workerStarted = started.tryAcquire(1, 5000);
   // Keep the descendant locked until the traversal has had time to block there.
   // An ancestor must remain available for a child-to-parent size update.
   const bool finishedWhileLocked = done.tryAcquire(1, 100);
   const bool parentAvailable = parent->canLock();
   if (parentAvailable)
      child->fileSizeChanged(0, 7);
   child->unlock();
   traversal.join();
   QVERIFY(workerStarted);
   QVERIFY(!finishedWhileLocked);
   QVERIFY(parentAvailable);
   QCOMPARE(result, leaf);
   QCOMPARE(parent->getSize(), qint64(7));
   QCOMPARE(root->getRootDir()->getSize(), qint64(7));
}

void CacheTest::directoryCreationDefersDeletion()
{
   class ProbeFile : public FM::File
   {
   public:
      using FM::File::File;
      bool* destroyed = nullptr;
      ~ProbeFile() override { *this->destroyed = true; }
   };
   bool destroyed = false;
   QTemporaryDir temp;
   QVERIFY(temp.isValid());
   FM::Cache cache(QSharedPointer<HC::IHashCache>(new MockHashCache));
   const auto shared = cache.addASharedPath(temp.path() + '/');
   auto root = dynamic_cast<FM::SharedDirectory*>(cache.getSharedEntry(shared.first.ID));
   QVERIFY(root);
   auto retired = new ProbeFile(root, "retired.bin", 0, false, QDateTime::currentDateTime(), root->getRootDir());
   retired->destroyed = &destroyed;
   retired->del(false);
   bool callbackRan = false;
   bool destroyedDuringTraversal = false;
   const auto connection = connect(&cache, &FM::Cache::entryAdded, &cache, [&](FM::Entry*) {
      if (!callbackRan)
      {
         callbackRan = true;
         cache.deleteEntry(retired);
         destroyedDuringTraversal = destroyed;
      }
   }, Qt::DirectConnection);
   const auto disconnectCallback = qScopeGuard([&] { disconnect(connection); });
   auto leaf = root->getRootDir()->createSubDirs({ "child", "leaf" }, false);
   QVERIFY(leaf);
   QVERIFY(callbackRan);
   QVERIFY(!destroyedDuringTraversal);
   QCoreApplication::sendPostedEvents(&cache, QEvent::MetaCall);
   QVERIFY(destroyed);
   QCOMPARE(root->getRootDir()->getEntry(Common::Path(QStringList { "child", "leaf" })), leaf);
   QVERIFY(!root->getRootDir()->getEntry(Common::Path(QStringList { "missing", "leaf" })));
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

void CacheTest::watcherRecovery_data()
{
   QTest::addColumn<QString>("failure");
   QTest::addColumn<bool>("watchedFile");
   for (const QString& failure : { "overflow", "enum-error", "short-header", "long-name", "odd-name",
         "bad-offset", "short-next", "oversize", "completion-error", "rearm-error" })
      for (bool watchedFile : { false, true })
         QTest::newRow(qPrintable(failure + (watchedFile ? "-file" : "-directory"))) << failure << watchedFile;
}

void CacheTest::watcherRecovery()
{
#ifdef Q_OS_WIN32
   QFETCH(QString, failure);
   QFETCH(bool, watchedFile);
   QTemporaryDir temp;
   QVERIFY(temp.isValid());
   const QString path = temp.path() + '/';
   const QString filename = watchedFile ? "original.txt" : "";
   QFile physical(temp.filePath("original.txt"));
   QVERIFY(physical.open(QIODevice::WriteOnly));
   physical.close();
   FM::DirWatcherWin watcher;
   FM::DirWatcherWin::Dir* dir;
   if (failure == "rearm-error")
   {
      // A valid ordinary-file handle cannot issue directory notifications.
      HANDLE handle = CreateFileW(reinterpret_cast<LPCWSTR>(physical.fileName().utf16()), GENERIC_READ,
         FILE_SHARE_READ | FILE_SHARE_WRITE, nullptr, OPEN_EXISTING, FILE_FLAG_OVERLAPPED, nullptr);
      QVERIFY(handle != INVALID_HANDLE_VALUE);
      HANDLE event = CreateEvent(nullptr, FALSE, FALSE, nullptr);
      QVERIFY(event);
      dir = new FM::DirWatcherWin::Dir(handle, event, path, filename);
      watcher.dirs << dir;
   }
   else
   {
      QVERIFY(watcher.addPath(path, filename));
      dir = watcher.dirs.first();
      QVERIFY(CancelIoEx(dir->handle, &dir->overlapped));
      DWORD ignored;
      GetOverlappedResult(dir->handle, &dir->overlapped, &ignored, TRUE);
      ResetEvent(dir->overlapped.hEvent);
   }
   dir->pendingRenameName = "stale-name.txt";
   memset(dir->buffer, 0, sizeof(dir->buffer));
   auto record = reinterpret_cast<FILE_NOTIFY_INFORMATION*>(dir->buffer);
   record->Action = FILE_ACTION_MODIFIED;
   const QString notifiedName = "original.txt";
   record->FileNameLength = notifiedName.size() * sizeof(wchar_t);
   memcpy(record->FileName, notifiedName.utf16(), record->FileNameLength);
   DWORD bytes = offsetof(FILE_NOTIFY_INFORMATION, FileName) + record->FileNameLength;
   DWORD error = ERROR_SUCCESS;
   if (failure == "overflow") bytes = 0;
   if (failure == "enum-error") error = ERROR_NOTIFY_ENUM_DIR;
   if (failure == "short-header") bytes = 4;
   if (failure == "long-name") record->FileNameLength = FM::NOTIFY_BUFFER_SIZE;
   if (failure == "odd-name") --record->FileNameLength;
   if (failure == "bad-offset") record->NextEntryOffset = 1;
   if (failure == "short-next") record->NextEntryOffset = bytes;
   if (failure == "oversize") bytes = FM::NOTIFY_BUFFER_SIZE + 1;
   if (failure == "completion-error") error = ERROR_ACCESS_DENIED;

   const auto events = watcher.processCompletion(dir, bytes, error);
   QVERIFY(!events.isEmpty());
   QCOMPARE(events.first().type, failure == "rearm-error" ? FM::WatcherEvent::CONTENT_CHANGED : FM::WatcherEvent::RESCAN);
   const bool lost = failure == "completion-error" || failure == "rearm-error";
   QCOMPARE(events.last().type, lost ? FM::WatcherEvent::WATCH_LOST : FM::WatcherEvent::RESCAN);
   QCOMPARE(events.last().path1, QDir::cleanPath(path + filename));
   QCOMPARE(events.last().isWatchedFile, watchedFile);
   QCOMPARE(watcher.nbWatchedPath(), lost ? 0 : 1);
   if (!lost)
   {
      QVERIFY(dir->pendingRenameName.isEmpty());
      QVERIFY(physical.open(QIODevice::Append));
      QCOMPARE(physical.write("x"), qint64(1));
      physical.close();
      const auto next = watcher.waitEvent(1000);
      QVERIFY(std::any_of(next.begin(), next.end(), [](const FM::WatcherEvent& event) {
         return event.type == FM::WatcherEvent::CONTENT_CHANGED;
      }));
   }
#else
   QSKIP("Windows notification regression");
#endif
}

void CacheTest::updaterWatcherRecovery_data()
{
   QTest::addColumn<bool>("watchedFile");
   QTest::newRow("directory") << false;
   QTest::newRow("file") << true;
}

void CacheTest::updaterWatcherRecovery()
{
   QFETCH(bool, watchedFile);
   QTemporaryDir temp;
   QVERIFY(temp.isValid());
   QFile physical(temp.filePath("original.txt"));
   QVERIFY(physical.open(QIODevice::WriteOnly));
   physical.close();
   FM::FileManager manager(QSharedPointer<HC::IHashCache>(new MockHashCache));
   const QString path = watchedFile ? physical.fileName() : temp.path() + '/';
   manager.addASharedPath(path);
   QTRY_COMPARE(manager.getCacheStatus(), FM::IFileManager::UP_TO_DATE);
   FM::FileUpdater updater(&manager);
   auto entry = manager.getEntry(Common::Path(path));
   QVERIFY(entry);
   const auto rescan = FM::WatcherEvent(FM::WatcherEvent::RESCAN, path, watchedFile);
   updater.processEvents({ rescan, rescan });
   QCOMPARE(updater.entriesToScan, QList<FM::Entry*> { entry });
   QVERIFY(updater.unwatchableEntries.isEmpty());
   const auto lost = FM::WatcherEvent(FM::WatcherEvent::WATCH_LOST, path, watchedFile);
   updater.processEvents({ lost, lost });
   QCOMPARE(updater.entriesToScan, QList<FM::Entry*> { entry });
   QCOMPARE(updater.unwatchableEntries, QList<FM::Entry*> { entry });
   QCOMPARE(updater.nextWaitTimeout(0, 0), int(SETTINGS.get<quint32>("scan_period_unwatchable_dirs")));
   if (watchedFile)
   {
      int removals = 0;
      connect(&updater, &FM::FileUpdater::deleteSharedEntry, &updater,
         [&](FM::SharedEntry* removed) { if (removed == entry->getRoot()) ++removals; });
      QVERIFY(QFile::remove(physical.fileName()));
      updater.scan(entry);
      QCOMPARE(removals, 1);
   }
   updater.rmRoot(entry->getRoot(), nullptr);
   QVERIFY(updater.entriesToScan.isEmpty());
   QVERIFY(updater.unwatchableEntries.isEmpty());
   updater.processEvents({ rescan, lost });
   QVERIFY(updater.entriesToScan.isEmpty());
   QVERIFY(updater.unwatchableEntries.isEmpty());
}

void CacheTest::newDirectoryPreservesFinalComponent_data()
{
   QTest::addColumn<QString>("destination");
   QTest::addColumn<QString>("parentPath");
   for (const QString& destination : { "missing", "invalid", "valid" })
      for (const QString& parentPath : { "/", "/parent/deeper/", "parent/deeper" })
         QTest::newRow(qPrintable(destination + ':' + parentPath)) << destination << parentPath;
}

void CacheTest::newDirectoryPreservesFinalComponent()
{
   QFETCH(QString, destination);
   QFETCH(QString, parentPath);
   QTemporaryDir temp;
   QVERIFY(temp.isValid());
   FM::Cache cache(QSharedPointer<HC::IHashCache>(new MockHashCache));
   const auto shared = cache.addASharedPath(temp.path() + '/');
   auto root = dynamic_cast<FM::SharedDirectory*>(cache.getSharedEntry(shared.first.ID));
   QVERIFY(root);
   root->getRootDir()->createSubDir("parent", true);

   Protos::Common::Entry entry;
   entry.set_type(Protos::Common::Entry::DIR);
   entry.set_path(parentPath.toStdString());
   entry.set_name("child");
   if (destination != "missing")
   {
      const Common::Hash id = destination == "valid" ? shared.first.ID : Common::Hash::rand();
      entry.mutable_shared_entry()->mutable_id()->set_hash(id.getData(), Common::Hash::HASH_SIZE);
   }
   cache.newDirectory(entry);

   const QString expected = temp.filePath(parentPath == "/" ? "child" : "parent/deeper/child");
   QVERIFY2(QFileInfo(expected).isDir(), qPrintable(expected));
   auto created = dynamic_cast<FM::Directory*>(cache.getEntry(Common::Path(expected + '/')));
   QVERIFY(created);
   QCOMPARE(created->getName(), QString("child"));
   QCOMPARE(created->getRoot(), static_cast<FM::SharedEntry*>(root));

   // Repeating the request must reuse both the physical and cached directory.
   cache.newDirectory(entry);
   QCOMPARE(cache.getEntry(Common::Path(expected + '/')), static_cast<FM::Entry*>(created));
   QVERIFY(QFileInfo(expected).isDir());
}

void CacheTest::emptyFileReplacement_data()
{
   QTest::addColumn<int>("originalSize");
   QTest::newRow("new-file") << -1;
   QTest::newRow("already-empty") << 0;
   QTest::newRow("replace-nonempty") << 8;
}

void CacheTest::emptyFileReplacement()
{
   QFETCH(int, originalSize);
   QTemporaryDir temp;
   QVERIFY(temp.isValid());
   const auto savedShares = SETTINGS.getRepeated<Protos::Common::SharedEntry>("shared_entries");
   const auto restoreShares = qScopeGuard([&] { SETTINGS.set("shared_entries", savedShares); });
   SETTINGS.rm("shared_entries");
   FM::FileManager manager(QSharedPointer<HC::IHashCache>(new MockHashCache));
   const auto shared = manager.addASharedPath(temp.path() + '/');
   QTRY_COMPARE(manager.getCacheStatus(), FM::IFileManager::UP_TO_DATE);

   Protos::Common::Entry entry;
   entry.set_type(Protos::Common::Entry::FILE);
   entry.set_path("/");
   entry.set_name("emptyreplacement.txt");
   entry.mutable_shared_entry()->mutable_id()->set_hash(shared.first.ID.getData(), Common::Hash::HASH_SIZE);
   QList<QSharedPointer<FM::IChunk>> originalChunks;
   QSharedPointer<FM::IDataReader> retainedReader;
   if (originalSize >= 0)
   {
      entry.set_size(originalSize);
      const QByteArray original(originalSize, 'x');
      if (originalSize > 0)
      {
         Common::Hasher hasher;
         hasher.addData(std::span<const char>(original));
         const auto hash = hasher.getResult();
         entry.add_chunks()->set_hash(hash.getData(), Common::Hash::HASH_SIZE);
      }
      originalChunks = manager.newFile(entry);
      if (!originalChunks.isEmpty())
      {
         auto writer = originalChunks.first()->getDataWriter();
         QVERIFY(writer->write(original.constData(), original.size()));
         retainedReader = originalChunks.first()->getDataReader();
      }
   }
   entry.set_size(0);
   entry.clear_chunks();
   entry.set_exists(false);
   QVERIFY(manager.newFile(entry).isEmpty());
   QVERIFY(entry.exists());

   const QString path = temp.filePath("emptyreplacement.txt");
   QVERIFY(QFileInfo(path).isFile());
   QCOMPARE(QFileInfo(path).size(), qint64(0));
   QVERIFY(!QFileInfo::exists(path + ".unfinished"));
   auto file = dynamic_cast<FM::File*>(manager.getEntry(Common::Path(path)));
   QVERIFY(file);
   if (originalSize > 0)
   {
      // Windows may deliver the old destination's deletion after replacement.
      FM::FileUpdater updater(&manager);
      updater.processEvents({ FM::WatcherEvent(FM::WatcherEvent::DELETED, path, false) });
      QCOMPARE(manager.getEntry(Common::Path(path)), static_cast<FM::Entry*>(file));
      QCOMPARE(updater.entriesToScan, QList<FM::Entry*> { file });
   }
   QVERIFY(file->isComplete());
   QCOMPARE(file->getSize(), qint64(0));
   QCOMPARE(manager.getAmount(), qint64(0));
   QVERIFY(file->getChunks().isEmpty());
   for (const auto& chunk : originalChunks)
   {
      QVERIFY(!chunk->isComplete());
      QVERIFY(chunk->getFilePath().isNull());
      QVERIFY(!manager.getChunk(chunk->getHash()));
   }
   const auto search = [&](const QString& words, const QList<QString>& extensions, qint64 size) {
      QStringList names;
      for (const auto& result : manager.find(words, extensions, size, size,
         Protos::Common::FindPattern::FILE, 100, 65536, true))
         for (const auto& hit : result.entries())
            names << QString::fromStdString(hit.entry().name());
      return names;
   };
   const QStringList expected { "emptyreplacement.txt" };
   QCOMPARE(search("emptyreplacement", {}, 0), expected);
   QCOMPARE(search("", { "txt" }, 0), expected);
   QCOMPARE(search("", {}, 0), expected);
   if (originalSize > 0)
      QVERIFY(search("", {}, originalSize).isEmpty());

   // Repeating the empty download must reuse the completed entry.
   QVERIFY(manager.newFile(entry).isEmpty());
   QCOMPARE(manager.getEntry(Common::Path(path)), static_cast<FM::Entry*>(file));
}

void CacheTest::emptyFileReplacementReportsRenameFailure()
{
#ifdef Q_OS_WIN32
   QTemporaryDir temp;
   QVERIFY(temp.isValid());
   const QString path = temp.filePath("blocked.txt");
   QFile physical(path);
   QVERIFY(physical.open(QIODevice::WriteOnly));
   QCOMPARE(physical.write("original"), qint64(8));
   physical.close();
   FM::Cache cache(QSharedPointer<HC::IHashCache>(new MockHashCache));
   const auto shared = cache.addASharedPath(temp.path() + '/');
   auto root = dynamic_cast<FM::SharedDirectory*>(cache.getSharedEntry(shared.first.ID));
   QVERIFY(root);
   auto file = new FM::File(root, "blocked.txt", 8, false, QFileInfo(path).lastModified(), root->getRootDir());

   // Simulate another application holding the destination open without delete sharing.
   HANDLE handle = CreateFileW(reinterpret_cast<LPCWSTR>(path.utf16()), GENERIC_READ,
      FILE_SHARE_READ | FILE_SHARE_WRITE, nullptr, OPEN_EXISTING, 0, nullptr);
   QVERIFY(handle != INVALID_HANDLE_VALUE);
   const auto closeHandle = qScopeGuard([&] { CloseHandle(handle); });
   Protos::Common::Entry entry;
   entry.set_type(Protos::Common::Entry::FILE);
   entry.set_path("/");
   entry.set_name("blocked.txt");
   entry.set_size(0);
   entry.mutable_shared_entry()->mutable_id()->set_hash(shared.first.ID.getData(), Common::Hash::HASH_SIZE);
   QVERIFY_THROWS_EXCEPTION(FM::UnableToCreateNewFileException, cache.newFile(entry));
   QVERIFY(!entry.exists());
   QVERIFY(!file->isComplete());
   QCOMPARE(file->getName(), QString("blocked.txt.unfinished"));
   QVERIFY(QFileInfo::exists(path + ".unfinished"));
   QVERIFY(physical.open(QIODevice::ReadOnly));
   QCOMPARE(physical.readAll(), QByteArray("original"));
#else
   QSKIP("Windows destination sharing regression");
#endif
}

void CacheTest::metadataReadersAvoidStructuralLocks()
{
   FM::Chunk::CHUNK_SIZE = Common::Constants::CHUNK_SIZE;
   class LockedFile : public FM::File
   {
   public:
      using FM::File::File;
      QRecursiveMutex& structuralMutex() { return this->mutex; }
   };
   QTemporaryDir temp;
   QVERIFY(temp.isValid());
   FM::Cache cache(QSharedPointer<HC::IHashCache>(new MockHashCache));
   const auto shared = cache.addASharedPath(temp.path() + '/');
   auto root = dynamic_cast<FM::SharedDirectory*>(cache.getSharedEntry(shared.first.ID));
   QVERIFY(root);
   auto file = new LockedFile(root, "locked.txt", 8, false, QDateTime::currentDateTime(), root->getRootDir());
   QSemaphore readDone;
   bool correct = false;
   QMutexLocker locker(&file->structuralMutex());
   std::thread reader([&] {
      // Parent lookups and index predicates may read child metadata while the
      // child's structural lock is held by a writer waiting to enter its parent.
      correct = root->getRootDir()->getFile("locked.txt") == file &&
         file->getNameWithoutExtension() == "locked" && file->getExtension() == "txt" &&
         file->getSize() == 8 && cache.getAmount() == 8 && file->getRoot() == root && !file->isRoot();
      readDone.release();
   });
   const bool independent = readDone.tryAcquire(1, 5000);
   locker.unlock();
   reader.join();
   QVERIFY2(independent, "Metadata reads acquired the child's structural mutex");
   QVERIFY(correct);
}

void CacheTest::concurrentEntryMetadata()
{
   FM::Chunk::CHUNK_SIZE = Common::Constants::CHUNK_SIZE;
   QTemporaryDir temp;
   QVERIFY(temp.isValid());
   FM::Cache cache(QSharedPointer<HC::IHashCache>(new MockHashCache));
   const auto shared = cache.addASharedPath(temp.path() + '/');
   auto root = dynamic_cast<FM::SharedDirectory*>(cache.getSharedEntry(shared.first.ID));
   QVERIFY(root);
   const QStringList stems { QString(257, 'a'), QString(513, 'b') };
   const QStringList names { stems[0] + ".txt", stems[1] + ".txt" };
   const QStringList directories { "first", "second" };
   const QStringList aliases { QString(257, 'c'), QString(513, 'd') };
   auto parent = root->getRootDir()->createSubDir(directories[0]);
   auto file = new FM::File(root, names[0], 8, false, QDateTime::currentDateTime(), parent);
   root->setUserName(aliases[0]);
   const Common::Path originalParent = root->getParentPath();
   const Common::Path alternateParent = originalParent.appendDir("alternate");
   const qint64 sizes[] { 0x100000001LL, 0x200000002LL };
   file->setSize(sizes[0]);
   std::atomic<bool> valid { true };
   // Notifications must be emitted after releasing the snapshot locks: consumers
   // immediately read the same metadata to maintain indexes.
   connect(&cache, &FM::Cache::entryRenamed, &cache, [&](FM::Entry* entry, const QString&) {
      entry->getUserName();
      entry->getRoot()->getPath();
   }, Qt::DirectConnection);
   QSemaphore start;
   const auto validSize = [&](qint64 size) { return size == sizes[0] || size == sizes[1]; };
   std::thread writer([&] {
      start.acquire();
      for (int i = 0; i < 2000; ++i)
      {
         file->rename(names[i % 2]);
         file->setSize(sizes[i % 2]);
         parent->rename(directories[i % 2]);
         root->setUserName(aliases[i % 2]);
         if (i % 64 == 0)
            root->setPath((i / 64) % 2 ? originalParent : alternateParent);
      }
   });
   std::thread hiddenWriter([&] {
      start.acquire();
      for (int i = 0; i < 2000; ++i)
         file->setHidden(i % 2);
   });
   std::thread reader([&] {
      start.acquire();
      for (int i = 0; i < 2000; ++i)
      {
         if (!names.contains(file->getName()) || !stems.contains(file->getNameWithoutExtension()) ||
             file->getExtension() != "txt" || !validSize(file->getSize()) ||
             !validSize(cache.getAmount()) || !aliases.contains(root->getUserName()) ||
             file->getRoot() != root || file->isRoot())
            valid = false;
         const auto sharedParent = root->getParentPath();
         if (!(sharedParent == originalParent) && !(sharedParent == alternateParent))
            valid = false;
         const auto path = file->getAbsolutePath();
         if (!names.contains(path.getFilename()) || !directories.contains(path.getLastDir()))
            valid = false;
         // Exercise lookup's parent-to-child reads while rename sorts membership.
         parent->getFile(names[i % 2]);
         Protos::Common::Entry entry;
         file->populateEntry(&entry);
         if (!names.contains(QString::fromStdString(entry.name())) || !validSize(entry.size()))
            valid = false;
      }
   });
   start.release(3);
   writer.join();
   hiddenWriter.join();
   reader.join();
   root->setPath(originalParent);
   QVERIFY(valid.load());
   QCOMPARE(cache.getAmount(), sizes[1]);
   QCOMPARE(parent->getFile(names[1]), file);
   QVERIFY(file->getRoot() == root);
}

void CacheTest::invalidDownloadEntries_data()
{
   QTest::addColumn<bool>("directory");
   QTest::addColumn<QString>("invalid");
   for (bool directory : { false, true })
      for (const QString& invalid : { "type", "parent-traversal", "drive-path", "unc-path", "name-separator",
            "name-backslash", "name-dotdot", "null-path", "chunks", "shared-id" })
         QTest::newRow(qPrintable(QString::number(directory) + ':' + invalid)) << directory << invalid;
   for (const QString& invalid : { "empty-name", "size-overflow", "chunk-count-overflow", "hash-length" })
      QTest::newRow(qPrintable(invalid)) << false << invalid;
}

void CacheTest::invalidDownloadEntries()
{
   QFETCH(bool, directory);
   QFETCH(QString, invalid);
   QTemporaryDir temp;
   QVERIFY(temp.isValid());
   FM::Cache cache(QSharedPointer<HC::IHashCache>(new MockHashCache));
   cache.addASharedPath(temp.path() + '/');
   Protos::Common::Entry entry;
   entry.set_type(directory ? Protos::Common::Entry::DIR : Protos::Common::Entry::FILE);
   entry.set_path("newparent/");
   entry.set_name("child");
   entry.set_size(directory ? 0 : 1);
   if (invalid == "type") entry.set_type(directory ? Protos::Common::Entry::FILE : Protos::Common::Entry::DIR);
   if (invalid == "parent-traversal") entry.set_path("../outside/");
   if (invalid == "drive-path") entry.set_path("C:/outside/");
   if (invalid == "unc-path") entry.set_path("//server/share/");
   if (invalid == "name-separator") entry.set_name("sub/child");
   if (invalid == "name-backslash") entry.set_name("sub\\child");
   if (invalid == "name-dotdot") entry.set_name("..");
   if (invalid == "null-path") entry.set_path(std::string("newparent/\0hidden/", 18));
   if (invalid == "shared-id") entry.mutable_shared_entry()->mutable_id()->set_hash("bad");
   if (invalid == "empty-name") entry.clear_name();
   if (invalid == "size-overflow") entry.set_size(std::numeric_limits<quint64>::max());
   if (invalid == "chunk-count-overflow")
      entry.set_size((quint64(std::numeric_limits<int>::max()) + 1) * Common::Constants::CHUNK_SIZE);
   if (invalid == "chunks")
   {
      entry.add_chunks();
      if (!directory) entry.add_chunks();
   }
   if (invalid == "hash-length") entry.add_chunks()->set_hash("bad");
   const auto original = entry.SerializeAsString();
   if (directory)
      QVERIFY_THROWS_EXCEPTION(FM::UnableToCreateNewDirException, cache.newDirectory(entry));
   else
      QVERIFY_THROWS_EXCEPTION(FM::UnableToCreateNewFileException, cache.newFile(entry));
   QCOMPARE(entry.SerializeAsString(), original);
   QVERIFY(QDir(temp.path()).entryList(QDir::AllEntries | QDir::NoDotAndDotDot | QDir::Hidden).isEmpty());
   QVERIFY(!cache.getEntry(Common::Path(temp.filePath("newparent/"))));
}

void CacheTest::downloadEntryPathsAndHashes_data()
{
   QTest::addColumn<int>("kind");
   QTest::newRow("empty-file") << 0;
   QTest::newRow("unknown-hashes") << 1;
   QTest::newRow("partial-hashes") << 2;
   QTest::newRow("directory") << 3;
}

void CacheTest::downloadEntryPathsAndHashes()
{
   QFETCH(int, kind);
   FM::Chunk::CHUNK_SIZE = Common::Constants::CHUNK_SIZE;
   QTemporaryDir temp;
   QVERIFY(temp.isValid());
   FM::Cache cache(QSharedPointer<HC::IHashCache>(new MockHashCache));
   cache.addASharedPath(temp.path() + '/');
   Protos::Common::Entry entry;
   entry.set_type(kind == 3 ? Protos::Common::Entry::DIR : Protos::Common::Entry::FILE);
   entry.set_path("/parent/deeper"); // Protocol root marker; no trailing slash.
   entry.set_name("child");
   entry.set_size(kind == 1 ? 1 : kind == 2 ? Common::Constants::CHUNK_SIZE + 1 : 0);
   const Common::Hash hash = Common::Hash::rand();
   if (kind == 2) entry.add_chunks()->set_hash(hash.getData(), Common::Hash::HASH_SIZE);
   if (kind == 3)
   {
      cache.newDirectory(entry);
      QVERIFY(QFileInfo(temp.filePath("parent/deeper/child")).isDir());
   }
   else
   {
      if (kind == 1)
      {
         auto emptyEntry = entry;
         emptyEntry.set_size(0);
         QVERIFY(cache.newFile(emptyEntry).isEmpty());
      }
      const auto chunks = cache.newFile(entry);
      QCOMPARE(chunks.size(), kind);
      if (kind == 1) QVERIFY(chunks[0]->getHash().isNull());
      if (kind == 2)
      {
         QCOMPARE(chunks[0]->getHash(), hash);
         QVERIFY(chunks[1]->getHash().isNull());
      }
      const QString path = temp.filePath("parent/deeper/child") + (kind ? ".unfinished" : "");
      QVERIFY(QFileInfo(path).isFile());
      QCOMPARE(QFileInfo(path).size(), qint64(entry.size()));
      QVERIFY(cache.getEntry(Common::Path(path)));
   }
}

void CacheTest::unfinishedDownloadRetry_data()
{
   QTest::addColumn<QString>("retry");
   for (const QString& retry : { "same", "omitted-hashes", "unknown-hash", "different-hash", "different-size", "missing-file", "empty" })
      QTest::newRow(qPrintable(retry)) << retry;
}

void CacheTest::unfinishedDownloadRetry()
{
   QFETCH(QString, retry);
   FM::Chunk::CHUNK_SIZE = Common::Constants::CHUNK_SIZE;
   QTemporaryDir temp;
   QVERIFY(temp.isValid());
   FM::Cache cache(QSharedPointer<HC::IHashCache>(new MockHashCache));
   const auto shared = cache.addASharedPath(temp.path() + '/');
   auto root = dynamic_cast<FM::SharedDirectory*>(cache.getSharedEntry(shared.first.ID));
   QVERIFY(root);
   const QByteArray original("abcdefgh");
   const auto hashOf = [](const QByteArray& data) {
      Common::Hasher hasher;
      hasher.addData(std::span<const char>(data));
      return hasher.getResult();
   };
   Protos::Common::Entry entry;
   entry.set_type(Protos::Common::Entry::FILE);
   entry.set_path("/");
   entry.set_name("retry.txt");
   entry.set_size(original.size());
   entry.mutable_shared_entry()->mutable_id()->set_hash(shared.first.ID.getData(), Common::Hash::HASH_SIZE);
   const auto hash = hashOf(original);
   entry.add_chunks()->set_hash(hash.getData(), Common::Hash::HASH_SIZE);
   const auto oldChunks = cache.newFile(entry);
   auto writer = oldChunks.first()->getDataWriter();
   QVERIFY(!writer->write(original.constData(), 3));
   auto file = root->getRootDir()->getFile("retry.txt.unfinished");
   QVERIFY(file);
   const QString unfinishedPath = temp.filePath("retry.txt.unfinished");
   const bool preserve = retry == "same" || retry == "omitted-hashes" || retry == "unknown-hash";
   QByteArray replacement = original;
   if (retry == "different-hash") replacement = "ijklmnop";
   if (retry == "different-size") replacement = "ijklmnopq";
   if (retry == "empty") replacement.clear();
   if (!preserve)
   {
      entry.set_size(replacement.size());
      entry.clear_chunks();
      if (!replacement.isEmpty())
      {
         const auto hash = hashOf(replacement);
         entry.add_chunks()->set_hash(hash.getData(), Common::Hash::HASH_SIZE);
      }
   }
   if (retry == "omitted-hashes") entry.clear_chunks();
   if (retry == "unknown-hash") entry.mutable_chunks(0)->clear_hash();
   if (retry == "missing-file") file->removeUnfinishedFiles();

   const auto chunks = cache.newFile(entry);
   QCOMPARE(root->getRootDir()->getFiles().size(), 1);
   QCOMPARE(root->getRootDir()->getFiles().first(), file);
   QCOMPARE(cache.getAmount(), qint64(replacement.size()));
   QVERIFY(!QFileInfo::exists(unfinishedPath + ".unfinished"));
   if (preserve)
   {
      QCOMPARE(chunks.first().data(), oldChunks.first().data());
      QCOMPARE(chunks.first()->getKnownBytes(), 3);
      QFile physical(unfinishedPath);
      QVERIFY(physical.open(QIODevice::ReadOnly));
      QCOMPARE(physical.read(3), original.left(3));
      physical.close();
      // The original writer still owns this generation and can finish it.
      QVERIFY(writer->write(original.constData() + 3, original.size() - 3));
   }
   else
   {
      QVERIFY(oldChunks.first()->getFilePath().isNull());
      QVERIFY_THROWS_EXCEPTION(FM::ChunkDeletedException, writer->write("x", 1));
      if (!chunks.isEmpty())
      {
         QVERIFY(chunks.first().data() != oldChunks.first().data());
         QCOMPARE(chunks.first()->getKnownBytes(), 0);
         auto replacementWriter = chunks.first()->getDataWriter();
         QVERIFY(replacementWriter->write(replacement.constData(), replacement.size()));
      }
   }
   QVERIFY(file->isComplete());
   QVERIFY(!QFileInfo::exists(unfinishedPath));
   QFile completed(temp.filePath("retry.txt"));
   QVERIFY(completed.open(QIODevice::ReadOnly));
   QCOMPARE(completed.readAll(), replacement);
}
