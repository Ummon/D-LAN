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
using namespace HC;

#include <atomic>
#include <thread>
#include <vector>

#include <QSemaphore>
#include <QScopeGuard>
#include <QSqlDatabase>
#include <QSqlQuery>
#include <QTemporaryDir>
#include <QFile>
#include <QSqlError>
#include <QUuid>
#include <QElapsedTimer>

#include <IHashCache.h>
#include <priv/HashCache.h>
#include <Common/Constants.h>
#include <Common/Settings.h>
#include <Protos/core_settings.pb.h>
#include <google/protobuf/util/json_util.h>

namespace
{
   QSharedPointer<HC::IHashCache> newTestHashCache(const QString& folder)
   {
      // Exercise maintenance immediately without waiting a minute per cache.
      return QSharedPointer<HC::IHashCache>(new HC::HashCache(folder, 0));
   }

   // An independent connection lets tests inspect committed rows and inject
   // failures without exposing Database or its worker thread in the public API.
   struct TestDatabase
   {
      QSqlDatabase db = QSqlDatabase::addDatabase("QSQLITE", QUuid::createUuid().toString());

      explicit TestDatabase(const QString& folder)
      {
         this->db.setDatabaseName(folder + "/" + Common::Constants::HASH_CACHE_INDEX_FILENAME);
         this->db.open();
      }

      ~TestDatabase()
      {
         const QString name = this->db.connectionName();
         this->db = QSqlDatabase();
         QSqlDatabase::removeDatabase(name);
      }
   };

   bool maintenanceCompleted(const QSqlDatabase& db)
   {
      QSqlQuery query(db);
      return query.exec("SELECT [value] FROM [Settings] WHERE [key] = 'last_check_time'") && query.first();
   }
}

Tests::Tests()
{
}

int Tests::runMaintenanceBatch(HC::HashCache& cache)
{
   int delay = 0;
   QMetaObject::invokeMethod(cache.databaseContext, [&] {
      cache.checkDeletedFileTimer->stop();
      delay = cache.checkFiles();
   }, Qt::BlockingQueuedConnection);
   return delay;
}

void Tests::init()
{
   auto settings = new Protos::Core::Settings;
   // Exercise the largest allowed period, including timer overflow and
   // destruction from another thread, in the existing tests as well.
   settings->set_hashcache_period_verify_files_exist(365 * 86400);
   settings->set_hashcache_nb_of_files_before_check(100000);
   settings->set_hashcache_nb_of_files_deleted_before_vacuum(10000);
   SETTINGS.setSettingsMessage(settings);
}

void Tests::independentConnections_data()
{
   QTest::addColumn<bool>("destroyFirst");
   QTest::newRow("destroy-first-created") << true;
   QTest::newRow("destroy-last-created") << false;
}

void Tests::independentConnections()
{
   QFETCH(bool, destroyFirst);
   QTest::failOnWarning();
   QTemporaryDir firstFolder;
   QTemporaryDir secondFolder;
   QVERIFY(firstFolder.isValid());
   QVERIFY(secondFolder.isValid());

   // A cache must also leave an unrelated default connection untouched.
   QVERIFY(!QSqlDatabase::contains());
   auto defaultDb = QSqlDatabase::addDatabase("QSQLITE");
   const QString defaultName = defaultDb.connectionName();
   const auto cleanupDefault = qScopeGuard([&]
   {
      defaultDb = QSqlDatabase();
      QSqlDatabase::removeDatabase(defaultName);
   });
   defaultDb.setDatabaseName(":memory:");
   QVERIFY(defaultDb.open());
   const auto previousConnections = QSqlDatabase::connectionNames();

   auto first = newTestHashCache(firstFolder.path());
   const QList<Common::Hash> firstHashes { Common::Hash(QByteArray(Common::Hash::HASH_SIZE, 'a')) };
   const QList<Common::Hash> secondHashes { Common::Hash(QByteArray(Common::Hash::HASH_SIZE, 'b')) };
   first->setHashes("file", firstHashes, 1);
   auto second = newTestHashCache(secondFolder.path());
   second->setHashes("file", secondHashes, 1);
   QCOMPARE(QSqlDatabase::connectionNames().size(), previousConnections.size() + 2);
   QCOMPARE(first->getHashes("file", 1), firstHashes);
   QCOMPARE(second->getHashes("file", 1), secondHashes);

   auto& survivor = destroyFirst ? second : first;
   (destroyFirst ? first : second).reset();
   QCOMPARE(QSqlDatabase::connectionNames().size(), previousConnections.size() + 1);
   QCOMPARE(survivor->getHashes("file", 1), destroyFirst ? secondHashes : firstHashes);
   survivor->setHashes("new-file", firstHashes, 1);
   QCOMPARE(survivor->getHashes("new-file", 1), firstHashes);
   survivor->rmHashes("file");
   QVERIFY(survivor->getHashes("file", 1).isEmpty());
   survivor.reset();
   QCOMPARE(QSqlDatabase::connectionNames(), previousConnections);

   QSqlQuery query(defaultDb);
   QVERIFY(query.exec("SELECT 42"));
   QVERIFY(query.next());
   QCOMPARE(query.value(0).toInt(), 42);
}

void Tests::lookupRequiresMatchingSize()
{
   QTest::failOnWarning();
   QTemporaryDir folder;
   QVERIFY(folder.isValid());
   const auto cache = newTestHashCache(folder.path());
   const QList<Common::Hash> hashes { Common::Hash::rand() };
   const QDateTime date = QDateTime::fromMSecsSinceEpoch(123456789);
   cache->setHashes("file", hashes, 2, date);

   QCOMPARE(cache->getHashes("file", 2, date), hashes);
   QCOMPARE(cache->getHashes("file", 2), hashes);
   // Both shorter and longer files can have the same chunk count and timestamp.
   for (qint64 size : { qint64(0), qint64(1), qint64(3), qint64(Common::Constants::CHUNK_SIZE) + 1 })
   {
      QVERIFY(cache->getHashes("file", size, date).isEmpty());
      QVERIFY(cache->getHashes("file", size).isEmpty());
   }
   QVERIFY(cache->getHashes("file", 2, date.addMSecs(1)).isEmpty());
   QVERIFY(cache->getHashes("missing", 2, date).isEmpty());
   // A miss must not discard the existing entry.
   QCOMPARE(cache->getHashes("file", 2, date), hashes);

   const QList<Common::Hash> replacement { Common::Hash::rand() };
   cache->setHashes("file", replacement, 3, date);
   QVERIFY(cache->getHashes("file", 2, date).isEmpty());
   QCOMPARE(cache->getHashes("file", 3, date), replacement);
}

void Tests::batchLookupPreservesOrderAndMetadata()
{
   QTest::failOnWarning();
   QTemporaryDir folder;
   QVERIFY(folder.isValid());
   const auto cache = newTestHashCache(folder.path());
   const QList<Common::Hash> first { Common::Hash::rand() };
   const QList<Common::Hash> second { Common::Hash::rand(), Common::Hash() };
   const QDateTime date = QDateTime::fromMSecsSinceEpoch(123456789);
   const qint64 secondSize = qint64(Common::Constants::CHUNK_SIZE) + 1;
   cache->setHashes("first", first, 2, date);
   cache->setHashes("second", second, secondSize, date);
   const QList<IHashCache::FileMetadata> requests {
      { "second", secondSize, date }, { "missing", 2, date },
      { "first", 2, date.addMSecs(1) }, { "first", 3, date },
      { "first", 2, {} }, { "second", secondSize, date }, { "first", 2, date }
   };
   const QList<QList<Common::Hash>> expected { second, {}, {}, {}, first, second, first };
   QCOMPARE(cache->getHashesBatch(requests), expected);
   QVERIFY(cache->getHashesBatch({}).isEmpty());
   for (int i = 0; i < requests.size(); ++i)
      QCOMPARE(cache->getHashes(requests[i].path, requests[i].size, requests[i].timeLastModified), expected[i]);
}

void Tests::batchedWritesPreserveOrderingAndShutdown()
{
   QTest::failOnWarning();
   QTemporaryDir folder;
   QVERIFY(folder.isValid());
   const QList<Common::Hash> first { Common::Hash::rand() };
   const QList<Common::Hash> second { Common::Hash::rand() };
   {
      auto cache = newTestHashCache(folder.path());
      cache->setHashes("replace", first, 1);
      cache->setHashes("replace", second, 1);
      QCOMPARE(cache->getHashes("replace", 1), second);
      cache->setHashes("remove", first, 1);
      cache->rmHashes("remove");
      QVERIFY(cache->getHashesBatch({ { "remove", 1, {} } }).first().isEmpty());
      cache->setHashes("replace", first, 1);
      cache->rmHashes("replace");
      cache->setHashes("replace", second, 1);
      // Exceed the row limit and leave a final partial batch for destruction.
      for (int i = 0; i < 513; ++i)
         cache->setHashes(QString("file-%1").arg(i), first, 1);
   }
   const auto reopened = newTestHashCache(folder.path());
   QCOMPARE(reopened->getHashes("replace", 1), second);
   QVERIFY(reopened->getHashes("remove", 1).isEmpty());
   QList<IHashCache::FileMetadata> requests;
   for (int i = 0; i < 513; ++i)
      requests.append({ QString("file-%1").arg(i), 1, {} });
   const auto results = reopened->getHashesBatch(requests);
   QCOMPARE(results.size(), requests.size());
   for (const auto& hashes : results)
      QCOMPARE(hashes, first);
}

void Tests::pendingWritesFlushWithoutRead()
{
   QTest::failOnWarning();
   QTemporaryDir folder;
   QVERIFY(folder.isValid());
   const auto cache = newTestHashCache(folder.path());
   TestDatabase inspector(folder.path());
   const QList<Common::Hash> hashes { Common::Hash::rand() };
   cache->setHashes("file", hashes, 1);
   QElapsedTimer deadline;
   deadline.start();
   bool committed = false;
   while (!committed && deadline.elapsed() < 3000)
   {
      QSqlQuery query(inspector.db);
      QVERIFY(query.exec("SELECT COUNT(*) FROM [File] WHERE [path] = 'file'"));
      QVERIFY(query.first());
      committed = query.value(0).toInt() == 1;
      query.finish();
      if (!committed)
         QThread::msleep(10); // Deliberately do not run the caller's event loop.
   }
   QVERIFY(committed);
}

void Tests::failedWriteBatchKeepsOtherUpdates()
{
   QTest::failOnWarning();
   QTemporaryDir folder;
   QVERIFY(folder.isValid());
   const auto cache = newTestHashCache(folder.path());
   const QList<Common::Hash> original { Common::Hash::rand() };
   const QList<Common::Hash> replacement { Common::Hash::rand() };
   cache->setHashes("bad", original, 1);
   QCOMPARE(cache->getHashes("bad", 1), original);
   TestDatabase inspector(folder.path());
   QSqlQuery query(inspector.db);
   QVERIFY(query.exec("CREATE TRIGGER fail_write BEFORE INSERT ON [File] WHEN NEW.path = 'bad' BEGIN SELECT RAISE(ABORT, 'test failure'); END"));
   query.finish();
   cache->setHashes("before", replacement, 1);
   cache->setHashes("bad", replacement, 1);
   cache->setHashes("after", replacement, 1);
   const QList<QList<Common::Hash>> expected { replacement, original, replacement };
   QCOMPARE(cache->getHashesBatch({ { "before", 1, {} }, { "bad", 1, {} }, { "after", 1, {} } }), expected);
   QVERIFY(query.exec("DROP TRIGGER fail_write"));
   query.finish();
   cache->setHashes("bad", replacement, 1);
   QCOMPARE(cache->getHashes("bad", 1), replacement);
}

void Tests::concurrentAccess()
{
   QTest::failOnWarning();
   QTemporaryDir folder;
   QVERIFY(folder.isValid());
   const auto previousConnections = QSqlDatabase::connectionNames();
   const QList<Common::Hash> initial { Common::Hash::rand() };
   const QDateTime date = QDateTime::fromMSecsSinceEpoch(123456789);

   {
      const auto cache = newTestHashCache(folder.path());
      cache->setHashes("shared", initial, 1, date);

      QSemaphore start;
      std::atomic<bool> success { true };
      std::vector<std::thread> callers;
      for (int worker = 0; worker < 4; ++worker)
      {
         callers.emplace_back([&, worker]
         {
            start.acquire();
            for (int i = 0; i < 25; ++i)
            {
               const QString path = QString("%1/%2").arg(worker).arg(i);
               const QList<Common::Hash> hashes { Common::Hash(QByteArray(Common::Hash::HASH_SIZE, char(worker + i + 1))) };
               if (cache->getHashes("shared", 1, date) != initial)
                  success = false;
               cache->setHashes(path, hashes, 1, date);
               if (cache->getHashes(path, 1, date) != hashes || cache->getHashes(path, 1) != hashes ||
                   !cache->getHashes(path, 1, date.addMSecs(1)).isEmpty())
                  success = false;
               cache->rmHashes(path);
               if (!cache->getHashes(path, 1).isEmpty())
                  success = false;
            }
         });
      }
      start.release(4);
      for (auto& caller : callers)
         caller.join();
      QVERIFY(success.load());
      QCOMPARE(cache->getHashes("shared", 1, date), initial);
   }
   QCOMPARE(QSqlDatabase::connectionNames(), previousConnections);

   // Committed data remains visible after the worker and connection are recreated.
   const auto reopened = newTestHashCache(folder.path());
   QCOMPARE(reopened->getHashes("shared", 1, date), initial);
}

void Tests::destructionFromAnotherThread()
{
   QTest::failOnWarning();
   QTemporaryDir folder;
   QVERIFY(folder.isValid());
   const auto previousConnections = QSqlDatabase::connectionNames();
   QSharedPointer<HC::IHashCache> cache;
   QSemaphore created;
   QSemaphore destroyed;
   std::thread creator([&]
   {
      cache = newTestHashCache(folder.path());
      created.release();
      destroyed.acquire();
   });
   created.acquire();
   const QList<Common::Hash> hashes { Common::Hash::rand() };
   cache->setHashes("file", hashes, 1);
   const auto retrieved = cache->getHashes("file", 1);
   cache.reset();
   destroyed.release();
   creator.join();

   QCOMPARE(retrieved, hashes);
   QCOMPARE(QSqlDatabase::connectionNames(), previousConnections);
}

void Tests::cleanupMissingFiles_data()
{
   QTest::addColumn<quint32>("minFiles");
   QTest::addColumn<quint32>("minDeleted");
   QTest::addColumn<int>("expectedRows");
   QTest::addColumn<quint64>("expectedDeleted");
   QTest::newRow("below-scan-threshold") << quint32(4) << quint32(3) << 3 << quint64(0);
   QTest::newRow("at-scan-threshold") << quint32(3) << quint32(3) << 3 << quint64(0);
   QTest::newRow("at-vacuum-threshold") << quint32(2) << quint32(2) << 1 << quint64(2);
   QTest::newRow("above-vacuum-threshold") << quint32(2) << quint32(1) << 1 << quint64(0);
   QTest::newRow("zero-thresholds") << quint32(0) << quint32(0) << 1 << quint64(0);
}

void Tests::cleanupMissingFiles()
{
   QFETCH(quint32, minFiles);
   QFETCH(quint32, minDeleted);
   QFETCH(int, expectedRows);
   QFETCH(quint64, expectedDeleted);
   QTest::failOnWarning();
   QTemporaryDir folder;
   QVERIFY(folder.isValid());
   const QString present = folder.filePath("present");
   QFile file(present);
   QVERIFY(file.open(QIODevice::WriteOnly));
   file.close();
   const QList<Common::Hash> hashes { Common::Hash::rand() };
   {
      auto cache = newTestHashCache(folder.path());
      cache->setHashes(present, hashes, 1);
      cache->setHashes(folder.filePath("missing-a"), hashes, 1);
      cache->setHashes(folder.filePath("missing-b"), hashes, 1);
      // Destruction must flush queued writes without needing a read first.
   }
   TestDatabase inspector(folder.path());
   QSqlQuery query(inspector.db);
   // Simulate a database produced by the uncommitted version-2 schema.
   QVERIFY(query.exec("DROP TABLE [Settings]"));
   QVERIFY(query.exec("CREATE TABLE [Settings] ([key] TEXT PRIMARY KEY NOT NULL, [value] BLOB) STRICT"));
   QVERIFY(query.exec("INSERT INTO [Settings] VALUES ('preserved', X'0102')"));
   QVERIFY(query.exec("DELETE FROM [Version] WHERE [version] = 3"));
   query.finish();
   SETTINGS.set("hashcache_nb_of_files_before_check", minFiles);
   SETTINGS.set("hashcache_nb_of_files_deleted_before_vacuum", minDeleted);
   {
      auto cache = newTestHashCache(folder.path());
      QCOMPARE(cache->getHashes(present, 1), hashes);
      QTRY_VERIFY_WITH_TIMEOUT(maintenanceCompleted(inspector.db), 5000);
   }
   QVERIFY(query.exec("SELECT COUNT(*) FROM [File]"));
   QVERIFY(query.first());
   QCOMPARE(query.value(0).toInt(), expectedRows);
   QVERIFY(query.exec("SELECT [value] FROM [Settings] WHERE [key] = 'nb_deleted_files'"));
   QVERIFY(query.first());
   QCOMPARE(query.value(0).toULongLong(), expectedDeleted);
   QVERIFY(query.exec("SELECT [value] FROM [Settings] WHERE [key] = 'last_check_time'"));
   QVERIFY(query.first());
   QVERIFY(query.value(0).toLongLong() > 0);
   QVERIFY(query.exec("SELECT [value] FROM [Settings] WHERE [key] = 'preserved'"));
   QVERIFY(query.first());
   QCOMPARE(query.value(0).toByteArray(), QByteArray::fromHex("0102"));
   query.finish();

   // A recent persisted check prevents another scan on restart.
   QVERIFY(file.remove());
   {
      auto cache = newTestHashCache(folder.path());
      QCOMPARE(cache->getHashes(present, 1), hashes);
   }

   if (expectedDeleted > 0)
   {
      // A later removal exceeds the vacuum threshold even though File is now
      // below the scan threshold. Removing an unknown path must not count.
      {
         auto cache = newTestHashCache(folder.path());
         cache->rmHashes(present);
         cache->rmHashes(present);
      }
      QVERIFY(query.exec("SELECT [value] FROM [Settings] WHERE [key] = 'nb_deleted_files'"));
      QVERIFY(query.first());
      QCOMPARE(query.value(0).toULongLong(), quint64(3));
      QVERIFY(query.exec("DELETE FROM [Settings] WHERE [key] = 'last_check_time'"));
      query.finish();
      {
         auto cache = newTestHashCache(folder.path());
         QVERIFY(cache->getHashes(present, 1).isEmpty());
         QTRY_VERIFY_WITH_TIMEOUT(maintenanceCompleted(inspector.db), 5000);
      }
      QVERIFY(query.exec("SELECT [value] FROM [Settings] WHERE [key] = 'nb_deleted_files'"));
      QVERIFY(query.first());
      QCOMPARE(query.value(0).toULongLong(), quint64(0));
   }
}

void Tests::cleanupRollsBackOnFailure()
{
   QTest::failOnWarning();
   QTemporaryDir folder;
   QVERIFY(folder.isValid());
   const QList<Common::Hash> hashes { Common::Hash::rand() };
   const QString missing = folder.filePath("missing");
   {
      auto cache = newTestHashCache(folder.path());
      cache->setHashes(missing, hashes, 1);
   }
   TestDatabase inspector(folder.path());
   QSqlQuery query(inspector.db);
   QVERIFY(query.exec("DELETE FROM [Settings]"));
   // Fail after deletion, while persisting the counter, to verify atomicity.
   QVERIFY(query.exec("CREATE TRIGGER fail_settings BEFORE INSERT ON [Settings] BEGIN SELECT RAISE(ABORT, 'test failure'); END"));
   query.finish();
   SETTINGS.set("hashcache_nb_of_files_before_check", quint32(0));
   {
      auto cache = newTestHashCache(folder.path());
      QCOMPARE(cache->getHashes(missing, 1), hashes);
      cache->rmHashes(missing);
      QCOMPARE(cache->getHashes(missing, 1), hashes);
   }
   QVERIFY(query.exec("SELECT COUNT(*) FROM [Settings]"));
   QVERIFY(query.first());
   QCOMPARE(query.value(0).toInt(), 0);
   QVERIFY(query.exec("DROP TRIGGER fail_settings"));
   query.finish();
   {
      auto cache = newTestHashCache(folder.path());
      QVERIFY(cache->getHashes(missing, 1).isEmpty());
   }
}

void Tests::maintenanceYieldsBetweenBatches()
{
   QTest::failOnWarning();
   QTemporaryDir folder;
   QVERIFY(folder.isValid());
   SETTINGS.set("hashcache_nb_of_files_before_check", quint32(0));
   HC::HashCache cache(folder.path(), 60000); // Advance one batch at a time below.
   const QList<Common::Hash> hashes { Common::Hash::rand() };
   constexpr int count = 1025;
   for (int i = 0; i < count; ++i)
      cache.setHashes(folder.filePath(QString("missing-%1").arg(i)), hashes, 1);
   const QString lastPath = folder.filePath("missing-1024");
   QCOMPARE(cache.getHashes(lastPath, 1), hashes);
   TestDatabase inspector(folder.path());

   QCOMPARE(this->runMaintenanceBatch(cache), 1);
   QSqlQuery query(inspector.db);
   QVERIFY(query.exec("SELECT COUNT(*) FROM [File]"));
   QVERIFY(query.first());
   const int remaining = query.value(0).toInt();
   QVERIFY(remaining >= count - 128 && remaining < count);
   query.finish();
   QVERIFY(!maintenanceCompleted(inspector.db));

   // Normal operations must be serviced before the rest of the sweep.
   QCOMPARE(cache.getHashes(lastPath, 1), hashes);
   QFile restored(lastPath);
   QVERIFY(restored.open(QIODevice::WriteOnly));
   QCOMPARE(restored.write("x", 1), qint64(1));
   restored.close();
   const QList<Common::Hash> replacement { Common::Hash::rand() };
   cache.setHashes(lastPath, replacement, 1);
   cache.rmHashes(folder.filePath("missing-1000"));
   // A newly appended row is outside this sweep's fixed upper ID bound.
   const QString added = folder.filePath("added-during-sweep");
   cache.setHashes(added, hashes, 1);
   QCOMPARE(cache.getHashes(added, 1), hashes);

   int delay = 1;
   for (int batch = 0; delay == 1 && batch < count; ++batch)
      delay = this->runMaintenanceBatch(cache);
   QVERIFY(delay > 1);
   QVERIFY(maintenanceCompleted(inspector.db));
   QCOMPARE(cache.getHashes(lastPath, 1), replacement);
   QCOMPARE(cache.getHashes(added, 1), hashes);
   QVERIFY(query.exec("SELECT COUNT(*) FROM [File]"));
   QVERIFY(query.first());
   QCOMPARE(query.value(0).toInt(), 2);
   QVERIFY(query.exec("SELECT [value] FROM [Settings] WHERE [key] = 'nb_deleted_files'"));
   QVERIFY(query.first());
   QCOMPARE(query.value(0).toInt(), count - 1);
}

void Tests::maintenanceWithPresentFiles()
{
   QTest::failOnWarning();
   QTemporaryDir folder;
   QVERIFY(folder.isValid());
   SETTINGS.set("hashcache_nb_of_files_before_check", quint32(0));
   HC::HashCache cache(folder.path(), 60000);
   const QList<Common::Hash> hashes { Common::Hash::rand() };
   for (int i = 0; i < 257; ++i)
   {
      const QString path = folder.filePath(QString("present-%1").arg(i));
      QFile file(path);
      QVERIFY(file.open(QIODevice::WriteOnly));
      QCOMPARE(file.write("x", 1), qint64(1));
      cache.setHashes(path, hashes, 1);
   }
   TestDatabase inspector(folder.path());
   QSqlQuery query(inspector.db);
   // A batch with no removals must advance without rewriting the deletion counter.
   QVERIFY(query.exec("CREATE TRIGGER no_settings_write BEFORE INSERT ON [Settings] BEGIN SELECT RAISE(ABORT, 'unexpected write'); END"));
   query.finish();
   QCOMPARE(this->runMaintenanceBatch(cache), 1);
   QVERIFY(!maintenanceCompleted(inspector.db));
   QCOMPARE(cache.getHashes(folder.filePath("present-256"), 1), hashes);
   QVERIFY(query.exec("DROP TRIGGER no_settings_write"));
   query.finish();
   int delay = 1;
   for (int batch = 0; delay == 1 && batch < 257; ++batch)
      delay = this->runMaintenanceBatch(cache);
   QVERIFY(delay > 1);
   QVERIFY(maintenanceCompleted(inspector.db));
   QVERIFY(query.exec("SELECT COUNT(*) FROM [File]"));
   QVERIFY(query.first());
   QCOMPARE(query.value(0).toInt(), 257);
   QVERIFY(query.exec("SELECT [value] FROM [Settings] WHERE [key] = 'nb_deleted_files'"));
   QVERIFY(query.first());
   QCOMPARE(query.value(0).toInt(), 0);
}

void Tests::interruptedMaintenanceRestarts()
{
   QTest::failOnWarning();
   QTemporaryDir folder;
   QVERIFY(folder.isValid());
   SETTINGS.set("hashcache_nb_of_files_before_check", quint32(0));
   const QList<Common::Hash> hashes { Common::Hash::rand() };
   {
      HC::HashCache cache(folder.path(), 60000);
      for (int i = 0; i < 257; ++i)
         cache.setHashes(folder.filePath(QString("missing-%1").arg(i)), hashes, 1);
      QCOMPARE(this->runMaintenanceBatch(cache), 1);
      // Destruction must not drain the remaining maintenance batches.
   }
   TestDatabase inspector(folder.path());
   QVERIFY(!maintenanceCompleted(inspector.db));
   QSqlQuery query(inspector.db);
   QVERIFY(query.exec("SELECT COUNT(*) FROM [File]"));
   QVERIFY(query.first());
   QVERIFY(query.value(0).toInt() >= 257 - 128);
   query.finish();
   {
      auto cache = newTestHashCache(folder.path());
      QTRY_VERIFY_WITH_TIMEOUT(maintenanceCompleted(inspector.db), 5000);
      QVERIFY(cache->getHashes(folder.filePath("missing-256"), 1).isEmpty());
   }
   QVERIFY(query.exec("SELECT COUNT(*) FROM [File]"));
   QVERIFY(query.first());
   QCOMPARE(query.value(0).toInt(), 0);
   QVERIFY(query.exec("SELECT [value] FROM [Settings] WHERE [key] = 'nb_deleted_files'"));
   QVERIFY(query.first());
   QCOMPARE(query.value(0).toInt(), 257);
}

void Tests::maintenanceBatchRollsBack()
{
   QTest::failOnWarning();
   QTemporaryDir folder;
   QVERIFY(folder.isValid());
   SETTINGS.set("hashcache_nb_of_files_before_check", quint32(0));
   HC::HashCache cache(folder.path(), 60000);
   const QList<Common::Hash> hashes { Common::Hash::rand() };
   for (int i = 0; i < 257; ++i)
      cache.setHashes(folder.filePath(QString("missing-%1").arg(i)), hashes, 1);
   QCOMPARE(this->runMaintenanceBatch(cache), 1);
   TestDatabase inspector(folder.path());
   QSqlQuery query(inspector.db);
   QVERIFY(query.exec("SELECT COUNT(*) FROM [File]"));
   QVERIFY(query.first());
   const int remaining = query.value(0).toInt();
   query.finish();
   QVERIFY(query.exec("CREATE TRIGGER fail_batch BEFORE INSERT ON [Settings] BEGIN SELECT RAISE(ABORT, 'test failure'); END"));
   query.finish();
   QVERIFY(this->runMaintenanceBatch(cache) > 1);
   QVERIFY(!maintenanceCompleted(inspector.db));
   QVERIFY(query.exec("SELECT COUNT(*) FROM [File]"));
   QVERIFY(query.first());
   QCOMPARE(query.value(0).toInt(), remaining);
   QVERIFY(query.exec("SELECT [value] FROM [Settings] WHERE [key] = 'nb_deleted_files'"));
   QVERIFY(query.first());
   QCOMPARE(query.value(0).toInt(), 257 - remaining);
   query.finish();
   QVERIFY(query.exec("DROP TRIGGER fail_batch"));
   query.finish();
   int delay = 1;
   for (int batch = 0; delay == 1 && batch < 257; ++batch)
      delay = this->runMaintenanceBatch(cache);
   QVERIFY(delay > 1);
   QVERIFY(maintenanceCompleted(inspector.db));
   QVERIFY(query.exec("SELECT [value] FROM [Settings] WHERE [key] = 'nb_deleted_files'"));
   QVERIFY(query.first());
   QCOMPARE(query.value(0).toInt(), 257);
}

void Tests::automaticMultiBatchMaintenance()
{
   QTest::failOnWarning();
   QTemporaryDir folder;
   QVERIFY(folder.isValid());
   SETTINGS.set("hashcache_nb_of_files_before_check", quint32(0));
   const QList<Common::Hash> hashes { Common::Hash::rand() };
   {
      HC::HashCache cache(folder.path(), 60000);
      for (int i = 0; i < 513; ++i)
         cache.setHashes(folder.filePath(QString("missing-%1").arg(i)), hashes, 1);
   }
   TestDatabase inspector(folder.path());
   const auto cache = newTestHashCache(folder.path());
   QTRY_VERIFY_WITH_TIMEOUT(maintenanceCompleted(inspector.db), 5000);
   QVERIFY(cache->getHashes(folder.filePath("missing-512"), 1).isEmpty());
   QSqlQuery query(inspector.db);
   QVERIFY(query.exec("SELECT COUNT(*) FROM [File]"));
   QVERIFY(query.first());
   QCOMPARE(query.value(0).toInt(), 0);
}

void Tests::vacuumCompactsDatabase()
{
   QTest::failOnWarning();
   QTemporaryDir folder;
   QVERIFY(folder.isValid());
   {
      auto cache = newTestHashCache(folder.path());
   }
   TestDatabase inspector(folder.path());
   QSqlQuery query(inspector.db);
   QVERIFY(query.exec("DELETE FROM [Settings] WHERE [key] = 'last_check_time'"));
   QVERIFY(query.prepare("INSERT INTO [File] ([path], [size], [date_last_modified], [hashes]) VALUES (?, 1, 0, ?)"));
   for (int i = 0; i < 100; ++i)
   {
      query.bindValue(0, folder.filePath(QString("missing-%1").arg(i)));
      query.bindValue(1, QByteArray(8192, 'a'));
      QVERIFY(query.exec());
   }
   QVERIFY(query.exec("PRAGMA wal_checkpoint(TRUNCATE)"));
   query.finish();
   const QString databasePath = folder.filePath(Common::Constants::HASH_CACHE_INDEX_FILENAME);
   const qint64 bytesBefore = QFile(databasePath).size();
   QVERIFY(query.exec("PRAGMA page_count"));
   QVERIFY(query.first());
   const int pagesBefore = query.value(0).toInt();
   query.finish();
   SETTINGS.set("hashcache_nb_of_files_before_check", quint32(0));
   SETTINGS.set("hashcache_nb_of_files_deleted_before_vacuum", quint32(1));
   auto cache = newTestHashCache(folder.path());
   QTRY_VERIFY_WITH_TIMEOUT(maintenanceCompleted(inspector.db), 5000);
   QVERIFY(cache->getHashes("barrier", 1).isEmpty());
   // Verify physical shrinking without closing the cache connection.
   QVERIFY(QFile(databasePath).size() < bytesBefore);
   QCOMPARE(QFile(databasePath + "-wal").size(), qint64(0));
   QVERIFY(query.exec("PRAGMA page_count"));
   QVERIFY(query.first());
   QVERIFY(query.value(0).toInt() < pagesBefore);
   QVERIFY(query.exec("SELECT COUNT(*) FROM [File]"));
   QVERIFY(query.first());
   QCOMPARE(query.value(0).toInt(), 0);
   QVERIFY(query.exec("SELECT [value] FROM [Settings] WHERE [key] = 'nb_deleted_files'"));
   QVERIFY(query.first());
   QCOMPARE(query.value(0).toULongLong(), quint64(0));
}

void Tests::periodicCleanupWithoutCallerEventLoop()
{
   QTest::failOnWarning();
   QTemporaryDir folder;
   QVERIFY(folder.isValid());
   SETTINGS.set("hashcache_period_verify_files_exist", quint32(1));
   SETTINGS.set("hashcache_nb_of_files_before_check", quint32(0));
   QSharedPointer<HC::IHashCache> cache;
   std::thread creator([&] { cache = newTestHashCache(folder.path()); });
   creator.join();
   const QString missing = folder.filePath("missing");
   const QList<Common::Hash> hashes { Common::Hash::rand() };
   cache->setHashes(missing, hashes, 1);
   QCOMPARE(cache->getHashes(missing, 1), hashes);
   QTRY_VERIFY_WITH_TIMEOUT(cache->getHashes(missing, 1).isEmpty(), 5000);
   cache.reset();
}

void Tests::restartKeepsMaintenanceDeadline()
{
   QTest::failOnWarning();
   QTemporaryDir folder;
   QVERIFY(folder.isValid());
   const QString missing = folder.filePath("missing");
   const QList<Common::Hash> hashes { Common::Hash::rand() };
   {
      auto cache = newTestHashCache(folder.path());
      cache->setHashes(missing, hashes, 1);
   }
   TestDatabase inspector(folder.path());
   QSqlQuery query(inspector.db);
   QVERIFY(query.prepare("UPDATE [Settings] SET [value] = ? WHERE [key] = 'last_check_time'"));
   query.bindValue(0, QDateTime::currentDateTimeUtc().addSecs(-8).toMSecsSinceEpoch());
   QVERIFY(query.exec());
   query.finish();
   SETTINGS.set("hashcache_period_verify_files_exist", quint32(10));
   SETTINGS.set("hashcache_nb_of_files_before_check", quint32(0));
   auto cache = newTestHashCache(folder.path());
   QCOMPARE(cache->getHashes(missing, 1), hashes);
   // Only two seconds remain; restarting must not postpone the check by ten.
   QTRY_VERIFY_WITH_TIMEOUT(cache->getHashes(missing, 1).isEmpty(), 5000);
}

void Tests::defaultsSurviveOlderSettings()
{
   Protos::Core::Settings settings;
   settings.set_hashcache_period_verify_files_exist(86400);
   settings.set_hashcache_nb_of_files_before_check(100000);
   settings.set_hashcache_nb_of_files_deleted_before_vacuum(10000);
   // The same JSON loader is used by PersistentData for existing installations.
   QVERIFY(google::protobuf::util::JsonStringToMessage("{}", &settings).ok());
   QCOMPARE(settings.hashcache_period_verify_files_exist(), quint32(86400));
   QCOMPARE(settings.hashcache_nb_of_files_before_check(), quint32(100000));
   QCOMPARE(settings.hashcache_nb_of_files_deleted_before_vacuum(), quint32(10000));
}

void Tests::firstCheckIsDelayed()
{
   QTest::failOnWarning();
   QTemporaryDir folder;
   QVERIFY(folder.isValid());
   SETTINGS.set("hashcache_nb_of_files_before_check", quint32(0));
   const QString missing = folder.filePath("missing");
   const QList<Common::Hash> hashes { Common::Hash::rand() };
   {
      auto cache = HC::Builder::newHashCache(folder.path());
      cache->setHashes(missing, hashes, 1);
   }
   TestDatabase inspector(folder.path());
   QSqlQuery query(inspector.db);
   {
      // The public builder uses the one-minute delay, even for an overdue check.
      auto cache = HC::Builder::newHashCache(folder.path());
      QCOMPARE(cache->getHashes(missing, 1), hashes);
      QVERIFY(query.exec("SELECT COUNT(*) FROM [Settings] WHERE [key] = 'last_check_time'"));
      QVERIFY(query.first());
      QCOMPARE(query.value(0).toInt(), 0);
      query.finish();
   }

   // Use a shorter delay to verify the timer fires without slowing the suite.
   constexpr int testDelay = 250;
   QElapsedTimer elapsed;
   elapsed.start();
   auto cache = QSharedPointer<HC::IHashCache>(new HC::HashCache(folder.path(), testDelay));
   QCOMPARE(cache->getHashes(missing, 1), hashes);
   QTRY_VERIFY_WITH_TIMEOUT(cache->getHashes(missing, 1).isEmpty(), 5000);
   QVERIFY(elapsed.elapsed() >= testDelay);
}
