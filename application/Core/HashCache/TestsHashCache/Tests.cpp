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

#include <IHashCache.h>
#include <Common/Constants.h>

Tests::Tests()
{
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

   auto first = HC::Builder::newHashCache(firstFolder.path());
   const QList<Common::Hash> firstHashes { Common::Hash(QByteArray(Common::Hash::HASH_SIZE, 'a')) };
   const QList<Common::Hash> secondHashes { Common::Hash(QByteArray(Common::Hash::HASH_SIZE, 'b')) };
   first->setHashes("file", firstHashes, 1);
   auto second = HC::Builder::newHashCache(secondFolder.path());
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
   const auto cache = HC::Builder::newHashCache(folder.path());
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

void Tests::concurrentAccess()
{
   QTest::failOnWarning();
   QTemporaryDir folder;
   QVERIFY(folder.isValid());
   const auto previousConnections = QSqlDatabase::connectionNames();
   const QList<Common::Hash> initial { Common::Hash::rand() };
   const QDateTime date = QDateTime::fromMSecsSinceEpoch(123456789);

   {
      const auto cache = HC::Builder::newHashCache(folder.path());
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
   const auto reopened = HC::Builder::newHashCache(folder.path());
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
      cache = HC::Builder::newHashCache(folder.path());
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
