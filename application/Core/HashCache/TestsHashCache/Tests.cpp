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
#include <QSqlDatabase>
#include <QTemporaryDir>

#include <IHashCache.h>

Tests::Tests()
{
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
               if (cache->getHashes("shared", date) != initial)
                  success = false;
               cache->setHashes(path, hashes, 1, date);
               if (cache->getHashes(path, date) != hashes || cache->getHashes(path) != hashes ||
                   !cache->getHashes(path, date.addMSecs(1)).isEmpty())
                  success = false;
               cache->rmHashes(path);
               if (!cache->getHashes(path).isEmpty())
                  success = false;
            }
         });
      }
      start.release(4);
      for (auto& caller : callers)
         caller.join();
      QVERIFY(success.load());
      QCOMPARE(cache->getHashes("shared", date), initial);
   }
   QCOMPARE(QSqlDatabase::connectionNames(), previousConnections);

   // Committed data remains visible after the worker and connection are recreated.
   const auto reopened = HC::Builder::newHashCache(folder.path());
   QCOMPARE(reopened->getHashes("shared", date), initial);
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
   const auto retrieved = cache->getHashes("file");
   cache.reset();
   destroyed.release();
   creator.join();

   QCOMPARE(retrieved, hashes);
   QCOMPARE(QSqlDatabase::connectionNames(), previousConnections);
}
