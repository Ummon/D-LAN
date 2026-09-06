#include <CacheTest.h>

#include <QTemporaryDir>
#include <QTest>

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
