#include <QTest>
#include <QTemporaryDir>
#include <priv/Cache/FilePool.h>

class FilePoolTests : public QObject
{
   Q_OBJECT
private slots:
   void creationAndReuse()
   {
      QTemporaryDir temp;
      QVERIFY(temp.isValid());
      const QString path = temp.filePath("download.bin");
      FM::FilePool pool;
      bool created = true;
      QVERIFY(!pool.open(path, QIODevice::ReadOnly, &created));
      QVERIFY(!created);
      QVERIFY(!QFile::exists(path));

      const auto mode = QIODevice::ReadWrite | QIODevice::Unbuffered;
      QFile* writer = pool.open(path, mode, &created);
      QVERIFY(writer);
      QVERIFY(created);
      const QByteArray data("keep existing download bytes");
      QCOMPARE(writer->write(data), data.size());
      QVERIFY(writer->flush());

      QFile* reader = pool.open(path, QIODevice::ReadOnly | QIODevice::Unbuffered, &created);
      QVERIFY(reader);
      QVERIFY(!created);
      QCOMPARE(reader->readAll(), data);
      pool.release(reader, true);
      pool.release(writer);
      QFile* reused = pool.open(path, mode, &created);
      QCOMPARE(reused, writer);
      QVERIFY(!created);
      pool.release(reused, true);

      writer = pool.open(path, mode, &created);
      QVERIFY(writer);
      QVERIFY(!created);
      QCOMPARE(writer->readAll(), data);
      pool.release(writer, true);

      QVERIFY(QFile::remove(path));
      writer = pool.open(path, mode, &created);
      QVERIFY(writer);
      QVERIFY(created);
      QCOMPARE(writer->size(), 0);
      pool.release(writer, true);

      created = true;
      QVERIFY(!pool.open(temp.filePath("missing/file.bin"), mode, &created));
      QVERIFY(!created);
   }
};

QTEST_GUILESS_MAIN(FilePoolTests)
#include "FilePoolTests.moc"
