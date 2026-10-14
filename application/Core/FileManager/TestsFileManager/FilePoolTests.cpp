#include <QTest>
#include <QTemporaryDir>
#include <QScopeGuard>
#include <QTimeZone>
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
      QVERIFY(reused);
      QVERIFY(!created);
      QVERIFY(reused->seek(0));
      QCOMPARE(reused->readAll(), data);
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

   /**
     * The size and the date of an open file are the ones a 'QFileInfo' gives, including after a change made through another
     * handle while the file stays open.
     */
   void metadataOfAnOpenFile()
   {
      QTemporaryDir temp;
      QVERIFY(temp.isValid());
      const QString path = temp.filePath("file.bin");
      {
         QFile physical(path);
         QVERIFY(physical.open(QIODevice::WriteOnly));
         QCOMPARE(physical.write("content"), qint64(7));
      }
      FM::FilePool pool;
      QFile* file = pool.open(path, QIODevice::ReadOnly | QIODevice::Unbuffered);
      QVERIFY(file);
      const auto release = qScopeGuard([&] { pool.release(file, true); });

      const auto compare = [&] {
         const auto metadata = FM::FilePool::getMetadata(*file);
         QVERIFY(metadata);
         const QFileInfo info(path);
         QCOMPARE(metadata->size, info.size());
         QCOMPARE(metadata->lastModified, info.lastModified());
      };
      compare();

      // With milliseconds, and before 1970 for the conversion of the time.
      // The date before 1970 has no milliseconds: on Unix, 'QFile::setFileTime(..)' truncates toward zero and gives
      // a negative 'tv_nsec' to 'futimens(..)', which fails with EINVAL.
      for (const QDateTime& date : {
         QDateTime(QDate(2020, 5, 17), QTime(13, 14, 15, 678), QTimeZone::UTC),
         QDateTime(QDate(1969, 7, 20), QTime(20, 17, 40), QTimeZone::UTC) })
      {
         QFile writer(path);
         QVERIFY(writer.open(QIODevice::Append));
         QCOMPARE(writer.write("+", 1), qint64(1));
         QVERIFY(writer.flush());
         QVERIFY(writer.setFileTime(date, QFileDevice::FileModificationTime));
         writer.close();
         QCOMPARE(QFileInfo(path).lastModified(), date);
         compare();
      }
   }

#if !defined(Q_OS_WIN) || defined(FILEPOOL_QT_BACKEND)
   void releasedFilesFollowCurrentPath_data()
   {
      QTest::addColumn<bool>("replace");
      QTest::addColumn<bool>("write");
      QTest::newRow("deleted-reader") << false << false;
      QTest::newRow("deleted-writer") << false << true;
      QTest::newRow("replaced-reader") << true << false;
      QTest::newRow("replaced-writer") << true << true;
   }

   void releasedFilesFollowCurrentPath()
   {
      QFETCH(bool, replace);
      QFETCH(bool, write);
      QTemporaryDir temp;
      QVERIFY(temp.isValid());
      const QString path = temp.filePath("file.bin");
      const QString oldPath = temp.filePath("old.bin");
      const QByteArray oldData("old content");
      const QByteArray newData("replacement content");
      {
         QFile physical(path);
         QVERIFY(physical.open(QIODevice::WriteOnly));
         QCOMPARE(physical.write(oldData), oldData.size());
      }
      FM::FilePool pool;
      const auto mode = (write ? QIODevice::ReadWrite : QIODevice::ReadOnly) | QIODevice::Unbuffered;
      QFile* cached = pool.open(path, mode);
      QVERIFY(cached);
      pool.release(cached);
#ifdef Q_OS_WIN
      // Windows denies replacement of an open QFile. Exercise the portable pool's reopening
      // path with a closed cached QFile here; POSIX runs retain the live descriptor across rename.
      cached->close();
#endif
      QVERIFY(QFile::rename(path, oldPath));
      if (replace)
      {
         QFile replacement(path);
         QVERIFY(replacement.open(QIODevice::WriteOnly));
         QCOMPARE(replacement.write(newData), newData.size());
      }

      bool created = true;
      QFile* current = pool.open(path, mode, &created);
      if (!replace && !write)
      {
         QVERIFY(!current);
         QVERIFY(!created);
         QVERIFY(!QFile::exists(path));
      }
      else
      {
         QVERIFY(current);
         QCOMPARE(created, !replace);
         QCOMPARE(current->readAll(), replace ? newData : QByteArray());
         if (write)
         {
            QVERIFY(current->seek(0));
            QCOMPARE(current->write("!", 1), qint64(1));
            QVERIFY(current->flush());
         }
         pool.release(current, true);
         if (write)
         {
            QFile physical(path);
            QVERIFY(physical.open(QIODevice::ReadOnly));
            QCOMPARE(physical.readAll(), replace ? QByteArray("!") + newData.mid(1) : QByteArray("!"));
         }
      }
      QFile oldFile(oldPath);
      QVERIFY(oldFile.open(QIODevice::ReadOnly));
      QCOMPARE(oldFile.readAll(), oldData);
      const auto remaining = pool.takeAll(path);
      qDeleteAll(remaining);
      QVERIFY(remaining.isEmpty());
   }
#endif
};

QTEST_GUILESS_MAIN(FilePoolTests)
#include "FilePoolTests.moc"
