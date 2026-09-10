#include <QTest>
#include <Common/Global.h>

#include <cerrno>
#include <limits>
#include <sys/statvfs.h>

namespace
{
   struct statvfs filesystemInfo;
   bool queryFails = false;
}

// Control the filesystem geometry without requiring a special mounted volume.
extern "C" int __wrap_statvfs(const char*, struct statvfs* info)
{
   if (queryFails)
   {
      errno = EIO;
      return -1;
   }
   *info = filesystemInfo;
   return 0;
}

class GlobalLinuxTests : public QObject
{
   Q_OBJECT

private slots:
   void availableDiskSpace_data()
   {
      QTest::addColumn<quint64>("blockSize");
      QTest::addColumn<quint64>("fragmentSize");
      QTest::addColumn<quint64>("availableBlocks");
      QTest::addColumn<qint64>("expectedBytes");
      QTest::newRow("equal-sizes") << quint64(4096) << quint64(4096) << quint64(100) << qint64(409600);
      QTest::newRow("smaller-fragments") << quint64(4096) << quint64(1024) << quint64(100) << qint64(102400);
      QTest::newRow("larger-fragments") << quint64(1024) << quint64(4096) << quint64(100) << qint64(409600);
      QTest::newRow("no-space") << quint64(4096) << quint64(1024) << quint64(0) << qint64(0);
      QTest::newRow("over-four-gib") << quint64(4096) << quint64(1024) << quint64(8388608) << qint64(8589934592LL);
   }

   void availableDiskSpace()
   {
      QFETCH(quint64, blockSize);
      QFETCH(quint64, fragmentSize);
      QFETCH(quint64, availableBlocks);
      QFETCH(qint64, expectedBytes);
      filesystemInfo = {};
      filesystemInfo.f_bsize = blockSize;
      filesystemInfo.f_frsize = fragmentSize;
      filesystemInfo.f_bavail = availableBlocks;
      filesystemInfo.f_bfree = availableBlocks + 100; // Reserved blocks are unavailable to this user.
      QCOMPARE(Common::Global::availableDiskSpace("/"), expectedBytes);
   }

   void failedQueryPreservesFallback()
   {
      queryFails = true;
      const qint64 result = Common::Global::availableDiskSpace("/");
      queryFails = false;
      QCOMPARE(result, std::numeric_limits<qint64>::max());
   }
};

QTEST_GUILESS_MAIN(GlobalLinuxTests)
#include "GlobalLinuxTests.moc"
