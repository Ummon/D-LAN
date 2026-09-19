#include <QDir>
#include <QFile>
#include <QTemporaryDir>
#include <QTest>
#include <QStandardPaths>
#include <QProcess>
#include <QTextStream>

#include <Common/Global.h>
#include <Common/PersistentData.h>
#include <Protos/gui_settings.pb.h>

#include <cerrno>
#include <dlfcn.h>
#include <limits>
#include <sys/mount.h>

namespace
{
   using Statfs = int (*)(const char*, struct statfs*);
   Statfs nativeStatfs()
   {
      static const auto query = reinterpret_cast<Statfs>(dlsym(RTLD_NEXT, "statfs" __DARWIN_SUF_64_BIT_INO_T));
      return query;
   }

   bool mockQuery = true;
   struct statfs filesystemInfo;
   int queryError = 0;
   QHash<QString, int> pathErrors;
   QStringList queriedPaths;
}

// Common is a static library, so this executable's symbol replaces its statfs
// call without changing production code or requiring a nearly full test volume.
extern "C" int statfs(const char* path, struct statfs* info)
{
   if (!mockQuery)
      return nativeStatfs()(path, info);
   const QString decoded = QString::fromUtf8(path);
   queriedPaths.append(decoded);
   const int error = pathErrors.value(decoded, queryError);
   if (error)
   {
      errno = error;
      return -1;
   }
   *info = filesystemInfo;
   return 0;
}

class GlobalDarwinTests : public QObject
{
   Q_OBJECT

private slots:
   void resourceFolder()
   {
      QCOMPARE(Common::Global::getResourceFolder(), QCoreApplication::applicationDirPath());
      QTemporaryDir temporary;
      QVERIFY(temporary.isValid());
      const QString contents = QDir(temporary.path()).canonicalPath() + "/D-LAN.app/Contents";
      QVERIFY(QDir().mkpath(contents + "/MacOS"));
      const QString executable = contents + "/MacOS/resource-probe";
      QVERIFY(QFile::copy(QCoreApplication::applicationFilePath(), executable));
      auto probe = [&]()
      {
         QProcess child;
         child.start(executable, {"--print-resource-folder"});
         if (!child.waitForFinished(10000) || child.exitStatus() != QProcess::NormalExit || child.exitCode() != 0)
            return QString();
         return QString::fromUtf8(child.readAllStandardOutput());
      };
      // A folder that only resembles a bundle must retain ordinary build paths.
      QCOMPARE(probe(), contents + "/MacOS");
      QVERIFY(QDir().mkpath(contents + "/Resources"));
      QFile plist(contents + "/Info.plist");
      QVERIFY(plist.open(QIODevice::WriteOnly));
      plist.write("<?xml version=\"1.0\"?><plist version=\"1.0\"><dict/></plist>");
      plist.close();
      QCOMPARE(probe(), contents + "/Resources");
   }

   void nativeDataLocations()
   {
      using Global = Common::Global;
      using Folder = Global::DataFolderType;
      Global::setDataFolderToDefault(Folder::ROAMING);
      Global::setDataFolderToDefault(Folder::LOCAL);
      const QString library = QDir::homePath() + "/Library";
      QCOMPARE(Global::getDataFolder(Folder::ROAMING, false), library + "/Application Support/D-LAN");
      QCOMPARE(Global::getDataFolder(Folder::LOCAL, false), library + "/Application Support/D-LAN");
      QCOMPARE(Global::getCacheFolder(false), library + "/Caches/D-LAN");
      QCOMPARE(Global::getLogFolder(false), library + "/Logs/D-LAN");
   }

   void init()
   {
      mockQuery = true;
      queryError = 0;
      pathErrors.clear();
      queriedPaths.clear();
      filesystemInfo = {};
      filesystemInfo.f_bsize = 4096;
      filesystemInfo.f_bavail = 1024;
   }

   void availableBytes_data()
   {
      QTest::addColumn<quint64>("fragmentSize");
      QTest::addColumn<quint64>("availableBlocks");
      QTest::addColumn<qint64>("expected");
      QTest::newRow("allocation-not-io-size") << quint64(4096) << quint64(100) << qint64(409600);
      QTest::newRow("small-allocation-unit") << quint64(512) << quint64(100) << qint64(51200);
      QTest::newRow("full-volume") << quint64(4096) << quint64(0) << qint64(0);
      QTest::newRow("over-four-gib") << quint64(4096) << quint64(2097152) << qint64(8589934592LL);
      QTest::newRow("over-sixteen-tib") << quint64(4096) << quint64(8589934592ULL) << qint64(35184372088832LL);
      QTest::newRow("saturated-result") << quint64(4096) << std::numeric_limits<quint64>::max()
         << std::numeric_limits<qint64>::max();
   }

   void availableBytes()
   {
      QFETCH(quint64, fragmentSize);
      QFETCH(quint64, availableBlocks);
      QFETCH(qint64, expected);
      filesystemInfo.f_iosize = 1048576; // I/O size must not determine available bytes.
      filesystemInfo.f_bsize = fragmentSize;
      filesystemInfo.f_bavail = availableBlocks;
      filesystemInfo.f_bfree = availableBlocks == std::numeric_limits<quint64>::max()
         ? availableBlocks : availableBlocks + 100; // Reserved blocks must not be counted.
      QCOMPARE(Common::Global::availableDiskSpace("/"), expected);
   }

   void fileUsesContainingDirectory()
   {
      QTemporaryDir temp;
      QVERIFY(temp.isValid());
      QFile file(temp.filePath(QString::fromUtf8("été 日本語.txt")));
      QVERIFY(file.open(QIODevice::WriteOnly));
      file.close();
      QCOMPARE(Common::Global::availableDiskSpace(file.fileName()), qint64(4194304));
      QCOMPARE(queriedPaths, QStringList{temp.path()});
   }

   void missingDestinationUsesExistingParent()
   {
      QTemporaryDir temp;
      QVERIFY(temp.isValid());
      const QString parent = temp.filePath(QString::fromUtf8("Téléchargements"));
      const QString destination = parent + "/new.bin";
      pathErrors.insert(destination, ENOENT);
      pathErrors.insert(parent, ENOENT);
      QCOMPARE(Common::Global::availableDiskSpace(destination), qint64(4194304));
      QCOMPARE(queriedPaths, (QStringList{destination, parent, temp.path()}));
      QVERIFY(!QFileInfo::exists(parent)); // Querying must never create anything.
   }

   void failedQuery_data()
   {
      QTest::addColumn<int>("error");
      QTest::newRow("permission-denied") << EACCES;
      QTest::newRow("io-error") << EIO;
      QTest::newRow("invalid-path") << ENOTDIR;
      QTest::newRow("missing-root") << ENOENT;
   }

   void failedQuery()
   {
      QFETCH(int, error);
      queryError = error;
      QCOMPARE(Common::Global::availableDiskSpace("/"), std::numeric_limits<qint64>::max());
      QCOMPARE(queriedPaths.size(), 1);
   }

   void inaccessibleDestinationDoesNotUseParent()
   {
      queryError = EACCES;
      QCOMPARE(Common::Global::availableDiskSpace("/inaccessible/file"), std::numeric_limits<qint64>::max());
      QCOMPARE(queriedPaths, QStringList{"/inaccessible/file"});
   }

   void persistenceHonoursLowSpace_data()
   {
      QTest::addColumn<quint64>("availableMiB");
      QTest::newRow("full") << quint64(0);
      QTest::newRow("below-reserve") << quint64(19);
      QTest::newRow("sufficient-space") << quint64(21);
   }

   void persistenceHonoursLowSpace()
   {
      QFETCH(quint64, availableMiB);
      QTemporaryDir temp;
      QVERIFY(temp.isValid());
      filesystemInfo.f_bsize = 1024 * 1024;
      filesystemInfo.f_bavail = availableMiB;
      const QString destination = temp.filePath("settings.json");
      pathErrors.insert(destination, ENOENT);
      Protos::GUI::Settings settings;
      const auto save = [&] {
         Common::PersistentData::setValue(temp.path(), "settings.json", settings,
            Common::Global::DataFolderType::ROAMING, true);
      };
      if (availableMiB < 20)
      {
         QVERIFY_THROWS_EXCEPTION(Common::PersistentDataIOException, save());
         QVERIFY(QDir(temp.path()).entryList(QDir::Files).isEmpty());
      }
      else
      {
         save();
         QVERIFY(QFileInfo::exists(destination));
      }
      QCOMPARE(queriedPaths, (QStringList{destination, temp.path()}));
   }

   void realFilesystem_data()
   {
      QTest::addColumn<QString>("kind");
      for (const auto& kind : {"directory", "file", "missing", "relative", "symlink"})
         QTest::newRow(kind) << QString(kind);
   }

   void realFilesystem()
   {
      QFETCH(QString, kind);
      QTemporaryDir temp;
      QVERIFY(temp.isValid());
      const QString directory = temp.filePath(QString::fromUtf8("été 日本語"));
      QVERIFY(QDir().mkpath(directory));
      QString path = directory;
      if (kind == "file")
      {
         path += "/existing.txt";
         QFile file(path);
         QVERIFY(file.open(QIODevice::WriteOnly));
      }
      else if (kind == "missing")
         path += "/not-yet-created/settings.json";
      else if (kind == "relative")
         path = QDir::current().relativeFilePath(path);
      else if (kind == "symlink")
      {
         path = temp.filePath("link");
         QVERIFY(QFile::link(directory, path));
      }
      QVERIFY(nativeStatfs());
      mockQuery = false;
      struct statfs before, after;
      QCOMPARE(nativeStatfs()(directory.toUtf8().constData(), &before), 0);
      const qint64 available = Common::Global::availableDiskSpace(path);
      QCOMPARE(nativeStatfs()(directory.toUtf8().constData(), &after), 0);
      const qint64 lower = qMin(before.f_bavail * before.f_bsize, after.f_bavail * after.f_bsize);
      const qint64 upper = qMax(before.f_bavail * before.f_bsize, after.f_bavail * after.f_bsize);
      // Other processes can allocate/free blocks between these three queries.
      constexpr qint64 tolerance = 64 * 1024 * 1024;
      QVERIFY(available >= 0);
      QVERIFY(available >= lower - tolerance && available <= upper + tolerance);
   }
};

int main(int argc, char** argv)
{
   QCoreApplication app(argc, argv);
   if (app.arguments().contains("--print-resource-folder"))
   {
      QTextStream(stdout) << Common::Global::getResourceFolder();
      return 0;
   }
   GlobalDarwinTests tests;
   return QTest::qExec(&tests, argc, argv);
}
#include "GlobalDarwinTests.moc"
