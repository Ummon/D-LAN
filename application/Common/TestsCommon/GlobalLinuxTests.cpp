#include <QTest>
#include <QDir>
#include <QFile>
#include <QScopeGuard>
#include <QStandardPaths>
#include <QTemporaryDir>
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
   void quickAccessFolders_data()
   {
      QTest::addColumn<QStringList>("configuredFolders");
      QTest::addColumn<QStringList>("existingFolders");
      QTest::addColumn<QStringList>("expectedFolders");
      const QStringList standardFolders{
         "Bureau", "Mes documents", "Téléchargements", "Musique",
         "Images", "Vidéos", "Public", "Modèles"
      };
      QTest::newRow("configured-user-directories") << standardFolders << standardFolders << standardFolders;
      QTest::newRow("duplicates-and-missing-folders")
         << QStringList{"$HOME", "Documents", "Documents/", "Music", "music", "missing", "not-a-directory", "$HOME"}
         << QStringList{"Documents", "Music", "music"}
         << QStringList{"Documents", "Music", "music"};
      QTest::newRow("all-disabled")
         << QStringList{"$HOME", "$HOME", "$HOME", "$HOME", "$HOME", "$HOME", "$HOME", "$HOME"}
         << QStringList{} << QStringList{};
   }

   void quickAccessFolders()
   {
      QFETCH(QStringList, configuredFolders);
      QFETCH(QStringList, existingFolders);
      QFETCH(QStringList, expectedFolders);
      QTemporaryDir directory;
      QVERIFY(directory.isValid());
      for (const auto& folder : existingFolders)
         QVERIFY(QDir(directory.path()).mkpath(folder));
      QFile regularFile(directory.filePath("not-a-directory"));
      QVERIFY(regularFile.open(QIODevice::WriteOnly));
      regularFile.close();

      const QStringList keys{"DESKTOP", "DOCUMENTS", "DOWNLOAD", "MUSIC", "PICTURES", "VIDEOS", "PUBLICSHARE", "TEMPLATES"};
      QFile config(directory.filePath("user-dirs.dirs"));
      QVERIFY(config.open(QIODevice::WriteOnly));
      for (int i = 0; i < keys.size(); ++i)
      {
         const QString path = configuredFolders[i] == "$HOME" ? configuredFolders[i] : directory.filePath(configuredFolders[i]);
         const QByteArray line = QString("XDG_%1_DIR=\"%2\"\n").arg(keys[i], path).toUtf8();
         QCOMPARE(config.write(line), line.size());
      }
      config.close();

      // Isolate XDG configuration without changing the real home or its contents.
      const QByteArray previousConfig = qgetenv("XDG_CONFIG_HOME");
      const auto restoreConfig = qScopeGuard([previousConfig]
      {
         if (previousConfig.isNull())
            qunsetenv("XDG_CONFIG_HOME");
         else
            qputenv("XDG_CONFIG_HOME", previousConfig);
      });
      QVERIFY(qputenv("XDG_CONFIG_HOME", directory.path().toUtf8()));

      const auto folders = Common::Global::getQuickAccessFolders();
      QCOMPARE(folders.size(), expectedFolders.size() + 1);
      QCOMPARE(folders.first().path, QDir::cleanPath(QDir::homePath()));
      QVERIFY(!folders.first().name.isEmpty());
      for (int i = 0; i < expectedFolders.size(); ++i)
      {
         QCOMPARE(folders[i + 1].path, directory.filePath(expectedFolders[i]));
         QVERIFY(!folders[i + 1].name.isEmpty());
         QVERIFY(!folders[i + 1].path.endsWith('/'));
      }
      if (expectedFolders.size() == keys.size())
      {
         const QList<QStandardPaths::StandardLocation> locations{
            QStandardPaths::DesktopLocation, QStandardPaths::DocumentsLocation, QStandardPaths::DownloadLocation,
            QStandardPaths::MusicLocation, QStandardPaths::PicturesLocation, QStandardPaths::MoviesLocation,
            QStandardPaths::PublicShareLocation, QStandardPaths::TemplatesLocation
         };
         for (int i = 0; i < locations.size(); ++i)
            QCOMPARE(folders[i + 1].name, QStandardPaths::displayName(locations[i]));
      }
      QVERIFY(!QFile::exists(directory.filePath("missing")));
   }

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
