#include <QDir>
#include <QFile>
#include <QTemporaryDir>
#include <QTest>

#include <priv/FileUpdater/DirWatcherLinux.h>

class DirWatcherLinuxTests : public QObject
{
   Q_OBJECT

private slots:
   void directoryMovesPreserveSiblings_data()
   {
      QTest::addColumn<QString>("destination");
      QTest::newRow("rename") << "root/renamed";
      QTest::newRow("move") << "root/dest/a";
      QTest::newRow("move-and-rename") << "root/dest/b";
   }

   void directoryMovesPreserveSiblings()
   {
      QFETCH(QString, destination);
      QTemporaryDir temp;
      QVERIFY(temp.isValid());
      QDir base(temp.path());
      QVERIFY(base.mkpath("root/a/deep"));
      QVERIFY(base.mkpath("root/b"));
      QVERIFY(base.mkpath("root/dest/other"));
      // Catch collisions with the old name at the destination as well as
      // collisions with the new name at the source.
      if (destination == "root/dest/b")
         QVERIFY(base.mkpath("root/dest/a"));

      FM::DirWatcherLinux watcher;
      QVERIFY(watcher.addPath(temp.filePath("root")));
      QVERIFY(base.rename("root/a", destination));

      const auto events = watcher.waitEvent(1000);
      QCOMPARE(events.size(), 1);
      QCOMPARE(events[0].type, FM::WatcherEvent::MOVE);
      QCOMPARE(events[0].path1, temp.filePath("root/a"));
      QCOMPARE(events[0].path2, temp.filePath(destination));

      QStringList directories{"root/b", "root/dest/other", destination, destination + "/deep"};
      if (destination == "root/dest/b")
         directories << "root/dest/a";
      for (const QString& directory : directories)
      {
         const QString path = temp.filePath(directory + "/new.txt");
         QFile file(path);
         QVERIFY(file.open(QIODevice::WriteOnly));
         QCOMPARE(file.write("data"), qint64(4));
         file.close();
         bool found = false;
         for (const auto& event : watcher.waitEvent(1000))
            if (event.type == FM::WatcherEvent::NEW && event.path1 == path && !event.isWatchedFile)
               found = true;
         QVERIFY2(found, qPrintable(path));
      }
   }

   void overlappingDirectories_data()
   {
      QTest::addColumn<bool>("childFirst");
      QTest::addColumn<bool>("removeParent");
      QTest::newRow("child-first-remove-child") << true << false;
      QTest::newRow("parent-first-remove-child") << false << false;
      QTest::newRow("child-first-remove-parent") << true << true;
      QTest::newRow("parent-first-remove-parent") << false << true;
   }

   void overlappingDirectories()
   {
      QFETCH(bool, childFirst);
      QFETCH(bool, removeParent);
      QTemporaryDir temp;
      QVERIFY(temp.isValid());
      const QString parent = temp.filePath("root");
      const QString child = temp.filePath("root/sub");
      QVERIFY(QDir().mkpath(child + "/deep"));

      FM::DirWatcherLinux watcher;
      QVERIFY(watcher.addPath(childFirst ? child : parent));
      QVERIFY(watcher.addPath(childFirst ? parent : child));
      QCOMPARE(watcher.nbWatchedPath(), 2);
      watcher.rmPath(removeParent ? parent : child);
      QCOMPARE(watcher.nbWatchedPath(), 1);

      // Existing watches at both levels must survive releasing either owner.
      for (const QString& path : QStringList{child + "/file.txt", child + "/deep/file.txt"})
      {
         QFile file(path);
         QVERIFY(file.open(QIODevice::WriteOnly));
         QCOMPARE(file.write("data"), qint64(4));
         file.close();
         bool found = false;
         for (const auto& event : watcher.waitEvent(1000))
            if (event.type == FM::WatcherEvent::NEW && event.path1 == path)
               found = true;
         QVERIFY(found);
      }

      // Preserve root self-notifications, but don't treat a surviving child as a root.
      const QString moved = temp.filePath("root/moved");
      QVERIFY(QDir().rename(child, moved));
      bool found = false;
      for (const auto& event : watcher.waitEvent(1000))
      {
         if (removeParent)
            found |= event.type == FM::WatcherEvent::DELETED && event.path1 == child;
         else
         {
            QVERIFY(event.type != FM::WatcherEvent::DELETED);
            found |= event.type == FM::WatcherEvent::MOVE && event.path1 == child && event.path2 == moved;
         }
      }
      QVERIFY(found);
   }

   void directoryNotifications_data()
   {
      QTest::addColumn<QString>("relativePath");
      QTest::newRow("root") << "first/file.txt";
      QTest::newRow("nested") << "first/child/grandchild/file.txt";
      QTest::newRow("second-root") << "second/file.txt";
   }

   void directoryNotifications()
   {
      QFETCH(QString, relativePath);
      QTemporaryDir temp;
      QVERIFY(temp.isValid());
      QVERIFY(QDir(temp.path()).mkpath("first/child/grandchild"));
      QVERIFY(QDir(temp.path()).mkdir("second"));
      FM::DirWatcherLinux watcher;
      QVERIFY(watcher.addPath(temp.filePath("first")));
      QVERIFY(watcher.addPath(temp.filePath("second")));

      const QString path = temp.filePath(relativePath);
      QFile file(path);
      QVERIFY(file.open(QIODevice::WriteOnly));
      QCOMPARE(file.write("data"), qint64(4));
      file.close();

      bool found = false;
      for (const auto& event : watcher.waitEvent(1000))
         if (event.type == FM::WatcherEvent::NEW && event.path1 == path && !event.isWatchedFile)
            found = true;
      QVERIFY(found);
   }

   void fileNotificationsAndRemoval()
   {
      QTemporaryDir temp;
      QVERIFY(temp.isValid());
      QFile file(temp.filePath("file.txt"));
      QVERIFY(file.open(QIODevice::WriteOnly));
      file.close();

      FM::DirWatcherLinux watcher;
      QVERIFY(watcher.addPath(temp.path(), "file.txt"));
      QVERIFY(file.open(QIODevice::Append));
      QCOMPARE(file.write("data"), qint64(4));
      file.close();

      bool found = false;
      for (const auto& event : watcher.waitEvent(1000))
         if (event.type == FM::WatcherEvent::CONTENT_CHANGED && event.path1 == file.fileName() && event.isWatchedFile)
            found = true;
      QVERIFY(found);

      watcher.rmPath(temp.path(), "file.txt");
      QCOMPARE(watcher.nbWatchedPath(), 0);
      // An IN_IGNORED event for the removed watch must not resolve to a directory.
      QVERIFY(watcher.waitEvent(1000).isEmpty());
      QVERIFY(watcher.addPath(temp.path(), "file.txt"));
      // The destructor also releases file watches using their owner pointer.
   }
};

QTEST_GUILESS_MAIN(DirWatcherLinuxTests)
#include "DirWatcherLinuxTests.moc"
