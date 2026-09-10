#include <QDir>
#include <QElapsedTimer>
#include <QFile>
#include <QTemporaryDir>
#include <QTest>

#include <priv/FileUpdater/DirWatcherLinux.h>

class DirWatcherLinuxTests : public QObject
{
   Q_OBJECT

private slots:
   void symlinkRootsAreRejected_data()
   {
      QTest::addColumn<bool>("directory");
      QTest::addColumn<bool>("trailingSlash");
      QTest::newRow("directory") << true << false;
      QTest::newRow("directory-with-trailing-slash") << true << true;
      QTest::newRow("file") << false << false;
   }

   void symlinkRootsAreRejected()
   {
      QFETCH(bool, directory);
      QFETCH(bool, trailingSlash);
      QTemporaryDir temp;
      QVERIFY(temp.isValid());
      if (directory)
         QVERIFY(QDir(temp.path()).mkdir("target"));
      else
      {
         QFile file(temp.filePath("target"));
         QVERIFY(file.open(QIODevice::WriteOnly));
      }
      QVERIFY(QFile::link(temp.filePath("target"), temp.filePath("link")));
      FM::DirWatcherLinux watcher;
      QVERIFY(!watcher.addPath(temp.filePath("link") + (trailingSlash ? "/" : "")));
      QVERIFY(!watcher.addPath(temp.path(), "link"));
      QCOMPARE(watcher.nbWatchedPath(), 0);
   }

   void symlinkTargetsAreNotWatched_data()
   {
      QTest::addColumn<QString>("when");
      QTest::newRow("initial-tree") << "initial";
      QTest::newRow("created-after-registration") << "created";
      QTest::newRow("moved-in-tree") << "moved";
      QTest::newRow("moved-in-links") << "moved-links";
   }

   void symlinkTargetsAreNotWatched()
   {
      QFETCH(QString, when);
      QTemporaryDir temp;
      QVERIFY(temp.isValid());
      QDir base(temp.path());
      QVERIFY(base.mkpath("root"));
      QVERIFY(base.mkpath("incoming"));
      QVERIFY(base.mkpath("outside/deep"));
      {
         QFile file(temp.filePath("outside/file.txt"));
         QVERIFY(file.open(QIODevice::WriteOnly));
      }
      FM::DirWatcherLinux watcher;
      if (when != "initial")
         QVERIFY(watcher.addPath(temp.filePath("root")));
      const QString container = when.startsWith("moved") ? temp.filePath("incoming") : temp.filePath("root");
      QVERIFY(QFile::link(temp.filePath("outside"), container + "/dir-link"));
      QVERIFY(QFile::link(temp.filePath("outside/file.txt"), container + "/file-link"));
      // An ancestor link must not cause recursive traversal either.
      QVERIFY(QFile::link(temp.filePath("root"), container + "/loop"));
      if (when == "initial")
         QVERIFY(watcher.addPath(temp.filePath("root")));
      else if (when == "moved")
         QVERIFY(base.rename("incoming", "root/incoming"));
      else if (when == "moved-links")
         for (const QString& name : QStringList{"dir-link", "file-link", "loop"})
            QVERIFY(base.rename("incoming/" + name, "root/" + name));
      for (const auto& event : watcher.waitEvent(0))
      {
         QVERIFY(!event.path1.endsWith("dir-link"));
         QVERIFY(!event.path1.endsWith("file-link"));
         QVERIFY(!event.path1.endsWith("loop"));
      }

      for (const QString& path : QStringList{temp.filePath("outside/deep/new.txt"), temp.filePath("outside/file.txt")})
      {
         QFile file(path);
         QVERIFY(file.open(QIODevice::WriteOnly));
         QCOMPARE(file.write("data"), qint64(4));
      }
      for (const auto& event : watcher.waitEvent(0))
         QCOMPARE(event.type, FM::WatcherEvent::TIMEOUT);

      const QString realPath = when == "moved" ? temp.filePath("root/incoming/real.txt") : temp.filePath("root/real.txt");
      {
         QFile file(realPath);
         QVERIFY(file.open(QIODevice::WriteOnly));
      }
      bool found = false;
      for (const auto& event : watcher.waitEvent(1000))
         found |= event.type == FM::WatcherEvent::NEW && event.path1 == realPath;
      QVERIFY(found);
   }

   void removedRootsReleaseWatches_data()
   {
      QTest::addColumn<bool>("rename");
      QTest::addColumn<bool>("explicitRemoval");
      QTest::newRow("renamed-root") << true << false;
      QTest::newRow("deleted-root") << false << false;
      QTest::newRow("remove-renamed-root") << true << true;
      QTest::newRow("remove-deleted-root") << false << true;
   }

   void removedRootsReleaseWatches()
   {
      QFETCH(bool, rename);
      QFETCH(bool, explicitRemoval);
      QTemporaryDir temp;
      QVERIFY(temp.isValid());
      QDir base(temp.path());
      QVERIFY(base.mkpath("root/deep"));
      const QString root = temp.filePath("root");
      FM::DirWatcherLinux watcher;
      QVERIFY(watcher.addPath(root));
      if (rename)
         QVERIFY(base.rename("root", "outside"));
      else
         QVERIFY(QDir(root).removeRecursively());

      if (explicitRemoval)
         watcher.rmPath(root);
      else
      {
         bool found = false;
         for (const auto& event : watcher.waitEvent(1000))
            found |= event.type == FM::WatcherEvent::DELETED && event.path1 == root;
         QVERIFY(found);
      }
      QCOMPARE(watcher.nbWatchedPath(), 0);
      watcher.rmPath(root); // Repeated removal is harmless.

      if (rename)
      {
         QFile outside(temp.filePath("outside/deep/new.txt"));
         QVERIFY(outside.open(QIODevice::WriteOnly));
      }
      for (const auto& event : watcher.waitEvent(0))
         QCOMPARE(event.type, FM::WatcherEvent::TIMEOUT);
   }

   void movedOutSubtreeStopsNotifications_data()
   {
      QTest::addColumn<bool>("replaceBeforeRead");
      QTest::newRow("move-out") << false;
      QTest::newRow("reuse-old-path-before-reading") << true;
   }

   void movedOutSubtreeStopsNotifications()
   {
      QFETCH(bool, replaceBeforeRead);
      QTemporaryDir temp;
      QVERIFY(temp.isValid());
      QDir base(temp.path());
      QVERIFY(base.mkpath("root/sub/deep"));
      FM::DirWatcherLinux watcher;
      QVERIFY(watcher.addPath(temp.filePath("root")));
      QVERIFY(base.rename("root/sub", "outside"));
      // Queue an event from outside the share in the same read as the move.
      {
         QFile outside(temp.filePath("outside/deep/before.txt"));
         QVERIFY(outside.open(QIODevice::WriteOnly));
      }
      if (replaceBeforeRead)
         QVERIFY(base.mkpath("root/sub/deep"));

      bool deleted = false;
      for (const auto& event : watcher.waitEvent(1000))
      {
         deleted |= event.type == FM::WatcherEvent::DELETED && event.path1 == temp.filePath("root/sub");
         if (event.type == FM::WatcherEvent::NEW && event.path1 == temp.filePath("root/sub"))
            QVERIFY(deleted); // FileUpdater must delete the old entry before adding its replacement.
         QVERIFY(!event.path1.endsWith("before.txt"));
      }
      QVERIFY(deleted);
      {
         QFile outside(temp.filePath("outside/deep/after.txt"));
         QVERIFY(outside.open(QIODevice::WriteOnly));
      }
      for (const auto& event : watcher.waitEvent(0))
         QCOMPARE(event.type, FM::WatcherEvent::TIMEOUT);

      if (!replaceBeforeRead)
      {
         QVERIFY(base.mkpath("root/sub/deep"));
         watcher.waitEvent(1000);
      }
      QFile replacement(temp.filePath("root/sub/deep/new.txt"));
      QVERIFY(replacement.open(QIODevice::WriteOnly));
      replacement.close();
      bool found = false;
      for (const auto& event : watcher.waitEvent(1000))
         found |= event.type == FM::WatcherEvent::NEW && event.path1 == replacement.fileName();
      QVERIFY(found);
   }

   void directoryMoveAcrossReads()
   {
      QTemporaryDir temp;
      QVERIFY(temp.isValid());
      QDir base(temp.path());
      QVERIFY(base.mkpath("root/sub/deep"));
      const QStringList floodPaths{temp.filePath("root/a"), temp.filePath("root/b")};
      for (const QString& path : floodPaths)
      {
         QFile file(path);
         QVERIFY(file.open(QIODevice::WriteOnly));
      }
      FM::DirWatcherLinux watcher;
      QVERIFY(watcher.addPath(temp.filePath("root")));
      // 1023 short-name events leave one slot in the 32 KiB read buffer,
      // placing IN_MOVED_FROM and IN_MOVED_TO in separate reads.
      for (int i = 0; i < 1023; ++i)
      {
         QFile file(floodPaths[i % 2]);
         QVERIFY(file.open(QIODevice::WriteOnly));
      }
      QVERIFY(base.rename("root/sub", "root/moved"));
      bool deleted = false;
      for (const auto& event : watcher.waitEvent(1000))
         deleted |= event.type == FM::WatcherEvent::DELETED && event.path1 == temp.filePath("root/sub");
      QVERIFY(deleted);
      bool added = false;
      for (const auto& event : watcher.waitEvent(1000))
         added |= event.type == FM::WatcherEvent::NEW && event.path1 == temp.filePath("root/moved");
      QVERIFY(added);
      QFile file(temp.filePath("root/moved/deep/new.txt"));
      QVERIFY(file.open(QIODevice::WriteOnly));
      file.close();
      bool found = false;
      for (const auto& event : watcher.waitEvent(1000))
         found |= event.type == FM::WatcherEvent::NEW && event.path1 == file.fileName();
      QVERIFY(found);
   }

   void queueOverflowRecoversWatches()
   {
      QFile limitFile("/proc/sys/fs/inotify/max_queued_events");
      QVERIFY(limitFile.open(QIODevice::ReadOnly));
      bool validLimit = false;
      const int limit = limitFile.readAll().trimmed().toInt(&validLimit);
      QVERIFY(validLimit && limit > 0);
      if (limit > 100000)
         QSKIP("The system inotify queue is too large for this bounded overflow test.");

      QTemporaryDir temp;
      QVERIFY(temp.isValid());
      QDir base(temp.path());
      for (const QString& path : QStringList{"root/old", "root/before", "second", "gone"})
         QVERIFY(base.mkpath(path));
      const QString filePath = temp.filePath("individual.txt");
      const QString goneFilePath = temp.filePath("gone.txt");
      const QStringList floodPaths{temp.filePath("root/a"), temp.filePath("root/b")};
      for (const QString& path : QStringList{filePath, goneFilePath, floodPaths[0], floodPaths[1]})
      {
         QFile file(path);
         QVERIFY(file.open(QIODevice::WriteOnly));
      }

      FM::DirWatcherLinux watcher;
      QVERIFY(watcher.addPath(temp.filePath("root")));
      QVERIFY(watcher.addPath(temp.filePath("second")));
      QVERIFY(watcher.addPath(temp.filePath("gone")));
      QVERIFY(watcher.addPath(temp.path(), "individual.txt"));
      QVERIFY(watcher.addPath(temp.path(), "gone.txt"));

      // Alternate names so adjacent IN_CLOSE_WRITE events cannot be coalesced.
      for (int i = 0; i <= limit; ++i)
      {
         QFile file(floodPaths[i % 2]);
         QVERIFY(file.open(QIODevice::WriteOnly));
      }

      // These notifications are lost while the queue is full.
      QVERIFY(base.rename("root/old", "outside"));
      QVERIFY(base.rename("root/before", "root/after"));
      QVERIFY(base.mkpath("root/new/deep"));
      QVERIFY(base.rmdir("gone"));
      QVERIFY(QFile::remove(goneFilePath));
      QVERIFY(QFile::remove(filePath));
      {
         QFile replacement(filePath);
         QVERIFY(replacement.open(QIODevice::WriteOnly));
      }

      QList<FM::WatcherEvent> recovery;
      QElapsedTimer timer;
      timer.start();
      while (timer.elapsed() < 10000)
      {
         const auto events = watcher.waitEvent(0);
         for (const auto& event : events)
            if (event.type == FM::WatcherEvent::RESCAN || event.type == FM::WatcherEvent::WATCH_LOST)
               recovery << event;
         if (!recovery.isEmpty() || events.isEmpty() || events[0].type == FM::WatcherEvent::TIMEOUT)
            break;
      }
      QCOMPARE(recovery.size(), 5);
      for (const auto& event : recovery)
      {
         const bool lost = event.path1 == temp.filePath("gone") || event.path1 == goneFilePath;
         QCOMPARE(event.type, lost ? FM::WatcherEvent::WATCH_LOST : FM::WatcherEvent::RESCAN);
         QCOMPARE(event.isWatchedFile, event.path1 == filePath || event.path1 == goneFilePath);
      }
      QCOMPARE(watcher.nbWatchedPath(), 3);

      // The fresh queue contains no notifications from the abandoned watch tree.
      const auto idle = watcher.waitEvent(0);
      QCOMPARE(idle.size(), 1);
      QCOMPARE(idle[0].type, FM::WatcherEvent::TIMEOUT);

      for (const QString& path : QStringList{temp.filePath("root/new/deep/new.txt"),
                                             temp.filePath("root/after/new.txt"),
                                             temp.filePath("second/new.txt"), filePath})
      {
         QFile file(path);
         QVERIFY(file.open(QIODevice::WriteOnly));
         QCOMPARE(file.write("data"), qint64(4));
         file.close();
         bool found = false;
         for (const auto& event : watcher.waitEvent(1000))
            if (event.path1 == path && event.type == (path == filePath ? FM::WatcherEvent::CONTENT_CHANGED : FM::WatcherEvent::NEW))
               found = true;
         QVERIFY2(found, qPrintable(path));
      }
      {
         QFile outside(temp.filePath("outside/new.txt"));
         QVERIFY(outside.open(QIODevice::WriteOnly));
      }
      const auto outsideEvents = watcher.waitEvent(0);
      QCOMPARE(outsideEvents.size(), 1);
      QCOMPARE(outsideEvents[0].type, FM::WatcherEvent::TIMEOUT);
   }

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
