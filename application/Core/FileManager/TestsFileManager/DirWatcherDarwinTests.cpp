#include <QDir>
#include <QElapsedTimer>
#include <QFile>
#include <QSaveFile>
#include <QTemporaryDir>
#include <QTest>

#include <priv/FileUpdater/DirWatcherDarwin.h>
#include <priv/FileUpdater/WaitConditionDarwin.h>

#include <chrono>
#include <future>
#include <thread>
#include <unistd.h>

class DirWatcherDarwinTests : public QObject
{
   Q_OBJECT

   static bool writeFile(const QString& path, const QByteArray& data = "data")
   {
      QFile file(path);
      return file.open(QIODevice::WriteOnly) && file.write(data) == data.size();
   }

   static bool timeout(const QList<FM::WatcherEvent>& events)
   {
      return events.size() == 1 && events.first().type == FM::WatcherEvent::TIMEOUT;
   }

   static void settle(FM::DirWatcherDarwin& watcher)
   {
      QElapsedTimer timer;
      timer.start();
      while (timer.elapsed() < 3000 && !timeout(watcher.waitEvent(300))) {}
   }

   static bool detects(FM::DirWatcherDarwin& watcher, const QString& path, bool file = false)
   {
      QElapsedTimer timer;
      timer.start();
      while (timer.elapsed() < 5000)
         for (const auto& event : watcher.waitEvent(100))
            if ((event.type == FM::WatcherEvent::RESCAN || event.type == FM::WatcherEvent::CONTENT_CHANGED ||
                 event.type == FM::WatcherEvent::WATCH_LOST) && event.isWatchedFile == file &&
                (event.path1 == path || (!file && path.startsWith(event.path1 + '/'))))
               return true;
      return false;
   }

private slots:
   void waitConditionConsumesReleaseOnce()
   {
      FM::DirWatcherDarwin watcher;
      FM::WaitConditionDarwin condition;
      for (bool viaWatcher : {false, true, false})
      {
         for (int i = 0; i < 100000; ++i)
            condition.release(); // Saturation must neither block nor lose the wakeup.
         if (viaWatcher)
            QVERIFY(watcher.waitEvent(0, {&condition}).isEmpty());
         else
            QVERIFY(!condition.wait(0));
         QVERIFY(condition.wait(10));
         QVERIFY(timeout(watcher.waitEvent(0, {&condition})));
      }
   }

   void releaseInterruptsBlockedWatcher()
   {
      FM::DirWatcherDarwin watcher;
      FM::WaitConditionDarwin first, second;
      auto waiting = std::async(std::launch::async, [&] { return watcher.waitEvent(3000, {&first, &second}); });
      std::this_thread::sleep_for(std::chrono::milliseconds(30));
      second.release();
      QVERIFY(waiting.wait_for(std::chrono::seconds(1)) == std::future_status::ready);
      QVERIFY(waiting.get().isEmpty());
      QVERIFY(second.wait(0));
   }

   void recursiveChanges()
   {
      QTemporaryDir temp;
      QVERIFY(temp.isValid());
      QVERIFY(QDir().mkpath(temp.filePath("a/b")));
      FM::DirWatcherDarwin watcher;
      QVERIFY(watcher.addPath(temp.path()));
      settle(watcher);
      const QString file = temp.filePath("a/b/file.txt");
      QVERIFY(writeFile(file));
      QVERIFY(detects(watcher, file));
      settle(watcher);
      QVERIFY(writeFile(file, "changed content"));
      QVERIFY(detects(watcher, file));
      settle(watcher);
      const QString renamed = temp.filePath("a/b/renamed.txt");
      QVERIFY(QFile::rename(file, renamed));
      QVERIFY(detects(watcher, renamed));
      settle(watcher);
      QVERIFY(QFile::remove(renamed));
      QVERIFY(detects(watcher, renamed));
      settle(watcher);
      QVERIFY(QDir().mkpath(temp.filePath("new/deep")));
      QVERIFY(writeFile(temp.filePath("new/deep/new.txt")));
      QVERIFY(detects(watcher, temp.filePath("new/deep/new.txt")));
      settle(watcher);
      QVERIFY(QDir(temp.filePath("new")).removeRecursively());
      QVERIFY(detects(watcher, temp.filePath("new")));
   }

   void sharedFileReplacementAndFiltering()
   {
      QTemporaryDir temp;
      QVERIFY(temp.isValid());
      const QString path = temp.filePath("shared.txt");
      QVERIFY(writeFile(path));
      FM::DirWatcherDarwin watcher;
      QVERIFY(watcher.addPath(temp.path(), "shared.txt"));
      settle(watcher);
      QVERIFY(writeFile(temp.filePath("unrelated.txt")));
      QVERIFY(timeout(watcher.waitEvent(500)));
      QSaveFile replacement(path);
      QVERIFY(replacement.open(QIODevice::WriteOnly));
      replacement.write("replacement");
      QVERIFY(replacement.commit());
      QVERIFY(detects(watcher, path, true));
      settle(watcher);
      QVERIFY(writeFile(path, "updated replacement"));
      QVERIFY(detects(watcher, path, true));
      settle(watcher);
      QVERIFY(QFile::remove(path));
      QVERIFY(detects(watcher, path, true));
   }

   void independentAndOverlappingRegistrations()
   {
      QTemporaryDir temp;
      QVERIFY(temp.isValid());
      QVERIFY(QDir().mkpath(temp.filePath("nested")));
      const QString path = temp.filePath("nested/shared.txt");
      QVERIFY(writeFile(path));
      FM::DirWatcherDarwin watcher;
      QVERIFY(watcher.addPath(temp.path() + '/'));
      QVERIFY(watcher.addPath(temp.filePath("nested")));
      QVERIFY(watcher.addPath(temp.filePath("nested"), "shared.txt"));
      QVERIFY(watcher.addPath(temp.path())); // Replace, don't duplicate.
      QCOMPARE(watcher.nbWatchedPath(), 3);
      settle(watcher);
      watcher.rmPath(temp.path());
      watcher.rmPath(temp.filePath("nested"));
      QCOMPARE(watcher.nbWatchedPath(), 1);
      QVERIFY(writeFile(path, "still watched"));
      QVERIFY(detects(watcher, path, true));
      watcher.rmPath(temp.filePath("nested") + '/', "shared.txt");
      QCOMPARE(watcher.nbWatchedPath(), 0);
      QVERIFY(writeFile(path, "no longer watched"));
      QVERIFY(timeout(watcher.waitEvent(400)));
   }

   void rootAndAncestorMoves_data()
   {
      QTest::addColumn<bool>("ancestor");
      QTest::newRow("root") << false;
      QTest::newRow("ancestor") << true;
   }

   void rootAndAncestorMoves()
   {
      QFETCH(bool, ancestor);
      QTemporaryDir temp;
      QVERIFY(temp.isValid());
      QVERIFY(QDir().mkpath(temp.filePath("parent/root")));
      const QString root = temp.filePath("parent/root");
      FM::DirWatcherDarwin watcher;
      QVERIFY(watcher.addPath(root));
      settle(watcher);
      QVERIFY(QDir().rename(ancestor ? temp.filePath("parent") : root, temp.filePath("moved")));
      bool lost = false;
      QElapsedTimer timer;
      timer.start();
      while (!lost && timer.elapsed() < 5000)
         for (const auto& event : watcher.waitEvent(100))
            lost |= event.type == FM::WatcherEvent::WATCH_LOST && event.path1 == root;
      QVERIFY(lost);
      QCOMPARE(watcher.nbWatchedPath(), 0);
   }

   void aliasesUnicodeAndSymlinks()
   {
      QTemporaryDir temp("/tmp/dlan-watcher-XXXXXX");
      QVERIFY(temp.isValid());
      const QString root = temp.filePath(QString::fromUtf8("été"));
      QVERIFY(QDir().mkpath(root + "/nested"));
      FM::DirWatcherDarwin watcher;
      QVERIFY(watcher.addPath(root));
      settle(watcher);
      const QString path = root + QString::fromUtf8("/nested/日本語.txt");
      QVERIFY(writeFile(path));
      QVERIFY(detects(watcher, path));
      QTemporaryDir outside;
      QVERIFY(outside.isValid());
      QVERIFY(QFile::link(outside.path(), root + "/link"));
      QVERIFY(!watcher.addPath(root + "/link"));
      settle(watcher);
      QVERIFY(writeFile(outside.filePath("outside.txt")));
      QVERIFY(timeout(watcher.waitEvent(500)));
   }

   void droppedEventsRecoverWholeRoot_data()
   {
      QTest::addColumn<quint32>("flags");
      QTest::newRow("coalesced") << quint32(kFSEventStreamEventFlagMustScanSubDirs);
      QTest::newRow("kernel-overflow") << quint32(kFSEventStreamEventFlagKernelDropped);
      QTest::newRow("user-overflow") << quint32(kFSEventStreamEventFlagUserDropped);
      QTest::newRow("wrapped") << quint32(kFSEventStreamEventFlagEventIdsWrapped);
      QTest::newRow("unmounted") << quint32(kFSEventStreamEventFlagUnmount);
   }

   void droppedEventsRecoverWholeRoot()
   {
      QFETCH(quint32, flags);
      QTemporaryDir temp;
      QVERIFY(temp.isValid());
      FM::DirWatcherDarwin watcher;
      QVERIFY(watcher.addPath(temp.path()));
      settle(watcher);
      // Overflow/unmount aren't safe to induce on the host. Inject the native
      // callback and verify the public recovery behavior, including its wakeup.
      QByteArray native("/unrelated/coalesced/ancestor");
      char* paths[] = {native.data()};
      FSEventStreamEventId ids[] = {1};
      FM::DirWatcherDarwin::callback(nullptr, watcher.watches.at(temp.path()).get(), 1, paths, &flags, ids);
      const auto events = watcher.waitEvent(1000);
      QCOMPARE(events.size(), 1);
      QCOMPARE(events.first().type, flags == kFSEventStreamEventFlagUnmount ? FM::WatcherEvent::WATCH_LOST : FM::WatcherEvent::RESCAN);
      QCOMPARE(events.first().path1, temp.path());
      QVERIFY(!events.first().isWatchedFile);
   }

   void invalidPathsAndShutdown()
   {
      QTemporaryDir temp;
      QVERIFY(temp.isValid());
      FM::DirWatcherDarwin watcher;
      QVERIFY(!watcher.addPath(temp.filePath("missing")));
      QVERIFY(!watcher.addPath("relative"));
      for (int i = 0; i < 10; ++i)
      {
         FM::DirWatcherDarwin shortLived;
         QVERIFY(shortLived.addPath(temp.path()));
         QVERIFY(writeFile(temp.filePath("changing.txt"), QByteArray::number(i)));
         // Destruction must join callbacks without requiring a Qt event loop.
      }
   }
};

QTEST_GUILESS_MAIN(DirWatcherDarwinTests)
#include "DirWatcherDarwinTests.moc"
