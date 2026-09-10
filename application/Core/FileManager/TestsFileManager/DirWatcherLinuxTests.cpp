#include <QDir>
#include <QElapsedTimer>
#include <QFile>
#include <QSet>
#include <QScopeGuard>
#include <QTemporaryDir>
#include <QTest>

#include <priv/FileUpdater/DirWatcherLinux.h>
#include <priv/FileUpdater/WaitConditionLinux.h>

#include <memory>
#include <functional>
#include <vector>
#include <sys/resource.h>
#include <sys/syscall.h>
#include <linux/fs.h>
#include <fcntl.h>
#include <unistd.h>
#include <cstdio>
#include <cerrno>
#include <system_error>

namespace
{
   std::function<void(const QString&)> beforeAddWatch;
   int pipeCreationError = 0;
   int createdPipe[2] = {-1, -1};
}

extern "C" int __real_pipe2(int fds[2], int flags);
extern "C" int __wrap_pipe2(int fds[2], int flags)
{
   if (pipeCreationError)
   {
      errno = pipeCreationError;
      return -1;
   }
   const int result = __real_pipe2(fds, flags);
   if (result == 0)
   {
      createdPipe[0] = fds[0];
      createdPipe[1] = fds[1];
   }
   return result;
}

extern "C" int __real_inotify_add_watch(int fd, const char* path, uint32_t mask);
extern "C" int __wrap_inotify_add_watch(int fd, const char* path, uint32_t mask)
{
   if (beforeAddWatch)
      beforeAddWatch(QFile::decodeName(path));
   return __real_inotify_add_watch(fd, path, mask);
}

class DirWatcherLinuxTests : public QObject
{
   Q_OBJECT

private slots:
   void waitConditionPipeFailure_data()
   {
      QTest::addColumn<int>("error");
      QTest::newRow("process-descriptor-limit") << EMFILE;
      QTest::newRow("system-descriptor-limit") << ENFILE;
   }

   void waitConditionPipeFailure()
   {
      QFETCH(int, error);
      QFile unrelated("/dev/null");
      QVERIFY(unrelated.open(QIODevice::ReadOnly));
      const int fd = unrelated.handle();
      const int flags = fcntl(fd, F_GETFL);
      QVERIFY(flags >= 0);
      const auto descriptors = QDir("/proc/self/fd").entryList();
      const auto resetError = qScopeGuard([] { pipeCreationError = 0; });
      pipeCreationError = error;
      bool threw = false;
      try
      {
         std::unique_ptr<FM::WaitCondition> condition(FM::WaitCondition::getNewWaitCondition());
      }
      catch (const std::system_error& e)
      {
         threw = true;
         QCOMPARE(e.code(), std::error_code(error, std::generic_category()));
      }
      pipeCreationError = 0;
      QVERIFY(threw);
      QCOMPARE(fcntl(fd, F_GETFL), flags);
      QCOMPARE(QDir("/proc/self/fd").entryList(), descriptors);

      // A subsequent successful construction still owns and releases both ends.
      int readFd, writeFd;
      {
         FM::WaitConditionLinux condition;
         readFd = createdPipe[0];
         writeFd = createdPipe[1];
         QCOMPARE(condition.getFd(), readFd);
         for (int pipeFd : {readFd, writeFd})
         {
            QVERIFY(pipeFd >= 0);
            QVERIFY(fcntl(pipeFd, F_GETFL) & O_NONBLOCK);
            QVERIFY(fcntl(pipeFd, F_GETFD) & FD_CLOEXEC);
         }
         QVERIFY(condition.wait(0));
      }
      QCOMPARE(fcntl(readFd, F_GETFD), -1);
      QCOMPARE(errno, EBADF);
      QCOMPARE(fcntl(writeFd, F_GETFD), -1);
      QCOMPARE(errno, EBADF);
      QCOMPARE(QDir("/proc/self/fd").entryList(), descriptors);
   }

   void equivalentPathSpellingsShareRegistration_data()
   {
      QTest::addColumn<QString>("spelling");
      QTest::addColumn<bool>("watchFile");
      QTest::newRow("directory-trailing-slash") << QString("root/") << false;
      QTest::newRow("directory-dot") << QString("./root/.") << false;
      QTest::newRow("directory-parent") << QString("root/sub/..") << false;
      QTest::newRow("directory-repeated-slashes") << QString("root//") << false;
      QTest::newRow("file-trailing-slash") << QString("root/") << true;
      QTest::newRow("file-dot") << QString("./root/.") << true;
      QTest::newRow("file-parent") << QString("root/sub/..") << true;
      QTest::newRow("file-repeated-slashes") << QString("root//") << true;
   }

   void equivalentPathSpellingsShareRegistration()
   {
      QFETCH(QString, spelling);
      QFETCH(bool, watchFile);
      QTemporaryDir temp;
      QVERIFY(temp.isValid());
      QDir base(temp.path());
      QVERIFY(base.mkpath("root/sub"));
      const QString filePath = temp.filePath("root/file.txt");
      {
         QFile file(filePath);
         QVERIFY(file.open(QIODevice::WriteOnly));
      }
      const QString path = watchFile ? filePath : temp.filePath("root");
      const QString filename = watchFile ? QString("./file.txt") : QString();
      FM::DirWatcherLinux watcher;

      // Removing a single registration using its normalized spelling must work.
      QVERIFY(watcher.addPath(temp.filePath(spelling), filename));
      watcher.rmPath(path);
      QCOMPARE(watcher.nbWatchedPath(), 0);
      QVERIFY(watcher.watchReferences.isEmpty());
      QVERIFY(watcher.ancestorPaths.isEmpty());
      watcher.waitEvent(0);

      QVERIFY(watcher.addPath(path));
      const auto references = watcher.watchReferences;
      const auto ancestors = watcher.ancestorPaths;
      QVERIFY(watcher.addPath(temp.filePath(spelling), filename));
      QCOMPARE(watcher.nbWatchedPath(), 1);
      QCOMPARE(watcher.watchReferences, references);
      QCOMPARE(watcher.ancestorPaths, ancestors);
      {
         QFile file(filePath);
         QVERIFY(file.open(QIODevice::Append));
         QCOMPARE(file.write("changed"), qint64(7));
      }
      int changes = 0;
      for (const auto& event : watcher.waitEvent(1000))
         if (event.type == FM::WatcherEvent::CONTENT_CHANGED && event.path1 == filePath && event.isWatchedFile == watchFile)
            ++changes;
      QCOMPARE(changes, 1);

      // Removal must also accept the alternate spelling after the path vanishes.
      QVERIFY(base.rename("root", "moved"));
      watcher.rmPath(temp.filePath(spelling), filename);
      QCOMPARE(watcher.nbWatchedPath(), 0);
      QVERIFY(watcher.watchReferences.isEmpty());
      QVERIFY(watcher.ancestorPaths.isEmpty());
      {
         QFile file(temp.filePath("moved/file.txt"));
         QVERIFY(file.open(QIODevice::Append));
         QCOMPARE(file.write("after removal"), qint64(13));
      }
      for (const auto& event : watcher.waitEvent(0))
         QCOMPARE(event.type, FM::WatcherEvent::TIMEOUT);
   }

   void fileRegistrationReplacesDirectory_data()
   {
      QTest::addColumn<bool>("removeImmediately");
      QTest::newRow("remove-before-processing-events") << true;
      QTest::newRow("check-replacement-events") << false;
   }

   void fileRegistrationReplacesDirectory()
   {
      QFETCH(bool, removeImmediately);
      QTemporaryDir temp;
      QVERIFY(temp.isValid());
      QDir base(temp.path());
      QVERIFY(base.mkpath("root/sub"));
      const QString path = temp.filePath("root");
      FM::DirWatcherLinux watcher;
      QVERIFY(watcher.addPath(path));
      const auto references = watcher.watchReferences;
      const auto ancestors = watcher.ancestorPaths;
      QVERIFY(base.rename("root", "backup"));

      // A failed replacement must preserve the original registration.
      QVERIFY(!watcher.addPath(path));
      QCOMPARE(watcher.nbWatchedPath(), 1);
      QCOMPARE(watcher.watchReferences, references);
      QCOMPARE(watcher.ancestorPaths, ancestors);
      {
         QFile file(path);
         QVERIFY(file.open(QIODevice::WriteOnly));
      }
      QVERIFY(watcher.addPath(path));
      QCOMPARE(watcher.nbWatchedPath(), 1);
      QVERIFY(watcher.dirs.isEmpty());
      QVERIFY(watcher.files.contains(path));
      if (!removeImmediately)
      {
         // Old directory events must not retire the replacement file.
         for (const auto& event : watcher.waitEvent(0))
            QCOMPARE(event.type, FM::WatcherEvent::TIMEOUT);
         {
            QFile file(path);
            QVERIFY(file.open(QIODevice::Append));
            QCOMPARE(file.write("changed"), qint64(7));
         }
         bool changed = false;
         for (const auto& event : watcher.waitEvent(1000))
            changed |= event.type == FM::WatcherEvent::CONTENT_CHANGED && event.path1 == path && event.isWatchedFile;
         QVERIFY(changed);
      }
      watcher.rmPath(path);
      QCOMPARE(watcher.nbWatchedPath(), 0);
      QVERIFY(watcher.watchReferences.isEmpty());
      QVERIFY(watcher.ancestorPaths.isEmpty());
      {
         QFile file(path);
         QVERIFY(file.open(QIODevice::Append));
         QCOMPARE(file.write("after removal"), qint64(13));
         QFile oldChild(temp.filePath("backup/sub/later.txt"));
         QVERIFY(oldChild.open(QIODevice::WriteOnly));
      }
      for (const auto& event : watcher.waitEvent(0))
         QCOMPARE(event.type, FM::WatcherEvent::TIMEOUT);
   }

   void ancestorReplacementDuringRegistration_data()
   {
      QTest::addColumn<QString>("triggerPath");
      QTest::newRow("before-parent-watch") << QString("parent");
      QTest::newRow("after-parent-watch") << QString("parent/sub");
   }

   void ancestorReplacementDuringRegistration()
   {
      QFETCH(QString, triggerPath);
      QTemporaryDir temp;
      QVERIFY(temp.isValid());
      QDir base(temp.path());
      QVERIFY(base.mkpath("parent/sub/root"));
      const QString root = temp.filePath("parent/sub/root");
      FM::DirWatcherLinux watcher;
      bool triggered = false;
      bool replaced = false;
      const auto resetHook = qScopeGuard([] { beforeAddWatch = {}; });
      beforeAddWatch = [&](const QString& path)
      {
         if (!triggered && path == temp.filePath(triggerPath))
         {
            triggered = true;
            replaced = base.rename("parent", "outside") && base.mkpath("parent/sub/root");
         }
      };
      QVERIFY(watcher.addPath(root));
      beforeAddWatch = {};
      QVERIFY(triggered);
      QVERIFY(replaced);
      // A move after the parent watch was installed may request a rescan.
      for (const auto& event : watcher.waitEvent(0))
         QVERIFY(event.type == FM::WatcherEvent::RESCAN || event.type == FM::WatcherEvent::TIMEOUT);
      QCOMPARE(watcher.nbWatchedPath(), 1);
      {
         QFile file(root + "/new.txt");
         QVERIFY(file.open(QIODevice::WriteOnly));
      }
      bool found = false;
      for (const auto& event : watcher.waitEvent(1000))
         found |= event.type == FM::WatcherEvent::NEW && event.path1 == root + "/new.txt";
      QVERIFY(found);

      // The immediate ancestor must belong to the replacement tree. Otherwise
      // this move is missed and subsequent events retain the obsolete root path.
      QVERIFY(base.rename("parent/sub", "moved-sub"));
      bool deleted = false;
      for (const auto& event : watcher.waitEvent(1000))
         deleted |= event.type == FM::WatcherEvent::DELETED && event.path1 == root;
      QVERIFY(deleted);
      QCOMPARE(watcher.nbWatchedPath(), 0);
      QVERIFY(watcher.watchReferences.isEmpty());
      QVERIFY(watcher.ancestorPaths.isEmpty());
      {
         QFile file(temp.filePath("moved-sub/root/later.txt"));
         QVERIFY(file.open(QIODevice::WriteOnly));
         QCOMPARE(file.write("changed"), qint64(7));
      }
      for (const auto& event : watcher.waitEvent(0))
         QCOMPARE(event.type, FM::WatcherEvent::TIMEOUT);
   }

   void duplicateDirectoryRegistrationReleasesOldTree_data()
   {
      QTest::addColumn<bool>("watchChild");
      QTest::addColumn<bool>("replaceDirectory");
      QTest::addColumn<bool>("registrationFails");
      QTest::newRow("same-directory") << false << false << false;
      QTest::newRow("shared-descendant") << true << false << false;
      QTest::newRow("replacement-directory") << false << true << false;
      QTest::newRow("failed-registration") << false << false << true;
   }

   void duplicateDirectoryRegistrationReleasesOldTree()
   {
      QFETCH(bool, watchChild);
      QFETCH(bool, replaceDirectory);
      QFETCH(bool, registrationFails);
      if (registrationFails && ::geteuid() == 0)
         QSKIP("Root can watch unreadable directories");
      QTemporaryDir temp;
      QVERIFY(temp.isValid());
      QDir base(temp.path());
      QVERIFY(base.mkpath("root/sub"));
      const QString root = temp.filePath("root");
      const QString child = temp.filePath("root/sub");
      FM::DirWatcherLinux watcher;
      QVERIFY(watcher.addPath(root));
      if (watchChild)
         QVERIFY(watcher.addPath(child));
      const auto references = watcher.watchReferences;
      const auto ancestors = watcher.ancestorPaths;
      if (replaceDirectory)
      {
         QVERIFY(base.rename("root", "old"));
         QVERIFY(base.mkpath("root/sub"));
      }
      const auto permissions = QFile::permissions(child);
      const auto restorePermissions = qScopeGuard([&] { QFile::setPermissions(child, permissions); });
      if (registrationFails)
         QVERIFY(QFile::setPermissions(child, QFileDevice::WriteOwner | QFileDevice::ExeOwner));
      for (int i = 0; i < 2; ++i)
         QCOMPARE(watcher.addPath(root), !registrationFails);
      QCOMPARE(watcher.nbWatchedPath(), watchChild ? 2 : 1);
      if (!replaceDirectory)
      {
         QCOMPARE(watcher.watchReferences, references);
         QCOMPARE(watcher.ancestorPaths, ancestors);
      }
      QVERIFY(QFile::setPermissions(child, permissions));
      watcher.waitEvent(0);
      const QString filePath = child + "/new.txt";
      {
         QFile file(filePath);
         QVERIFY(file.open(QIODevice::WriteOnly));
      }
      bool found = false;
      for (const auto& event : watcher.waitEvent(1000))
      {
         QVERIFY(event.type != FM::WatcherEvent::WATCH_LOST);
         found |= event.type == FM::WatcherEvent::NEW && event.path1 == filePath;
      }
      QVERIFY(found);
      watcher.rmPath(root);
      QCOMPARE(watcher.nbWatchedPath(), watchChild ? 1 : 0);
      watcher.waitEvent(0);
      {
         QFile file(filePath);
         QVERIFY(file.open(QIODevice::Append));
         QCOMPARE(file.write("changed"), qint64(7));
      }
      bool changed = false;
      for (const auto& event : watcher.waitEvent(0))
      {
         QVERIFY(event.type == FM::WatcherEvent::CONTENT_CHANGED || event.type == FM::WatcherEvent::TIMEOUT);
         changed |= event.type == FM::WatcherEvent::CONTENT_CHANGED && event.path1 == filePath;
      }
      QCOMPARE(changed, watchChild);
      watcher.rmPath(child);
      QVERIFY(watcher.watchReferences.isEmpty());
      QVERIFY(watcher.ancestorPaths.isEmpty());
   }

   void ancestorWatchLossRequestsFallback()
   {
      QTemporaryDir temp;
      QVERIFY(temp.isValid());
      QDir base(temp.path());
      QVERIFY(base.mkpath("parent/root"));
      QVERIFY(base.mkdir("other"));
      const QString root = temp.filePath("parent/root");
      const QString filePath = temp.filePath("parent/file.txt");
      const QString other = temp.filePath("other");
      {
         QFile file(filePath);
         QVERIFY(file.open(QIODevice::WriteOnly));
      }
      FM::DirWatcherLinux watcher;
      QVERIFY(watcher.addPath(root));
      QVERIFY(watcher.addPath(filePath));
      QVERIFY(watcher.addPath(other));
      const int wd = watcher.dirs.first()->ancestors.first().wd;
      QVERIFY(watcher.getDirs(wd).isEmpty()); // An ancestor-only watch.
      QVERIFY(::inotify_rm_watch(watcher.fileDescriptor, wd) == 0);
      QSet<QString> lost;
      for (const auto& event : watcher.waitEvent(1000))
      {
         QCOMPARE(event.type, FM::WatcherEvent::WATCH_LOST);
         QCOMPARE(event.isWatchedFile, event.path1 == filePath);
         lost.insert(event.path1);
      }
      QCOMPARE(lost, (QSet<QString>{root, filePath}));
      QCOMPARE(watcher.nbWatchedPath(), 1);
      watcher.rmPath(other);
      QVERIFY(watcher.watchReferences.isEmpty());
      QVERIFY(watcher.ancestorPaths.isEmpty());
   }

   void ancestorWatchFailureRollsBack_data()
   {
      QTest::addColumn<bool>("file");
      QTest::newRow("directory") << false;
      QTest::newRow("file") << true;
   }

   void ancestorWatchFailureRollsBack()
   {
      if (::geteuid() == 0)
         QSKIP("Root can watch unreadable ancestors");
      QFETCH(bool, file);
      QTemporaryDir temp;
      QVERIFY(temp.isValid());
      QDir base(temp.path());
      QVERIFY(base.mkpath("private/sub/root"));
      QVERIFY(base.mkdir("other"));
      const QString filePath = temp.filePath("private/sub/file.txt");
      {
         QFile file(filePath);
         QVERIFY(file.open(QIODevice::WriteOnly));
      }
      FM::DirWatcherLinux watcher;
      QVERIFY(watcher.addPath(temp.filePath("other")));
      const auto references = watcher.watchReferences;
      const auto ancestors = watcher.ancestorPaths;
      const QString denied = temp.filePath("private");
      const auto permissions = QFile::permissions(denied);
      const auto restorePermissions = qScopeGuard([&] { QFile::setPermissions(denied, permissions); });
      QVERIFY(QFile::setPermissions(denied, QFileDevice::WriteOwner | QFileDevice::ExeOwner));
      QVERIFY(!watcher.addPath(file ? filePath : temp.filePath("private/sub/root")));
      QCOMPARE(watcher.nbWatchedPath(), 1);
      QCOMPARE(watcher.watchReferences, references);
      QCOMPARE(watcher.ancestorPaths, ancestors);
   }

   void unwatchedAncestorMovesInvalidateRegistrations_data()
   {
      QTest::addColumn<bool>("grandparent");
      QTest::addColumn<bool>("replacement");
      for (bool grandparent : {false, true})
         for (bool replacement : {false, true})
            QTest::newRow(qPrintable(QString("grandparent=%1,replacement=%2").arg(grandparent).arg(replacement)))
               << grandparent << replacement;
   }

   void unwatchedAncestorMovesInvalidateRegistrations()
   {
      QFETCH(bool, grandparent);
      QFETCH(bool, replacement);
      QTemporaryDir temp;
      QVERIFY(temp.isValid());
      QDir base(temp.path());
      QVERIFY(base.mkpath("parent/nested/root/deep"));
      QVERIFY(base.mkpath("parent-other"));
      const QString root = temp.filePath("parent/nested/root");
      const QString filePath = temp.filePath("parent/nested/file.txt");
      const QString sibling = temp.filePath("parent-other");
      {
         QFile file(filePath);
         QVERIFY(file.open(QIODevice::WriteOnly));
      }
      FM::DirWatcherLinux watcher;
      QVERIFY(watcher.addPath(root));
      QVERIFY(watcher.addPath(filePath));
      QVERIFY(watcher.addPath(sibling));
      QVERIFY(base.rename(grandparent ? "parent" : "parent/nested", "outside"));
      if (replacement)
      {
         QVERIFY(base.mkpath("parent/nested/root/deep"));
         QFile file(filePath);
         QVERIFY(file.open(QIODevice::WriteOnly));
      }
      // No descendant activity is needed to wake the watcher after a parent move.
      QSet<QString> invalidated;
      for (const auto& event : watcher.waitEvent(1000))
      {
         QCOMPARE(event.type, replacement ? FM::WatcherEvent::RESCAN : FM::WatcherEvent::DELETED);
         QCOMPARE(event.isWatchedFile, event.path1 == filePath);
         QVERIFY(!invalidated.contains(event.path1));
         invalidated.insert(event.path1);
      }
      QCOMPARE(invalidated, (QSet<QString>{root, filePath}));
      QCOMPARE(watcher.nbWatchedPath(), replacement ? 3 : 1);
      watcher.waitEvent(0);
      const QString oldBase = temp.filePath(grandparent ? "outside/nested" : "outside");
      for (const auto& path : QStringList{oldBase + "/root/deep/old.txt", oldBase + "/file.txt"})
      {
         QFile file(path);
         QVERIFY(file.open(QIODevice::Append));
         QCOMPARE(file.write("old"), qint64(3));
      }
      for (const auto& event : watcher.waitEvent(0))
         QCOMPARE(event.type, FM::WatcherEvent::TIMEOUT);
      const QString directoryFile = root + "/deep/new.txt";
      const QString siblingFile = sibling + "/new.txt";
      QStringList paths{siblingFile};
      if (replacement)
         paths << directoryFile << filePath;
      for (const auto& path : paths)
      {
         QFile file(path);
         QVERIFY(file.open(QIODevice::Append));
         QCOMPARE(file.write("new"), qint64(3));
      }
      QSet<QString> notified;
      for (const auto& event : watcher.waitEvent(1000))
      {
         QVERIFY(paths.contains(event.path1));
         notified.insert(event.path1);
      }
      QCOMPARE(notified, QSet<QString>(paths.begin(), paths.end()));
      watcher.rmPath(root);
      watcher.rmPath(filePath);
      watcher.rmPath(sibling);
      QVERIFY(watcher.watchReferences.isEmpty());
   }

   void conflictingDirectoryMovesRebuildWatches_data()
   {
      QTest::addColumn<bool>("exchange");
      QTest::addColumn<bool>("crossParent");
      QTest::addColumn<bool>("splitRead");
      QTest::addColumn<bool>("newDestination");
      QTest::newRow("overwrite") << false << false << false << false;
      QTest::newRow("overwrite-across-parents") << false << true << false << false;
      QTest::newRow("exchange") << true << false << false << false;
      QTest::newRow("exchange-across-parents") << true << true << false << false;
      QTest::newRow("exchange-across-reads") << true << false << true << false;
      QTest::newRow("destination-created-in-batch") << false << false << false << true;
   }

   void conflictingDirectoryMovesRebuildWatches()
   {
      QFETCH(bool, exchange);
      QFETCH(bool, crossParent);
      QFETCH(bool, splitRead);
      QFETCH(bool, newDestination);
      QTemporaryDir temp;
      QVERIFY(temp.isValid());
      QDir base(temp.path());
      QVERIFY(base.mkpath("root/left/source/deep"));
      QVERIFY(base.mkpath("root/right"));
      const QString root = temp.filePath("root");
      const QString left = temp.filePath("root/left");
      const QString right = temp.filePath("root/right");
      const QString source = left + "/source";
      const QString destination = (crossParent ? right : left) + "/destination";
      if (!newDestination)
         QVERIFY(base.mkpath(destination + (exchange ? "/other" : "")));
      FM::DirWatcherLinux watcher;
      QVERIFY(watcher.addPath(root));
      QVERIFY(watcher.addPath(left));
      QVERIFY(watcher.addPath(right));
      if (newDestination)
         QVERIFY(base.mkpath(destination));
      const QByteArray from = QFile::encodeName(source);
      const QByteArray to = QFile::encodeName(destination);
      if (exchange)
         QVERIFY(::syscall(SYS_renameat2, AT_FDCWD, from.constData(), AT_FDCWD, to.constData(), RENAME_EXCHANGE) == 0);
      else
         QVERIFY(::rename(from.constData(), to.constData()) == 0);
      if (splitRead)
      {
         // Read only IN_MOVED_FROM, leaving its destination in the kernel queue.
         alignas(inotify_event) char buf[sizeof(inotify_event) + 16];
         const int len = ::read(watcher.fileDescriptor, buf, sizeof(buf));
         QCOMPARE(len, int(sizeof(buf)));
         QVERIFY(reinterpret_cast<const inotify_event*>(buf)->mask & IN_MOVED_FROM);
         watcher.processInotifyEvents(buf, len);
      }
      QSet<QString> rescanned;
      for (const auto& event : watcher.waitEvent(1000))
      {
         // An exchange must not be applied to the cache as two ordinary moves.
         QCOMPARE(event.type, FM::WatcherEvent::RESCAN);
         QVERIFY(!event.isWatchedFile);
         QVERIFY(!rescanned.contains(event.path1));
         rescanned.insert(event.path1);
      }
      QCOMPARE(rescanned, (QSet<QString>{root, left, right}));
      QCOMPARE(watcher.nbWatchedPath(), 3);
      // The outer root must own the complete rebuilt trees independently.
      watcher.rmPath(left);
      watcher.rmPath(right);
      watcher.waitEvent(0);
      QStringList paths{destination + "/deep/first.txt"};
      if (exchange)
         paths << source + "/other/second.txt";
      for (const auto& path : paths)
      {
         QFile file(path);
         QVERIFY(file.open(QIODevice::WriteOnly));
      }
      QSet<QString> created;
      for (const auto& event : watcher.waitEvent(1000))
      {
         QVERIFY(paths.contains(event.path1));
         if (event.type == FM::WatcherEvent::NEW)
            created.insert(event.path1);
      }
      QCOMPARE(created, QSet<QString>(paths.begin(), paths.end()));
      watcher.rmPath(root);
      QCOMPARE(watcher.nbWatchedPath(), 0);
      QVERIFY(watcher.watchReferences.isEmpty());
   }

   void replacedChildPreservesParentCoverage_data()
   {
      QTest::addColumn<bool>("childFirst");
      QTest::addColumn<bool>("recreate");
      QTest::addColumn<bool>("watchFailure");
      QTest::newRow("atomic-parent-first") << false << false << false;
      QTest::newRow("atomic-child-first") << true << false << false;
      QTest::newRow("recreate-parent-first") << false << true << false;
      QTest::newRow("recreate-child-first") << true << true << false;
      QTest::newRow("replacement-watch-failure") << false << false << true;
   }

   void replacedChildPreservesParentCoverage()
   {
      QFETCH(bool, childFirst);
      QFETCH(bool, recreate);
      QFETCH(bool, watchFailure);
      if (watchFailure && ::geteuid() == 0)
         QSKIP("Root can watch unreadable directories");
      QTemporaryDir temp;
      QVERIFY(temp.isValid());
      QDir base(temp.path());
      QVERIFY(base.mkpath("root/parent/child"));
      QVERIFY(base.mkpath("incoming/deep"));
      const QString root = temp.filePath("root");
      const QString parent = temp.filePath("root/parent");
      const QString child = temp.filePath("root/parent/child");
      const auto permissions = QFile::permissions(temp.filePath("incoming/deep"));
      const auto restorePermissions = qScopeGuard([&]
      {
         QFile::setPermissions(temp.filePath("incoming/deep"), permissions);
         QFile::setPermissions(child + "/deep", permissions);
      });
      FM::DirWatcherLinux watcher;
      QVERIFY(watcher.addPath(childFirst ? child : root));
      QVERIFY(watcher.addPath(parent));
      QVERIFY(watcher.addPath(childFirst ? root : child));
      if (watchFailure)
         QVERIFY(QFile::setPermissions(temp.filePath("incoming/deep"), QFileDevice::WriteOwner | QFileDevice::ExeOwner));
      if (recreate)
         QVERIFY(base.rmdir("root/parent/child"));
      QVERIFY(::rename(QFile::encodeName(temp.filePath("incoming")).constData(), QFile::encodeName(child).constData()) == 0);
      QSet<QString> lost;
      QSet<QString> rescanned;
      for (const auto& event : watcher.waitEvent(1000))
      {
         if (event.type == FM::WatcherEvent::WATCH_LOST)
         {
            QVERIFY(!lost.contains(event.path1));
            lost.insert(event.path1);
         }
         if (event.type == FM::WatcherEvent::RESCAN)
            rescanned.insert(event.path1);
      }
      if (watchFailure)
      {
         QCOMPARE(lost, (QSet<QString>{root, parent, child}));
         QCOMPARE(watcher.nbWatchedPath(), 0);
         QVERIFY(watcher.watchReferences.isEmpty());
         return;
      }
      QVERIFY(lost.isEmpty());
      QVERIFY(rescanned.contains(child));
      if (!recreate)
         QCOMPARE(rescanned, (QSet<QString>{root, parent, child}));
      QCOMPARE(watcher.nbWatchedPath(), 3);
      // Each containing root must retain a complete tree after the more
      // specific registrations are removed, even with pending IN_IGNORED events.
      for (const QString& removed : {child, parent})
      {
         watcher.rmPath(removed);
         watcher.waitEvent(0);
         const QString filePath = child + "/deep/" + (removed == child ? "first.txt" : "second.txt");
         {
            QFile file(filePath);
            QVERIFY(file.open(QIODevice::WriteOnly));
         }
         bool found = false;
         for (const auto& event : watcher.waitEvent(1000))
         {
            QVERIFY(event.type != FM::WatcherEvent::WATCH_LOST);
            found |= event.type == FM::WatcherEvent::NEW && event.path1 == filePath;
         }
         QVERIFY(found);
      }
      watcher.rmPath(root);
      QCOMPARE(watcher.nbWatchedPath(), 0);
      QVERIFY(watcher.watchReferences.isEmpty());
   }

   void duplicateFileRegistrationReleasesOldReference_data()
   {
      QTest::addColumn<bool>("replaceFile");
      QTest::newRow("same-inode") << false;
      QTest::newRow("replacement-inode") << true;
   }

   void duplicateFileRegistrationReleasesOldReference()
   {
      QFETCH(bool, replaceFile);
      QTemporaryDir temp;
      QVERIFY(temp.isValid());
      const QString path = temp.filePath("file.txt");
      const QString alias = temp.filePath("alias.txt");
      {
         QFile file(path);
         QVERIFY(file.open(QIODevice::WriteOnly));
      }
      QVERIFY(::link(QFile::encodeName(path).constData(), QFile::encodeName(alias).constData()) == 0);
      FM::DirWatcherLinux watcher;
      QVERIFY(watcher.addPath(path));
      QVERIFY(watcher.addPath(alias));
      if (replaceFile)
      {
         const QString replacement = temp.filePath("replacement.txt");
         {
            QFile file(replacement);
            QVERIFY(file.open(QIODevice::WriteOnly));
         }
         QVERIFY(::rename(QFile::encodeName(replacement).constData(), QFile::encodeName(path).constData()) == 0);
      }
      // Both forms of addPath identify the same registration.
      QVERIFY(watcher.addPath(temp.path(), "file.txt"));
      QVERIFY(watcher.addPath(path));
      QCOMPARE(watcher.nbWatchedPath(), 2);
      const int wd = watcher.files.value(path)->wd;
      const int aliasWd = watcher.files.value(alias)->wd;
      QCOMPARE(watcher.watchReferences.value(wd), replaceFile ? 1 : 2);
      QCOMPARE(watcher.watchReferences.value(aliasWd), replaceFile ? 1 : 2);
      watcher.waitEvent(0); // Drain any attribute event on the old inode.
      {
         QFile file(path);
         QVERIFY(file.open(QIODevice::Append));
         QCOMPARE(file.write("changed"), qint64(7));
      }
      QSet<QString> changed;
      for (const auto& event : watcher.waitEvent(1000))
      {
         QCOMPARE(event.type, FM::WatcherEvent::CONTENT_CHANGED);
         QVERIFY(event.isWatchedFile);
         QVERIFY(!changed.contains(event.path1));
         changed.insert(event.path1);
      }
      QCOMPARE(changed, replaceFile ? QSet<QString>{path} : (QSet<QString>{path, alias}));
      watcher.rmPath(path);
      QCOMPARE(watcher.nbWatchedPath(), 1);
      QCOMPARE(watcher.watchReferences.size(), 1 + watcher.files.value(alias)->ancestors.size());
      QCOMPARE(watcher.watchReferences.value(aliasWd), 1);
      watcher.waitEvent(0);
      {
         QFile file(alias);
         QVERIFY(file.open(QIODevice::Append));
         QCOMPARE(file.write("later"), qint64(5));
      }
      const auto events = watcher.waitEvent(1000);
      QCOMPARE(events.size(), 1);
      QCOMPARE(events[0].type, FM::WatcherEvent::CONTENT_CHANGED);
      QCOMPARE(events[0].path1, alias);
      watcher.rmPath(alias);
      QCOMPARE(watcher.nbWatchedPath(), 0);
      QVERIFY(watcher.watchReferences.isEmpty());
   }

   void hardLinkChangesNotifyEveryRegistration()
   {
      QTemporaryDir temp;
      QVERIFY(temp.isValid());
      const QString first = temp.filePath("first.txt");
      const QString second = temp.filePath("second.txt");
      {
         QFile file(first);
         QVERIFY(file.open(QIODevice::WriteOnly));
      }
      QVERIFY(::link(QFile::encodeName(first).constData(), QFile::encodeName(second).constData()) == 0);
      FM::DirWatcherLinux watcher;
      QVERIFY(watcher.addPath(first));
      QVERIFY(watcher.addPath(second));
      for (const QString& path : {first, second})
      {
         {
            QFile file(path);
            QVERIFY(file.open(QIODevice::Append));
            QCOMPARE(file.write("changed"), qint64(7));
         }
         QSet<QString> changed;
         for (const auto& event : watcher.waitEvent(1000))
         {
            QCOMPARE(event.type, FM::WatcherEvent::CONTENT_CHANGED);
            QVERIFY(event.isWatchedFile);
            QVERIFY(!changed.contains(event.path1));
            changed.insert(event.path1);
         }
         QCOMPARE(changed, (QSet<QString>{first, second}));
      }
      // Releasing one registration must retain the other owner's kernel watch.
      watcher.rmPath(first);
      {
         QFile file(first);
         QVERIFY(file.open(QIODevice::Append));
         QCOMPARE(file.write("later"), qint64(5));
      }
      const auto events = watcher.waitEvent(1000);
      QCOMPARE(events.size(), 1);
      QCOMPARE(events[0].type, FM::WatcherEvent::CONTENT_CHANGED);
      QCOMPARE(events[0].path1, second);
      watcher.rmPath(second);
      QVERIFY(watcher.watchReferences.isEmpty());
   }

   void hardLinkPathChangesAreIndependent_data()
   {
      QTest::addColumn<QString>("operation");
      QTest::addColumn<bool>("changeFirst");
      for (const QString& operation : {QString("replace"), QString("rename"), QString("delete")})
         for (bool changeFirst : {false, true})
            QTest::newRow(qPrintable(QString("%1,first=%2").arg(operation).arg(changeFirst))) << operation << changeFirst;
   }

   void hardLinkPathChangesAreIndependent()
   {
      QFETCH(QString, operation);
      QFETCH(bool, changeFirst);
      QTemporaryDir temp;
      QVERIFY(temp.isValid());
      const QString first = temp.filePath("first.txt");
      const QString second = temp.filePath("second.txt");
      const QString changedPath = changeFirst ? first : second;
      const QString unchangedPath = changeFirst ? second : first;
      {
         QFile file(first);
         QVERIFY(file.open(QIODevice::WriteOnly));
      }
      QVERIFY(::link(QFile::encodeName(first).constData(), QFile::encodeName(second).constData()) == 0);
      FM::DirWatcherLinux watcher;
      QVERIFY(watcher.addPath(first));
      QVERIFY(watcher.addPath(second));
      if (operation == "replace")
      {
         const QString replacementPath = temp.filePath("replacement.txt");
         {
            QFile file(replacementPath);
            QVERIFY(file.open(QIODevice::WriteOnly));
         }
         QVERIFY(::rename(QFile::encodeName(replacementPath).constData(), QFile::encodeName(changedPath).constData()) == 0);
      }
      else if (operation == "rename")
         QVERIFY(QFile::rename(changedPath, temp.filePath("renamed.txt")));
      else
         QVERIFY(QFile::remove(changedPath));
      const auto events = watcher.waitEvent(1000);
      QCOMPARE(events.size(), 1);
      QCOMPARE(events[0].type, operation == "replace" ? FM::WatcherEvent::RESCAN : FM::WatcherEvent::DELETED);
      QCOMPARE(events[0].path1, changedPath);
      QVERIFY(events[0].isWatchedFile);
      QCOMPARE(watcher.nbWatchedPath(), operation == "replace" ? 2 : 1);
      // The remaining hard link still watches the original inode. Replacements
      // must separately watch their new inode, without cross-path notifications.
      for (const QString& path : operation == "replace" ? QStringList{unchangedPath, changedPath} : QStringList{unchangedPath})
      {
         {
            QFile file(path);
            QVERIFY(file.open(QIODevice::Append));
            QCOMPARE(file.write("changed"), qint64(7));
         }
         const auto changes = watcher.waitEvent(1000);
         QCOMPARE(changes.size(), 1);
         QCOMPARE(changes[0].type, FM::WatcherEvent::CONTENT_CHANGED);
         QCOMPARE(changes[0].path1, path);
         QVERIFY(changes[0].isWatchedFile);
      }
      watcher.rmPath(first);
      watcher.rmPath(second);
      QVERIFY(watcher.watchReferences.isEmpty());
   }

   void movedNonRootAncestorRetiresDescendantRegistrations_data()
   {
      QTest::addColumn<bool>("moveOutside");
      QTest::addColumn<bool>("childFirst");
      for (bool moveOutside : {false, true})
         for (bool childFirst : {false, true})
            QTest::newRow(qPrintable(QString("outside=%1,child-first=%2").arg(moveOutside).arg(childFirst)))
               << moveOutside << childFirst;
   }

   void movedNonRootAncestorRetiresDescendantRegistrations()
   {
      QFETCH(bool, moveOutside);
      QFETCH(bool, childFirst);
      QTemporaryDir temp;
      QVERIFY(temp.isValid());
      QDir base(temp.path());
      QVERIFY(base.mkpath("root/ancestor/child/deep"));
      QVERIFY(base.mkpath("root/ancestor-other"));
      const QString root = temp.filePath("root");
      const QString ancestor = temp.filePath("root/ancestor");
      const QString child = ancestor + "/child";
      const QString deep = child + "/deep";
      const QString filePath = deep + "/file.txt";
      const QString sibling = temp.filePath("root/ancestor-other");
      const QString destination = temp.filePath(moveOutside ? "outside" : "root/renamed");
      {
         QFile file(filePath);
         QVERIFY(file.open(QIODevice::WriteOnly));
      }
      FM::DirWatcherLinux watcher;
      QVERIFY(watcher.addPath(childFirst ? child : root));
      QVERIFY(watcher.addPath(childFirst ? root : child));
      QVERIFY(watcher.addPath(deep));
      QVERIFY(watcher.addPath(filePath));
      QVERIFY(watcher.addPath(sibling));
      QVERIFY(base.rename(ancestor, destination));
      const QString movedFile = destination + "/child/deep/file.txt";
      {
         // These events share a read with the ancestor move and must never
         // resolve through the descendants' obsolete registrations.
         QFile file(movedFile);
         QVERIFY(file.open(QIODevice::Append));
         QCOMPARE(file.write("changed"), qint64(7));
      }
      QSet<QString> deleted;
      bool foundMove = false;
      for (const auto& event : watcher.waitEvent(1000))
      {
         if (event.type == FM::WatcherEvent::DELETED)
         {
            QVERIFY(!deleted.contains(event.path1));
            QCOMPARE(event.isWatchedFile, event.path1 == filePath);
            deleted.insert(event.path1);
         }
         else if (event.type == FM::WatcherEvent::MOVE)
         {
            QCOMPARE(event.path1, ancestor);
            QCOMPARE(event.path2, destination);
            foundMove = true;
         }
         else
         {
            QVERIFY(!moveOutside);
            QCOMPARE(event.type, FM::WatcherEvent::CONTENT_CHANGED);
            QCOMPARE(event.path1, movedFile);
            QVERIFY(!event.isWatchedFile);
         }
      }
      QSet<QString> expected{child, deep, filePath};
      if (moveOutside)
         expected.insert(ancestor);
      QCOMPARE(deleted, expected);
      QCOMPARE(foundMove, !moveOutside);
      QCOMPARE(watcher.nbWatchedPath(), 2);
      watcher.waitEvent(0);
      const QString laterPath = destination + "/child/deep/later.txt";
      const QString siblingPath = sibling + "/later.txt";
      for (const auto& path : {laterPath, siblingPath})
      {
         QFile file(path);
         QVERIFY(file.open(QIODevice::WriteOnly));
      }
      bool foundLater = false;
      bool foundSibling = false;
      for (const auto& event : watcher.waitEvent(1000))
      {
         QVERIFY(!event.path1.startsWith(ancestor + '/'));
         foundLater |= event.type == FM::WatcherEvent::NEW && event.path1 == laterPath;
         foundSibling |= event.type == FM::WatcherEvent::NEW && event.path1 == siblingPath;
      }
      QCOMPARE(foundLater, !moveOutside);
      QVERIFY(foundSibling);
      watcher.rmPath(root);
      watcher.rmPath(sibling);
      QVERIFY(watcher.watchReferences.isEmpty());
   }

   void replacedDirectoriesRemainWatched_data()
   {
      QTest::addColumn<QString>("operation");
      QTest::addColumn<bool>("trailingSlash");
      QTest::newRow("atomic-replace") << "atomic" << false;
      QTest::newRow("delete-and-recreate") << "recreate" << false;
      QTest::newRow("rename-to-backup") << "backup" << false;
      QTest::newRow("trailing-slash") << "atomic" << true;
   }

   void replacedDirectoriesRemainWatched()
   {
      QFETCH(QString, operation);
      QFETCH(bool, trailingSlash);
      QTemporaryDir temp;
      QVERIFY(temp.isValid());
      QDir base(temp.path());
      QVERIFY(base.mkdir("root"));
      QVERIFY(base.mkpath("incoming/deep"));
      const QString path = temp.filePath("root");
      const QString registration = path + (trailingSlash ? "/" : "");
      FM::DirWatcherLinux watcher;
      QVERIFY(watcher.addPath(registration));
      if (operation == "recreate")
         QVERIFY(base.rmdir("root"));
      else if (operation == "backup")
         QVERIFY(base.rename("root", "backup"));
      QVERIFY(::rename(QFile::encodeName(temp.filePath("incoming")).constData(), QFile::encodeName(path).constData()) == 0);
      const auto events = watcher.waitEvent(1000);
      QCOMPARE(events.size(), 1);
      QCOMPARE(events[0].type, FM::WatcherEvent::RESCAN);
      QCOMPARE(events[0].path1, path);
      QVERIFY(!events[0].isWatchedFile);
      QCOMPARE(watcher.nbWatchedPath(), 1);
      // Old IN_IGNORED events must not invalidate the replacement watch.
      for (const auto& event : watcher.waitEvent(0))
         QCOMPARE(event.type, FM::WatcherEvent::TIMEOUT);
      if (operation == "backup")
      {
         QFile oldFile(temp.filePath("backup/old.txt"));
         QVERIFY(oldFile.open(QIODevice::WriteOnly));
      }
      const QString filePath = temp.filePath("root/deep/later.txt");
      {
         QFile file(filePath);
         QVERIFY(file.open(QIODevice::WriteOnly));
      }
      bool found = false;
      for (const auto& event : watcher.waitEvent(1000))
      {
         QVERIFY(!event.path1.endsWith("old.txt"));
         found |= event.type == FM::WatcherEvent::NEW && event.path1 == filePath;
      }
      QVERIFY(found);
      watcher.rmPath(registration);
      QCOMPARE(watcher.nbWatchedPath(), 0);
      QVERIFY(watcher.watchReferences.isEmpty());
   }

   void directoryReplacementRestoresDescendantRegistrations()
   {
      QTemporaryDir temp;
      QVERIFY(temp.isValid());
      QDir base(temp.path());
      QVERIFY(base.mkpath("root/sub"));
      QVERIFY(base.mkpath("incoming/sub"));
      for (const auto& name : {"root/sub/file.txt", "incoming/sub/file.txt"})
      {
         QFile file(temp.filePath(name));
         QVERIFY(file.open(QIODevice::WriteOnly));
      }
      const QString root = temp.filePath("root");
      const QString sub = temp.filePath("root/sub");
      const QString filePath = temp.filePath("root/sub/file.txt");
      FM::DirWatcherLinux watcher;
      QVERIFY(watcher.addPath(root));
      QVERIFY(watcher.addPath(sub));
      QVERIFY(watcher.addPath(filePath));
      QVERIFY(base.rename("root", "backup"));
      QVERIFY(base.rename("incoming", "root"));
      QSet<QString> rescanned;
      for (const auto& event : watcher.waitEvent(1000))
      {
         QCOMPARE(event.type, FM::WatcherEvent::RESCAN);
         QCOMPARE(event.isWatchedFile, event.path1 == filePath);
         QVERIFY(!rescanned.contains(event.path1));
         rescanned.insert(event.path1);
      }
      QCOMPARE(rescanned, (QSet<QString>{root, sub, filePath}));
      // Removing the restored ancestor must preserve the descendant watches.
      watcher.rmPath(root);
      QCOMPARE(watcher.nbWatchedPath(), 2);
      watcher.waitEvent(0);
      {
         QFile file(filePath);
         QVERIFY(file.open(QIODevice::Append));
         QCOMPARE(file.write("changed"), qint64(7));
      }
      bool fileChanged = false;
      bool directoryChanged = false;
      for (const auto& event : watcher.waitEvent(1000))
         if (event.type == FM::WatcherEvent::CONTENT_CHANGED && event.path1 == filePath)
         {
            fileChanged |= event.isWatchedFile;
            directoryChanged |= !event.isWatchedFile;
         }
      QVERIFY(fileChanged);
      QVERIFY(directoryChanged);
   }

   void replacementDirectoryWatchFailureRequestsFallback()
   {
      if (::geteuid() == 0)
         QSKIP("Root can watch unreadable directories");
      QTemporaryDir temp;
      QVERIFY(temp.isValid());
      QDir base(temp.path());
      QVERIFY(base.mkdir("root"));
      QVERIFY(base.mkpath("incoming/blocked"));
      const QString path = temp.filePath("root");
      const auto permissions = QFile::permissions(temp.filePath("incoming/blocked"));
      const auto restorePermissions = qScopeGuard([&]
      {
         QFile::setPermissions(temp.filePath("incoming/blocked"), permissions);
         QFile::setPermissions(temp.filePath("root/blocked"), permissions);
      });
      FM::DirWatcherLinux watcher;
      QVERIFY(watcher.addPath(path));
      QVERIFY(QFile::setPermissions(temp.filePath("incoming/blocked"), QFileDevice::WriteOwner | QFileDevice::ExeOwner));
      QVERIFY(::rename(QFile::encodeName(temp.filePath("incoming")).constData(), QFile::encodeName(path).constData()) == 0);
      const auto events = watcher.waitEvent(1000);
      QCOMPARE(events.size(), 1);
      QCOMPARE(events[0].type, FM::WatcherEvent::WATCH_LOST);
      QCOMPARE(events[0].path1, path);
      QVERIFY(!events[0].isWatchedFile);
      QCOMPARE(watcher.nbWatchedPath(), 0);
      QVERIFY(watcher.watchReferences.isEmpty());
   }

   void unexpectedWatchLossRequestsFallback_data()
   {
      QTest::addColumn<QString>("target");
      QTest::addColumn<bool>("unmount");
      for (const QString& target : {QString("root"), QString("descendant"), QString("file")})
         for (bool unmount : {false, true})
            QTest::newRow(qPrintable(target + (unmount ? "-unmount" : "-ignored"))) << target << unmount;
   }

   void unexpectedWatchLossRequestsFallback()
   {
      QFETCH(QString, target);
      QFETCH(bool, unmount);
      QTemporaryDir temp;
      QVERIFY(temp.isValid());
      QDir base(temp.path());
      QVERIFY(base.mkpath("root/sub/deep"));
      QVERIFY(base.mkdir("unaffected"));
      const QString root = temp.filePath("root");
      const QString sub = temp.filePath("root/sub");
      const QString filePath = temp.filePath("file.txt");
      const QString aliasPath = temp.filePath("alias.txt");
      {
         QFile file(filePath);
         QVERIFY(file.open(QIODevice::WriteOnly));
      }
      QVERIFY(::link(QFile::encodeName(filePath).constData(), QFile::encodeName(aliasPath).constData()) == 0);
      FM::DirWatcherLinux watcher;
      QVERIFY(watcher.addPath(root));
      QVERIFY(watcher.addPath(sub));
      QVERIFY(watcher.addPath(filePath));
      QVERIFY(watcher.addPath(aliasPath));
      QVERIFY(watcher.addPath(temp.filePath("unaffected")));
      const int wd = target == "file" ? watcher.files.value(filePath)->wd :
         watcher.dirs[target == "root" ? 0 : 1]->wd;
      // Remove the kernel watch behind the class's back to get a real,
      // unexpected IN_IGNORED event without deleting the watched object.
      QVERIFY(inotify_rm_watch(watcher.fileDescriptor, wd) == 0);
      QList<FM::WatcherEvent> events;
      if (unmount)
      {
         // A real unmount reports IN_UNMOUNT before IN_IGNORED. Inject the
         // first event separately to exercise a pair split across reads.
         const inotify_event event{wd, IN_UNMOUNT, 0, 0};
         events = watcher.processInotifyEvents(reinterpret_cast<const char*>(&event), sizeof(event));
      }
      else
         events = watcher.waitEvent(1000);
      // The independently registered subdirectory also depends on the root's
      // watch to detect ancestor moves, so losing it invalidates both owners.
      const QSet<QString> expected = target == "file" ? QSet<QString>{filePath, aliasPath} : QSet<QString>{root, sub};
      QSet<QString> lost;
      for (const auto& event : events)
      {
         QCOMPARE(event.type, FM::WatcherEvent::WATCH_LOST);
         QCOMPARE(event.isWatchedFile, target == "file");
         QVERIFY(!lost.contains(event.path1));
         lost.insert(event.path1);
      }
      QCOMPARE(lost, expected);
      QCOMPARE(watcher.nbWatchedPath(), 5 - expected.size());
      // The subsequent IN_IGNORED and cleanup of the other watches must not
      // produce duplicate notifications or affect surviving registrations.
      for (const auto& event : watcher.waitEvent(0))
         QCOMPARE(event.type, FM::WatcherEvent::TIMEOUT);
      const QString unaffectedPath = temp.filePath("unaffected/new.txt");
      {
         QFile file(unaffectedPath);
         QVERIFY(file.open(QIODevice::WriteOnly));
      }
      bool found = false;
      for (const auto& event : watcher.waitEvent(1000))
      {
         QVERIFY(event.type != FM::WatcherEvent::WATCH_LOST);
         found |= event.type == FM::WatcherEvent::NEW && event.path1 == unaffectedPath;
      }
      QVERIFY(found);
   }

   void expectedWatchRemovalDoesNotRequestFallback_data()
   {
      QTest::addColumn<bool>("explicitRemoval");
      QTest::newRow("explicit-removal") << true;
      QTest::newRow("directory-deletion") << false;
   }

   void expectedWatchRemovalDoesNotRequestFallback()
   {
      QFETCH(bool, explicitRemoval);
      QTemporaryDir temp;
      QVERIFY(temp.isValid());
      QDir base(temp.path());
      QVERIFY(base.mkpath("root/sub/deep"));
      FM::DirWatcherLinux watcher;
      QVERIFY(watcher.addPath(temp.filePath("root")));
      QVERIFY(watcher.addPath(temp.filePath("root/sub")));
      if (explicitRemoval)
         watcher.rmPath(temp.filePath("root"));
      else
         QVERIFY(QDir(temp.filePath("root/sub")).removeRecursively());
      for (const auto& event : watcher.waitEvent(1000))
         QVERIFY(event.type != FM::WatcherEvent::WATCH_LOST);
      QCOMPARE(watcher.nbWatchedPath(), 1);
      QVERIFY(base.mkpath("root/sub/deep"));
      for (const auto& event : watcher.waitEvent(0))
         QVERIFY(event.type != FM::WatcherEvent::WATCH_LOST);
      const QString path = temp.filePath("root/sub/deep/new.txt");
      {
         QFile file(path);
         QVERIFY(file.open(QIODevice::WriteOnly));
      }
      bool found = false;
      for (const auto& event : watcher.waitEvent(1000))
      {
         QVERIFY(event.type != FM::WatcherEvent::WATCH_LOST);
         found |= event.type == FM::WatcherEvent::NEW && event.path1 == path;
      }
      QVERIFY(found);
   }

   void watchLossDuringMove_data()
   {
      QTest::addColumn<bool>("reattach");
      QTest::newRow("move-within-root") << true;
      QTest::newRow("move-out-of-root") << false;
   }

   void watchLossDuringMove()
   {
      QFETCH(bool, reattach);
      QTemporaryDir temp;
      QVERIFY(temp.isValid());
      QDir base(temp.path());
      QVERIFY(base.mkpath("root/source/deep"));
      QVERIFY(base.mkpath("root/destination"));
      FM::DirWatcherLinux watcher;
      const QString rootPath = temp.filePath("root");
      QVERIFY(watcher.addPath(rootPath));
      auto* root = watcher.dirs.first();
      const int lostWd = root->children.value("source")->children.value("deep")->wd;
      QVERIFY(inotify_rm_watch(watcher.fileDescriptor, lostWd) == 0);
      QVERIFY(base.rename("root/source", reattach ? "root/destination/moved" : "outside"));

      // Exercise a terminal event while its subtree is detached from the index,
      // including IN_UNMOUNT and IN_IGNORED arriving in the same read.
      QByteArray batch;
      const auto appendEvent = [&](int wd, uint32_t mask, uint32_t cookie, const QByteArray& name = {})
      {
         const uint32_t len = name.isEmpty() ? 0 : (name.size() + 16) & ~15;
         const inotify_event event{wd, mask, cookie, len};
         batch.append(reinterpret_cast<const char*>(&event), sizeof(event));
         batch.append(name);
         batch.append(QByteArray(len - name.size(), '\0'));
      };
      appendEvent(root->wd, IN_MOVED_FROM | IN_ISDIR, 1, "source");
      appendEvent(lostWd, IN_UNMOUNT, 0);
      appendEvent(lostWd, IN_IGNORED, 0);
      if (reattach)
         appendEvent(root->children.value("destination")->wd, IN_MOVED_TO | IN_ISDIR, 1, "moved");
      const auto events = watcher.processInotifyEvents(batch.constData(), batch.size());
      QCOMPARE(events.size(), reattach ? 2 : 1);
      QCOMPARE(events[0].type, reattach ? FM::WatcherEvent::MOVE : FM::WatcherEvent::DELETED);
      QCOMPARE(events[0].path1, temp.filePath("root/source"));
      if (reattach)
      {
         QCOMPARE(events[0].path2, temp.filePath("root/destination/moved"));
         QCOMPARE(events[1].type, FM::WatcherEvent::WATCH_LOST);
         QCOMPARE(events[1].path1, rootPath);
         QVERIFY(!events[1].isWatchedFile);
      }
      QCOMPARE(watcher.nbWatchedPath(), reattach ? 0 : 1);
   }

   void newDirectoryWatchFailureRequestsFallback_data()
   {
      QTest::addColumn<QString>("operation");
      QTest::addColumn<bool>("nestedFailure");
      for (const QString& operation : {QString("create"), QString("move-in"), QString("matched-move")})
         for (bool nestedFailure : {false, true})
            QTest::newRow(qPrintable(operation + (nestedFailure ? "-nested" : "-direct"))) << operation << nestedFailure;
   }

   void newDirectoryWatchFailureRequestsFallback()
   {
      if (::geteuid() == 0)
         QSKIP("Root can watch unreadable directories");
      QFETCH(QString, operation);
      QFETCH(bool, nestedFailure);
      QTemporaryDir temp;
      QVERIFY(temp.isValid());
      QDir base(temp.path());
      QVERIFY(base.mkpath("root/sub"));
      QVERIFY(base.mkpath("unaffected"));
      const QString root = temp.filePath("root");
      const QString sub = temp.filePath("root/sub");
      const QString destination = temp.filePath("root/sub/incoming");
      const QString source = operation == "create" ? destination :
         temp.filePath(operation == "matched-move" ? "root/source" : "outside");
      const QString suffix = nestedFailure ? "/blocked" : "";
      if (operation != "create")
         QVERIFY(base.mkpath(source + suffix));

      FM::DirWatcherLinux watcher;
      QVERIFY(watcher.addPath(root));
      QVERIFY(watcher.addPath(sub));
      QVERIFY(watcher.addPath(temp.filePath("unaffected")));
      if (operation == "create")
         QVERIFY(base.mkpath(source + suffix));
      const auto originalPermissions = QFile::permissions(source + suffix);
      const auto restorePermissions = qScopeGuard([&]
      {
         QFile::setPermissions(source + suffix, originalPermissions);
         QFile::setPermissions(destination + suffix, originalPermissions);
      });
      QVERIFY(QFile::setPermissions(source + suffix, QFileDevice::WriteOwner | QFileDevice::ExeOwner));
      if (operation != "create")
         QVERIFY(base.rename(source, destination));

      const auto events = watcher.waitEvent(1000);
      QSet<QString> lostRoots;
      bool foundChange = false;
      for (const auto& event : events)
      {
         if (event.type == FM::WatcherEvent::WATCH_LOST)
         {
            QVERIFY(!event.isWatchedFile);
            QVERIFY(!lostRoots.contains(event.path1));
            lostRoots.insert(event.path1);
         }
         foundChange |= operation == "matched-move" ?
            event.type == FM::WatcherEvent::MOVE && event.path1 == source && event.path2 == destination :
            event.type == FM::WatcherEvent::NEW && event.path1 == destination;
      }
      QVERIFY(foundChange);
      // A matched move reuses the outer root's existing watches. Only the
      // additional destination owner needs new watches and loses coverage.
      QCOMPARE(lostRoots, operation == "matched-move" ? QSet<QString>{sub} : (QSet<QString>{root, sub}));
      QCOMPARE(watcher.nbWatchedPath(), operation == "matched-move" ? 2 : 1);
      QVERIFY(QFile::setPermissions(destination + suffix, originalPermissions));
      watcher.waitEvent(0); // Drain IN_IGNORED from retired watches.

      const QString laterPath = destination + suffix + "/later.txt";
      const QString unaffectedPath = temp.filePath("unaffected/later.txt");
      for (const auto& path : {laterPath, unaffectedPath})
      {
         QFile file(path);
         QVERIFY(file.open(QIODevice::WriteOnly));
      }
      bool foundLater = false;
      bool foundUnaffected = false;
      for (const auto& event : watcher.waitEvent(1000))
      {
         foundLater |= event.type == FM::WatcherEvent::NEW && event.path1 == laterPath;
         foundUnaffected |= event.type == FM::WatcherEvent::NEW && event.path1 == unaffectedPath;
      }
      QCOMPARE(foundLater, operation == "matched-move");
      QVERIFY(foundUnaffected);
   }

   void replacedFilesRemainWatched_data()
   {
      QTest::addColumn<QString>("replacementMode");
      QTest::newRow("atomic-replace") << "atomic";
      QTest::newRow("old-file-still-open") << "open";
      QTest::newRow("old-file-has-another-link") << "hardlink";
      QTest::newRow("rename-old-file-to-backup") << "backup";
   }

   void replacedFilesRemainWatched()
   {
      QFETCH(QString, replacementMode);
      QTemporaryDir temp;
      QVERIFY(temp.isValid());
      const QString path = temp.filePath("file.txt");
      const QString replacementPath = temp.filePath("replacement.txt");
      const QString backupPath = temp.filePath("backup.txt");
      {
         QFile file(path);
         QVERIFY(file.open(QIODevice::WriteOnly));
         QCOMPARE(file.write("original"), qint64(8));
      }
      QFile original(path);
      if (replacementMode == "open")
         QVERIFY(original.open(QIODevice::ReadWrite));
      if (replacementMode == "hardlink")
         QVERIFY(::link(QFile::encodeName(path).constData(), QFile::encodeName(backupPath).constData()) == 0);

      FM::DirWatcherLinux watcher;
      QVERIFY(watcher.addPath(temp.path(), "file.txt"));
      // Attribute changes on the same inode must preserve its watch.
      QVERIFY(QFile::setPermissions(path, QFile::permissions(path) | QFileDevice::ExeOwner));
      QVERIFY(watcher.waitEvent(1000).isEmpty());
      QCOMPARE(watcher.nbWatchedPath(), 1);
      for (int iteration = 0; iteration < 2; ++iteration)
      {
         {
            QFile replacement(replacementPath);
            QVERIFY(replacement.open(QIODevice::WriteOnly));
            QCOMPARE(replacement.write("replacement"), qint64(11));
         }
         if (replacementMode == "backup")
         {
            QFile::remove(backupPath);
            QVERIFY(QFile::rename(path, backupPath));
         }
         QVERIFY(::rename(QFile::encodeName(replacementPath).constData(), QFile::encodeName(path).constData()) == 0);
         const auto events = watcher.waitEvent(1000);
         QCOMPARE(events.size(), 1);
         QCOMPARE(events[0].type, FM::WatcherEvent::RESCAN);
         QCOMPARE(events[0].path1, path);
         QVERIFY(events[0].isWatchedFile);
         QCOMPARE(watcher.nbWatchedPath(), 1);

         // Writes to a retained old inode must not be attributed to the replacement.
         if (original.isOpen())
         {
            QCOMPARE(original.write("old"), qint64(3));
            QVERIFY(original.flush());
         }
         for (const auto& event : watcher.waitEvent(0))
            QCOMPARE(event.type, FM::WatcherEvent::TIMEOUT);
         {
            QFile replacement(path);
            QVERIFY(replacement.open(QIODevice::Append));
            QCOMPARE(replacement.write("new"), qint64(3));
         }
         bool found = false;
         for (const auto& event : watcher.waitEvent(1000))
            found |= event.type == FM::WatcherEvent::CONTENT_CHANGED && event.path1 == path && event.isWatchedFile;
         QVERIFY(found);
      }
   }

   void replacementWatchFailureRequestsFallback()
   {
      if (::geteuid() == 0)
         QSKIP("Root can watch unreadable files");
      QTemporaryDir temp;
      QVERIFY(temp.isValid());
      const QString path = temp.filePath("file.txt");
      const QString replacementPath = temp.filePath("replacement.txt");
      for (const auto& filePath : {path, replacementPath})
      {
         QFile file(filePath);
         QVERIFY(file.open(QIODevice::WriteOnly));
      }
      FM::DirWatcherLinux watcher;
      QVERIFY(watcher.addPath(temp.path(), "file.txt"));
      QVERIFY(QFile::setPermissions(replacementPath, QFileDevice::Permissions{}));
      QVERIFY(::rename(QFile::encodeName(replacementPath).constData(), QFile::encodeName(path).constData()) == 0);
      const auto events = watcher.waitEvent(1000);
      QCOMPARE(events.size(), 1);
      QCOMPARE(events[0].type, FM::WatcherEvent::WATCH_LOST);
      QCOMPARE(events[0].path1, path);
      QVERIFY(events[0].isWatchedFile);
      QCOMPARE(watcher.nbWatchedPath(), 0);
   }

   void movedAncestorReleasesDescendantShares_data()
   {
      QTest::addColumn<bool>("childFirst");
      QTest::addColumn<bool>("trailingSlash");
      QTest::newRow("parent-first") << false << false;
      QTest::newRow("child-first") << true << false;
      QTest::newRow("parent-first-trailing-slash") << false << true;
      QTest::newRow("child-first-trailing-slash") << true << true;
   }

   void movedAncestorReleasesDescendantShares()
   {
      QFETCH(bool, childFirst);
      QFETCH(bool, trailingSlash);
      QTemporaryDir temp;
      QVERIFY(temp.isValid());
      QDir base(temp.path());
      QVERIFY(base.mkpath("root/sub/deep"));
      QVERIFY(base.mkpath("root-other"));
      const QString root = temp.filePath("root");
      const QString rootRegistration = root + (trailingSlash ? "/" : "");
      const QString child = temp.filePath("root/sub");
      const QString deep = temp.filePath("root/sub/deep");
      const QString filePath = temp.filePath("root/sub/deep/file.txt");
      {
         QFile file(filePath);
         QVERIFY(file.open(QIODevice::WriteOnly));
      }

      FM::DirWatcherLinux watcher;
      QVERIFY(watcher.addPath(childFirst ? child : rootRegistration));
      QVERIFY(watcher.addPath(childFirst ? rootRegistration : child));
      QVERIFY(watcher.addPath(deep));
      QVERIFY(watcher.addPath(deep, "file.txt"));
      QVERIFY(watcher.addPath(temp.filePath("root-other")));
      QVERIFY(base.rename("root", "outside"));
      // Queue descendant changes immediately after the ancestor move.
      {
         QFile file(temp.filePath("outside/sub/deep/file.txt"));
         QVERIFY(file.open(QIODevice::Append));
         QCOMPARE(file.write("data"), qint64(4));
      }
      {
         QFile file(temp.filePath("outside/sub/new.txt"));
         QVERIFY(file.open(QIODevice::WriteOnly));
      }
      QSet<QString> deleted;
      const auto events = watcher.waitEvent(1000);
      QCOMPARE(events.size(), 4);
      for (const auto& event : events)
      {
         QCOMPARE(event.type, FM::WatcherEvent::DELETED);
         QCOMPARE(event.isWatchedFile, event.path1 == filePath);
         deleted.insert(event.path1);
      }
      QCOMPARE(deleted, (QSet<QString>{root, child, deep, filePath}));
      QCOMPARE(watcher.nbWatchedPath(), 1);
      {
         QFile file(temp.filePath("outside/sub/deep/later.txt"));
         QVERIFY(file.open(QIODevice::WriteOnly));
      }
      for (const auto& event : watcher.waitEvent(0))
         QCOMPARE(event.type, FM::WatcherEvent::TIMEOUT);

      // A similarly prefixed sibling share must remain active.
      const QString siblingFile = temp.filePath("root-other/new.txt");
      {
         QFile file(siblingFile);
         QVERIFY(file.open(QIODevice::WriteOnly));
      }
      bool found = false;
      for (const auto& event : watcher.waitEvent(1000))
         found |= event.type == FM::WatcherEvent::NEW && event.path1 == siblingFile;
      QVERIFY(found);
   }

   void overlappingDirectoryChanges_data()
   {
      QTest::addColumn<bool>("childFirst");
      QTest::addColumn<bool>("removeParent");
      for (bool childFirst : {false, true})
         for (bool removeParent : {false, true})
            QTest::newRow(qPrintable(QString("child-first=%1,remove-parent=%2").arg(childFirst).arg(removeParent)))
               << childFirst << removeParent;
   }

   void overlappingDirectoryChanges()
   {
      QFETCH(bool, childFirst);
      QFETCH(bool, removeParent);
      QTemporaryDir temp;
      QVERIFY(temp.isValid());
      QDir base(temp.path());
      QVERIFY(base.mkpath("root/sub/original/deep"));
      QVERIFY(base.mkpath("incoming/deep"));
      const QString parent = temp.filePath("root");
      const QString child = temp.filePath("root/sub");
      FM::DirWatcherLinux watcher;
      QVERIFY(watcher.addPath(childFirst ? child : parent));
      QVERIFY(watcher.addPath(childFirst ? parent : child));

      QVERIFY(base.mkpath("root/sub/created/deep"));
      watcher.waitEvent(1000);
      QVERIFY(base.rename("incoming", "root/sub/incoming"));
      watcher.waitEvent(1000);
      QVERIFY(base.rename("root/sub/original", "root/sub/renamed"));
      const auto renamed = watcher.waitEvent(1000);
      QCOMPARE(renamed.size(), 1); // One public event, even with multiple owners.
      QCOMPARE(renamed[0].type, FM::WatcherEvent::MOVE);
      QVERIFY(QDir(temp.filePath("root/sub/created")).removeRecursively());
      watcher.waitEvent(1000);
      QVERIFY(base.mkpath("root/sub/created/deep"));
      watcher.waitEvent(1000);

      // Move between locations with different numbers of owning root trees.
      QVERIFY(base.mkpath("root/entering/deep"));
      watcher.waitEvent(1000);
      QVERIFY(base.rename("root/entering", "root/sub/entered"));
      watcher.waitEvent(1000);
      QVERIFY(base.mkpath("root/sub/leaving/deep"));
      watcher.waitEvent(1000);
      QVERIFY(base.rename("root/sub/leaving", "root/left"));
      watcher.waitEvent(1000);
      QVERIFY(base.mkpath("root/sub/exiting/deep"));
      watcher.waitEvent(1000);
      QVERIFY(base.rename("root/sub/exiting", "outside"));
      watcher.waitEvent(1000);
      {
         QFile outside(temp.filePath("outside/deep/file.txt"));
         QVERIFY(outside.open(QIODevice::WriteOnly));
      }
      for (const auto& event : watcher.waitEvent(0))
         QCOMPARE(event.type, FM::WatcherEvent::TIMEOUT);

      watcher.rmPath(removeParent ? parent : child);
      QCOMPARE(watcher.nbWatchedPath(), 1);
      watcher.waitEvent(0); // Drain IN_IGNORED for watches belonging only to the removed root.
      QStringList directories{"root/sub/created", "root/sub/incoming", "root/sub/renamed", "root/sub/entered"};
      if (!removeParent)
         directories << "root/left";
      for (const QString& directory : directories)
      {
         const QString path = temp.filePath(directory + "/deep/file.txt");
         QFile file(path);
         QVERIFY(file.open(QIODevice::WriteOnly));
         file.close();
         int found = 0;
         for (const auto& event : watcher.waitEvent(1000))
            if (event.type == FM::WatcherEvent::NEW && event.path1 == path)
               ++found;
         QCOMPARE(found, 1);
      }
   }

   void descriptorPolling_data()
   {
      QTest::addColumn<bool>("highDescriptors");
      QTest::newRow("normal-descriptors") << false;
      QTest::newRow("descriptors-above-1023") << true;
   }

   void descriptorPolling()
   {
      QFETCH(bool, highDescriptors);
      QTemporaryDir temp;
      QVERIFY(temp.isValid());
      // Hold the lower descriptors open without changing the process limits.
      // RAII also closes them if an assertion fails or the test is skipped.
      std::vector<std::unique_ptr<QFile>> heldFiles;
      if (highDescriptors)
      {
         struct rlimit limit;
         QVERIFY(getrlimit(RLIMIT_NOFILE, &limit) == 0);
         if (limit.rlim_cur < 1120)
            QSKIP("The process descriptor limit is too low for this test.");
         do
         {
            auto file = std::make_unique<QFile>("/dev/null");
            if (!file->open(QIODevice::ReadOnly))
               QSKIP("The process descriptor limit is too low for this test.");
            heldFiles.push_back(std::move(file));
         }
         while (heldFiles.back()->handle() < 1100);
      }

      FM::DirWatcherLinux watcher;
      FM::WaitConditionLinux first;
      FM::WaitConditionLinux second;
      if (highDescriptors)
      {
         QVERIFY(first.getFd() > 1100);
         QVERIFY(second.getFd() > 1100);
      }
      // FileUpdater also waits directly on the condition when no paths are
      // watched. That path must support high descriptors as well.
      QVERIFY(first.wait(0));
      QElapsedTimer waitTimer;
      waitTimer.start();
      QVERIFY(first.wait(20));
      QVERIFY(waitTimer.elapsed() >= 20);

      QVERIFY(watcher.addPath(temp.path()));
      const QList<FM::WaitCondition*> conditions{&first, &second};
      const auto timeout = watcher.waitEvent(10, conditions);
      QCOMPARE(timeout.size(), 1);
      QCOMPARE(timeout[0].type, FM::WatcherEvent::TIMEOUT);

      const QString path = temp.filePath("file.txt");
      {
         QFile file(path);
         QVERIFY(file.open(QIODevice::WriteOnly));
      }
      // A wait-condition wakeup takes priority without consuming file events.
      second.release();
      QVERIFY(watcher.waitEvent(1000, conditions).isEmpty());
      bool found = false;
      for (const auto& event : watcher.waitEvent(1000, conditions))
         found |= event.type == FM::WatcherEvent::NEW && event.path1 == path;
      QVERIFY(found);
      first.release();
      QVERIFY(watcher.waitEvent(1000, conditions).isEmpty());

      const auto drained = watcher.waitEvent(0, conditions);
      QCOMPARE(drained.size(), 1);
      QCOMPARE(drained[0].type, FM::WatcherEvent::TIMEOUT);
   }

   void movedFilesReleaseOldPath_data()
   {
      QTest::addColumn<QString>("destination");
      QTest::newRow("rename") << "renamed.txt";
      QTest::newRow("move-to-another-directory") << "other/moved.txt";
   }

   void movedFilesReleaseOldPath()
   {
      QFETCH(QString, destination);
      QTemporaryDir temp;
      QVERIFY(temp.isValid());
      QDir base(temp.path());
      QVERIFY(base.mkdir("other"));
      const QString oldPath = temp.filePath("file.txt");
      const QString newPath = temp.filePath(destination);
      {
         QFile file(oldPath);
         QVERIFY(file.open(QIODevice::WriteOnly));
      }
      FM::DirWatcherLinux watcher;
      QVERIFY(watcher.addPath(temp.path(), "file.txt"));
      QVERIFY(QFile::rename(oldPath, newPath));
      {
         QFile file(newPath);
         QVERIFY(file.open(QIODevice::Append));
         QCOMPARE(file.write("before"), qint64(6));
      }
      // A self-move has no destination name. Retire the old registration,
      // including modifications queued after the rename in the same read.
      const auto events = watcher.waitEvent(1000);
      QCOMPARE(events.size(), 1);
      QCOMPARE(events[0].type, FM::WatcherEvent::DELETED);
      QCOMPARE(events[0].path1, oldPath);
      QVERIFY(events[0].isWatchedFile);
      QCOMPARE(watcher.nbWatchedPath(), 0);
      {
         QFile file(newPath);
         QVERIFY(file.open(QIODevice::Append));
         QCOMPARE(file.write("after"), qint64(5));
      }
      for (const auto& event : watcher.waitEvent(0))
         QCOMPARE(event.type, FM::WatcherEvent::TIMEOUT);

      // Explicitly watching the new location must work normally.
      QVERIFY(watcher.addPath(newPath));
      {
         QFile file(newPath);
         QVERIFY(file.open(QIODevice::Append));
         QCOMPARE(file.write("watched"), qint64(7));
      }
      bool found = false;
      for (const auto& event : watcher.waitEvent(1000))
         found |= event.type == FM::WatcherEvent::CONTENT_CHANGED && event.path1 == newPath && event.isWatchedFile;
      QVERIFY(found);
   }

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
      QTest::newRow("hidden-directory") << "first/.hidden/file.txt";
      QTest::newRow("inside-hidden-directory") << "first/.hidden/sub/file.txt";
      QTest::newRow("nested-hidden-directory") << "first/child/.hidden/file.txt";
      QTest::newRow("second-root") << "second/file.txt";
   }

   void directoryNotifications()
   {
      QFETCH(QString, relativePath);
      QTemporaryDir temp;
      QVERIFY(temp.isValid());
      QVERIFY(QDir(temp.path()).mkpath("first/child/grandchild"));
      QVERIFY(QDir(temp.path()).mkpath("first/.hidden/sub"));
      QVERIFY(QDir(temp.path()).mkpath("first/child/.hidden"));
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
