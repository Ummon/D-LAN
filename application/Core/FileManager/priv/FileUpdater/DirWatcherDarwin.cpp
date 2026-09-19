#include <priv/FileUpdater/DirWatcherDarwin.h>
using namespace FM;

#include <QDeadlineTimer>
#include <QDir>
#include <QFileInfo>
#include <QSet>
#include <cerrno>
#include <poll.h>
#include <stdexcept>
#include <system_error>
#include <vector>

namespace
{
   QString registrationPath(const QString& directory, const QString& filename)
   {
      return QDir::cleanPath(filename.isEmpty() ? directory : QDir(directory).filePath(filename));
   }

   bool isUnder(const QString& path, const QString& root)
   {
      return path == root || path.startsWith(root == "/" ? root : root + '/');
   }
}

struct DirWatcherDarwin::Watch
{
   Watch(WaitConditionDarwin& changed, dispatch_queue_t queue) : changed(changed), queue(queue) {}
   ~Watch()
   {
      if (stream)
      {
         if (started)
            FSEventStreamStop(stream);
         FSEventStreamInvalidate(stream);
         // Finish callbacks before their context (this) or its wake-up pipe dies.
         // Callbacks use only the per-watch mutex, never the registration mutex.
         dispatch_sync_f(queue, nullptr, [](void*) {});
         FSEventStreamRelease(stream);
      }
   }

   WaitConditionDarwin& changed;
   dispatch_queue_t queue;
   FSEventStreamRef stream = nullptr;
   bool started = false;
   QString path; // Preserve the spelling used by the cache, including /tmp aliases.
   QString nativePath;
   bool file = false;
   QMutex mutex;
   QSet<QString> directories;
   bool rescan = false;
   bool lost = false;
};

DirWatcherDarwin::DirWatcherDarwin() : queue(dispatch_queue_create("net.d-lan.fsevents", DISPATCH_QUEUE_SERIAL))
{
   if (!this->queue)
      throw std::runtime_error("Unable to create FSEvents dispatch queue");
}

DirWatcherDarwin::~DirWatcherDarwin()
{
   this->watches.clear();
   dispatch_release(this->queue);
}

bool DirWatcherDarwin::addPath(const QString& directory, const QString& filename)
{
   const QString path = registrationPath(directory, filename);
   const QFileInfo info(path);
   if (!QDir::isAbsolutePath(path) || info.isSymLink() || (!info.isDir() && !info.isFile()))
      return false;

   auto watch = std::make_unique<Watch>(this->changed, this->queue);
   watch->path = path;
   watch->nativePath = info.canonicalFilePath().normalized(QString::NormalizationForm_C);
   if (watch->nativePath.isEmpty())
      return false; // The entry may have disappeared since the metadata check.
   watch->file = info.isFile();
   // A file's parent remains watched across atomic saves/replacements.
   const QString streamPath = watch->file ? QFileInfo(watch->nativePath).absolutePath() : watch->nativePath;
   const auto utf8 = streamPath.toUtf8();
   CFStringRef nativeString = CFStringCreateWithBytes(kCFAllocatorDefault,
      reinterpret_cast<const UInt8*>(utf8.constData()), utf8.size(), kCFStringEncodingUTF8, false);
   if (!nativeString)
      return false;
   const void* value = nativeString;
   CFArrayRef paths = CFArrayCreate(kCFAllocatorDefault, &value, 1, &kCFTypeArrayCallBacks);
   CFRelease(nativeString);
   if (!paths)
      return false;
   FSEventStreamContext context{0, watch.get(), nullptr, nullptr, nullptr};
   watch->stream = FSEventStreamCreate(kCFAllocatorDefault, &DirWatcherDarwin::callback,
      &context, paths, kFSEventStreamEventIdSinceNow, 0.1,
      kFSEventStreamCreateFlagFileEvents | kFSEventStreamCreateFlagWatchRoot | kFSEventStreamCreateFlagNoDefer);
   CFRelease(paths);
   if (!watch->stream)
      return false;
   FSEventStreamSetDispatchQueue(watch->stream, this->queue);
   if (!FSEventStreamStart(watch->stream))
      return false;
   watch->started = true;

   QMutexLocker locker(&this->mutex);
   // The old registration stays alive until its replacement has started.
   this->watches[path] = std::move(watch);
   // A callback can run between Start and insertion. Its first wakeup may
   // already have been consumed while the watch wasn't in the map yet.
   this->changed.release();
   return true;
}

void DirWatcherDarwin::rmPath(const QString& directory, const QString& filename)
{
   QMutexLocker locker(&this->mutex);
   this->watches.erase(registrationPath(directory, filename));
}

int DirWatcherDarwin::nbWatchedPath()
{
   QMutexLocker locker(&this->mutex);
   return static_cast<int>(this->watches.size());
}

void DirWatcherDarwin::callback(ConstFSEventStreamRef, void* context, size_t count,
   void* paths, const FSEventStreamEventFlags flags[], const FSEventStreamEventId[])
{
   recordEvents(*static_cast<Watch*>(context), count, static_cast<char**>(paths), flags);
}

void DirWatcherDarwin::recordEvents(Watch& watch, size_t count, char* const* paths,
   const FSEventStreamEventFlags* flags)
{
   QMutexLocker locker(&watch.mutex);
   for (size_t i = 0; i < count; ++i)
   {
      if (flags[i] & kFSEventStreamEventFlagHistoryDone)
         continue;
      // The stream follows inodes. If the root or an ancestor moves, its old
      // pathname is no longer reliably watched; hand it to periodic recovery.
      if (flags[i] & (kFSEventStreamEventFlagRootChanged | kFSEventStreamEventFlagUnmount))
         watch.lost = true;
      if (flags[i] & (kFSEventStreamEventFlagMustScanSubDirs | kFSEventStreamEventFlagUserDropped |
          kFSEventStreamEventFlagKernelDropped | kFSEventStreamEventFlagEventIdsWrapped | kFSEventStreamEventFlagMount))
         watch.rescan = true;
      if (watch.lost || watch.rescan)
      {
         watch.directories.clear();
         continue;
      }

      const QString native = QDir::cleanPath(QString::fromUtf8(paths[i])).normalized(QString::NormalizationForm_C);
      if (watch.file)
      {
         if (native == watch.nativePath)
            watch.rescan = true;
         continue;
      }
      if (!isUnder(native, watch.nativePath))
         continue;
      if (native == watch.nativePath)
         watch.rescan = true;
      else
      {
         // FSEvents may coalesce creates, deletes and renames. Reconcile the
         // containing directory rather than guessing a rename pair. This also
         // lets the normal scanner exclude symlinks and unfinished downloads.
         const QString relative = native.mid(watch.nativePath == "/" ? 1 : watch.nativePath.size() + 1);
         watch.directories.insert(QFileInfo(QDir(watch.path).filePath(relative)).absolutePath());
         if (watch.directories.size() > 256)
            watch.rescan = true; // Bound pending work during large imports.
      }
   }
   if (watch.rescan || watch.lost)
      watch.directories.clear();
   if (watch.rescan || watch.lost || !watch.directories.isEmpty())
      watch.changed.release();
}

QList<WatcherEvent> DirWatcherDarwin::takeEvents()
{
   QList<WatcherEvent> events;
   QMutexLocker locker(&this->mutex);
   for (auto it = this->watches.begin(); it != this->watches.end();)
   {
      auto& watch = *it->second;
      bool lost;
      {
         QMutexLocker pendingLocker(&watch.mutex);
         lost = watch.lost;
         if (watch.lost || watch.rescan || watch.directories.contains(watch.path))
            events.append(WatcherEvent(watch.lost ? WatcherEvent::WATCH_LOST : WatcherEvent::RESCAN, watch.path, watch.file));
         else
            for (const auto& path : std::as_const(watch.directories))
            {
               // A notified directory may already have vanished or become a
               // symlink. Scan its nearest surviving parent, without following
               // symlinks outside the share, so stale cached directories vanish.
               QString surviving = watch.path;
               const QString relative = QDir(watch.path).relativeFilePath(path);
               for (const auto& component : relative.split('/', Qt::SkipEmptyParts))
               {
                  const QString next = QDir(surviving).filePath(component);
                  const QFileInfo info(next);
                  if (info.isSymLink() || !info.isDir())
                     break;
                  surviving = next;
               }
               events.append(WatcherEvent(surviving == watch.path ? WatcherEvent::RESCAN : WatcherEvent::CONTENT_CHANGED,
                  surviving, false));
            }
         watch.directories.clear();
         watch.rescan = false;
      }
      if (lost)
         it = this->watches.erase(it);
      else
         ++it;
   }
   return events;
}

const QList<WatcherEvent> DirWatcherDarwin::waitEvent(QList<WaitCondition*> ws)
{
   return this->waitEvent(-1, ws);
}

const QList<WatcherEvent> DirWatcherDarwin::waitEvent(int timeout, QList<WaitCondition*> ws)
{
   std::vector<pollfd> fds{{this->changed.getFd(), POLLIN, 0}};
   for (auto* condition : ws)
      fds.push_back({static_cast<WaitConditionDarwin*>(condition)->getFd(), POLLIN, 0});
   QDeadlineTimer deadline(timeout);
   for (;;)
   {
      int ready;
      do
         ready = poll(fds.data(), fds.size(), static_cast<int>(deadline.remainingTime()));
      while (ready < 0 && errno == EINTR);
      if (ready == 0)
         return {WatcherEvent(WatcherEvent::TIMEOUT, false)};
      if (ready < 0)
         throw std::system_error(errno, std::generic_category(), "Unable to poll FSEvents notifications");
      bool released = false;
      for (qsizetype i = 0; i < ws.size(); ++i)
         if (fds[i + 1].revents & POLLIN)
         {
            ws[i]->wait(0);
            released = true;
         }
      if (fds[0].revents & POLLIN)
         this->changed.wait(0);
      auto events = this->takeEvents();
      if (released || !events.isEmpty())
         return events;
      // A removed registration can leave a wake byte but no events. Continue
      // with the original deadline, rather than reporting an early timeout.
   }
}
