/**
  * D-LAN - A decentralized LAN file sharing software.
  * Copyright (C) 2010-2012 Greg Burri <greg.burri@gmail.com>
  *
  * This program is free software: you can redistribute it and/or modify
  * it under the terms of the GNU General Public License as published by
  * the Free Software Foundation, either version 3 of the License, or
  * (at your option) any later version.
  *
  * This program is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  *
  * You should have received a copy of the GNU General Public License
  * along with this program.  If not, see <http://www.gnu.org/licenses/>.
  */

#include <priv/FileUpdater/DirWatcherLinux.h>
using namespace FM;

#include <unistd.h>

#include <QMutexLocker>
#include <QFile>
#include <QFileInfo>

#include <priv/FileUpdater/WaitConditionLinux.h>
#include <priv/Log.h>

#include <poll.h>
#include <sys/inotify.h>
#include <sys/stat.h>
#include <unistd.h>
#include <errno.h>
#include <memory>
#include <vector>

/**
  * @class FM::DirWatcherLinux
  * @author Hervé Martinet
  *
  * Implementation of 'DirWatcher' for the linux platform with inotify.
  */

const int DirWatcherLinux::EVENT_SIZE = (sizeof (struct inotify_event));
const size_t DirWatcherLinux::BUF_LEN = (1024 * (EVENT_SIZE + 16));
const uint32_t DirWatcherLinux::EVENTS_OBS = IN_MOVE | IN_DELETE | IN_CREATE | IN_CLOSE_WRITE;
const uint32_t DirWatcherLinux::ROOT_EVENTS_OBS = EVENTS_OBS | IN_MOVE_SELF | IN_DELETE_SELF;
const uint32_t DirWatcherLinux::EVENTS_FILE = IN_MODIFY | IN_ATTRIB | IN_MOVE_SELF | IN_DELETE_SELF;

class UnableToWatchException {};

/**
  * Constructor.
  */
DirWatcherLinux::DirWatcherLinux()
{
   // Initialize inotify
   this->initialized = true;
   this->fileDescriptor = inotify_init();
   if (fileDescriptor < 0)
   {
      L_WARN(QString("Unable to initialize inotify, DirWatcher not used."));
      this->initialized = false;
   }
}

/**
  * Destructor.
  */
DirWatcherLinux::~DirWatcherLinux()
{
   QMutexLocker locker(&this->mutex);
   this->clearWatches();

   if (this->fileDescriptor >= 0 && close(this->fileDescriptor) < 0)
      L_WARN(QString("DirWatcherLinux::~DirWatcherLinux: Unable to close file descriptor (inotify)"));
}

void DirWatcherLinux::clearWatches()
{
   // Remove all directories.
   for (QMutableListIterator<Dir*> i(dirs); i.hasNext();)
   {
      Dir* dir = i.next();
      delete dir;
      i.remove();
   }

   // Remove all files.
   for (auto i = this->files.begin(); i != this->files.end(); ++i)
      delete i.value();
   this->files.clear();
   this->watchReferences.clear();
}

QList<WatcherEvent> DirWatcherLinux::recoverFromOverflow()
{
   L_WARN("Inotify queue overflowed; rebuilding watches and requesting a rescan.");
   QStringList directoryPaths;
   for (Dir* dir : this->dirs)
      directoryPaths << dir->name;
   const QStringList filePaths = this->files.keys();

   this->clearWatches();
   // A fresh instance discards stale events and descriptors from the old tree.
   close(this->fileDescriptor);
   this->fileDescriptor = inotify_init();
   this->initialized = this->fileDescriptor >= 0;

   QList<WatcherEvent> events;
   for (const QString& path : directoryPaths)
   {
      bool restored = false;
      if (this->initialized && QDir(path).exists())
         try
         {
            this->dirs << new Dir(this, nullptr, path);
            restored = true;
         }
         catch (UnableToWatchException&) {}
      events << WatcherEvent(restored ? WatcherEvent::RESCAN : WatcherEvent::WATCH_LOST, path, false);
   }
   for (const QString& path : filePaths)
   {
      bool restored = false;
      if (this->initialized && QFileInfo(path).isFile())
         try
         {
            this->files.insert(path, new File(this, path));
            restored = true;
         }
         catch (UnableToWatchException&) {}
      events << WatcherEvent(restored ? WatcherEvent::RESCAN : WatcherEvent::WATCH_LOST, path, true);
   }
   return events;
}

/**
  * @copydoc FM::DirWatcher::addPath(..)
  */
bool DirWatcherLinux::addPath(const QString& directory, const QString& filename)
{
   const QString path = filename.isEmpty() ? directory : QDir(directory).filePath(filename);
   QMutexLocker locker(&this->mutex);

   if (!this->initialized)
      return false;

   try
   {
      if (QDir(path).exists())
      {
         Dir* dir = new Dir(this, nullptr, path);
         this->dirs << dir;
      }
      else
      {
         File* file = new File(this, path);
         // Acquire the new watch first so failure preserves the registration.
         // Replacing its pointer must also release the previous owner's reference.
         delete this->files.value(path);
         this->files.insert(path, file);
      }
      return true;
   }
   catch (UnableToWatchException&)
   {
      return false;
   }
}

// Hard-linked files share a kernel watch. Snapshot every owner before handling
// an event, since individual paths may be removed or attached to a new inode.
QList<DirWatcherLinux::File*> DirWatcherLinux::getFiles(int wd) const
{
   QList<File*> result;
   for (auto i = this->files.begin(); i != this->files.end(); ++i)
      if (i.value()->wd == wd)
         result << i.value();
   return result;
}

/**
  * Find a watched directory, including descendants of every watched root.
  * Return 'nullptr' if not found.
  */
DirWatcherLinux::Dir* DirWatcherLinux::getDir(int wd) const
{
   // A directly shared directory takes precedence over another root's descendant.
   for (Dir* dir : this->dirs)
      if (dir->wd == wd)
         return dir;

   QList<Dir*> pending = this->dirs;
   while (!pending.isEmpty())
   {
      Dir* dir = pending.takeLast();
      if (dir->wd == wd)
         return dir;
      for (Dir* child : dir->children)
         pending.append(child);
   }
   return nullptr;
}

// A kernel watch can be represented in several overlapping root trees.
QList<DirWatcherLinux::Dir*> DirWatcherLinux::getDirs(int wd) const
{
   QList<Dir*> result;
   QList<Dir*> pending = this->dirs;
   while (!pending.isEmpty())
   {
      Dir* dir = pending.takeLast();
      if (dir->wd == wd)
         result << dir;
      for (Dir* child : dir->children)
         pending << child;
   }
   return result;
}

void DirWatcherLinux::addChildWatches(int parentWd, const QString& name, QSet<QString>& failedRoots)
{
   for (Dir* parent : this->getDirs(parentWd))
      if (!parent->children.contains(name))
         try
         {
            new Dir(this, parent, name);
         }
         catch (UnableToWatchException&)
         {
            failedRoots.insert(parent->getRoot()->name);
         }
}

/**
  * @copydoc FM::DirWatcher::rmPath(..)
  */
void DirWatcherLinux::rmPath(const QString& directory, const QString& filename)
{
   const QString path = filename.isEmpty() ? directory : QDir(directory).filePath(filename);
   QMutexLocker locker(&this->mutex);

   // Consult the registration, since a removed or renamed path no longer
   // identifies the filesystem object that was originally watched.
   for (QMutableListIterator<Dir*> i(dirs); i.hasNext();)
   {
      Dir* dir = i.next();
      if (dir->name == path)
      {
         delete dir;
         i.remove();
         return;
      }
   }
   auto file = this->files.find(path);
   if (file != this->files.end())
   {
      delete file.value();
      this->files.erase(file);
   }
}

// An ancestor move also invalidates the registered paths of independently shared
// descendants, which do not receive their own IN_MOVE_SELF notifications.
QList<DirWatcherLinux::RemovedPath> DirWatcherLinux::removeWatchedPathsUnder(const QString& path)
{
   const QString root = QDir::cleanPath(path);
   const QString prefix = root.endsWith('/') ? root : root + '/';
   const auto isUnderRoot = [&](const QString& registeredPath)
   {
      const QString cleanPath = QDir::cleanPath(registeredPath);
      return cleanPath == root || cleanPath.startsWith(prefix);
   };

   QList<RemovedPath> removed;
   for (QMutableListIterator<Dir*> i(this->dirs); i.hasNext();)
   {
      Dir* dir = i.next();
      if (isUnderRoot(dir->name))
      {
         removed << RemovedPath{dir->name, false};
         delete dir;
         i.remove();
      }
   }
   for (auto i = this->files.begin(); i != this->files.end();)
   {
      if (isUnderRoot(i.key()))
      {
         removed << RemovedPath{i.key(), true};
         delete i.value();
         i = this->files.erase(i);
      }
      else
         ++i;
   }
   return removed;
}

/**
  * Return the full path notified by an inotify event on a directory watch.
  * Return a null QString if not found.
  * @param path the full path
  */
QString DirWatcherLinux::getEventPath(const inotify_event* event)
{
   QMutexLocker locker(&this->mutex);

   // Event for a watched directory.
   Dir* dir = this->getDir(event->wd);
   if (dir)
   {
      QString p = dir->getFullPath();
      if (event->len)
         p.append('/').append(event->name);
      return p;
   }

   return QString();
}

/**
  * @copydoc FM::DirWatcher::nbWatchedPath()
  */
int DirWatcherLinux::nbWatchedPath()
{
   QMutexLocker locker(&this->mutex);
   return this->dirs.size() + this->files.size();
}

/**
  * @copydoc FM::DirWatcher::waitEvent(QList<WaitCondition*>)
  */
const QList<WatcherEvent> DirWatcherLinux::waitEvent(QList<WaitCondition*> ws)
{
   return this->waitEvent(-1, ws);
}

/**
  * @copydoc FM::DirWatcher::waitEvent(int, QList<WaitCondition*>)
  */
const QList<WatcherEvent> DirWatcherLinux::waitEvent(int timeout, QList<WaitCondition*> ws)
{
   QMutexLocker locker(&this->mutex);

   // poll() accepts descriptors above FD_SETSIZE. Keep inotify in the first
   // slot; a negative descriptor is ignored if initialization failed.
   std::vector<pollfd> fds;
   fds.reserve(ws.size() + 1);
   fds.push_back({this->initialized ? this->fileDescriptor : -1, POLLIN, 0});
   for (WaitCondition* condition : ws)
   {
      const int wcfd = dynamic_cast<WaitConditionLinux*>(condition)->getFd();
      fds.push_back({wcfd, POLLIN, 0});
   }

   // Wait with the mutex unlocked so registrations can still change.
   L_DEBU("DirWatcherLinux::waitEvent: active poll");
   locker.unlock();
   const int ready = poll(fds.data(), fds.size(), timeout);
   locker.relock();

   if (ready < 0)
   {
      L_ERRO(QString("DirWatcherLinux::waitEvent: poll error."));
      return QList<WatcherEvent>();
   }
   else if (!ready)
   {
      L_DEBU("DirWatcherLinux::waitEvent: exit poll by timeout");
      QList<WatcherEvent> events;
      events << WatcherEvent(WatcherEvent::TIMEOUT, false);
      return events;
   }

   // Give wait conditions priority, leaving pending filesystem events queued.
   for (size_t i = 1; i < fds.size(); ++i)
   {
      if (fds[i].revents & POLLIN)
      {
         const int wcfd = fds[i].fd;
         L_DEBU(QString("DirWatcherLinux::waitEvent: exit poll by WaitCondition release (fd=%1)").arg(wcfd));
         char dummy[4096];
         while (read(wcfd, dummy, sizeof(dummy)) > 0);
         return QList<WatcherEvent>();
      }
   }

   // An error or hangup on a descriptor can wake poll without readable data.
   // Do not enter the blocking inotify read unless POLLIN was reported.
   if (!(fds[0].revents & POLLIN))
   {
      L_ERRO(QString("DirWatcherLinux::waitEvent: poll woke without readable data."));
      return QList<WatcherEvent>();
   }

   L_DEBU("DirWatcherLinux::waitEvent: exit poll by inotify");

   alignas(inotify_event) char buf[BUF_LEN];
   int len = read(this->fileDescriptor, buf, BUF_LEN);
   if (len < 0)
   {
      if (errno == EINTR)
         // Need to reissue system call.
         return QList<WatcherEvent>();
      else
         L_ERRO(QString("DirWatcherLinux::waitEvent: read inotify event failed."));
   }
   else if (!len)
   {
      L_ERRO(QString("DirWatcherLinux::waitEvent: BUF_LEN to small?"));
   }

   return this->processInotifyEvents(buf, len);
}

// Called with the watcher mutex held.
QList<WatcherEvent> DirWatcherLinux::processInotifyEvents(const char* buf, int len)
{
   // Detect overflow before interpreting any events in this buffer against a
   // potentially stale index. Overflow is global and has no watch descriptor.
   for (int i = 0; i < len;)
   {
      const auto* event = reinterpret_cast<const inotify_event*>(&buf[i]);
      if (event->mask & IN_Q_OVERFLOW)
         return this->recoverFromOverflow();
      i += EVENT_SIZE + event->len;
   }

   QList<WatcherEvent> events;
   struct DetachedDirectory
   {
      Dir* root;
      std::unique_ptr<Dir> directory;
   };
   struct PendingMove
   {
      uint32_t cookie;
      QString path;
      qsizetype eventIndex;
      std::vector<DetachedDirectory> directories;
   };
   std::vector<PendingMove> movedFromEvents;
   QSet<QString> failedRoots;
   QSet<int> lostWatches;
   struct PendingRestoration
   {
      RemovedPath registration;
      qsizetype eventIndex;
   };
   QList<PendingRestoration> pathsToRestore;
   const auto retirePaths = [&](const QString& path)
   {
      for (const RemovedPath& removed : this->removeWatchedPathsUnder(path))
      {
         pathsToRestore << PendingRestoration{removed, events.size()};
         events << WatcherEvent(WatcherEvent::DELETED, removed.path, removed.isWatchedFile);
      }
   };

   for (int i = 0; i < len;)
   {
      const auto* event = reinterpret_cast<const inotify_event*>(&buf[i]);
      i += EVENT_SIZE + event->len;

      if (event->mask & (IN_IGNORED | IN_UNMOUNT))
      {
         // Explicit removal has already released all references. Any remaining
         // owners have unexpectedly lost coverage, including detached moves.
         if (this->watchReferences.contains(event->wd))
            lostWatches.insert(event->wd);
         continue;
      }
      if (lostWatches.contains(event->wd))
         continue;

      Dir* dir = nullptr;

      // Watched directories.
      if (dir = this->getDir(event->wd))
      {
         if (event->mask & IN_DELETE_SELF)
         {
            // Retire all representations before the expected IN_IGNORED,
            // which can arrive before the parent's IN_DELETE notification.
            const QString path = dir->getFullPath();
            retirePaths(path);
            for (Dir* deleted : this->getDirs(event->wd))
               delete deleted;
            continue;
         }

         if (event->mask & IN_MOVED_FROM)
         {
            L_DEBU(QString("inotify event (dir): IN_MOVED_FROM (path=%1)").arg(this->getEventPath(event)));
            const QString path = this->getEventPath(event);
            // Hide the old subtree immediately, including from later events in
            // this read. Keep ownership until the move is matched or abandoned.
            PendingMove move{event->cookie, path, events.size(), {}};
            if (event->mask & IN_ISDIR)
               for (Dir* parent : this->getDirs(event->wd))
                  if (Dir* movedDir = parent->children.take(event->name))
                  {
                     movedDir->parent = nullptr;
                     move.directories.push_back({parent->getRoot(), std::unique_ptr<Dir>(movedDir)});
                  }
            movedFromEvents.push_back(std::move(move));
            // Preserve ordering if the old path is recreated before this read ends.
            events << WatcherEvent(WatcherEvent::DELETED, path, false);
            // Independently registered descendants do not receive IN_MOVE_SELF
            // when an ancestor moves. Retire their old paths before processing
            // any queued changes; replacements are restored at the end as usual.
            if (event->mask & IN_ISDIR)
               retirePaths(path);
         }

         if ((event->mask & IN_MOVED_TO) && !QFileInfo(this->getEventPath(event)).isSymLink())
         {
            L_DEBU(QString("inotify event (dir): IN_MOVED_TO (path=%1)").arg(this->getEventPath(event)));
            // Check list of IN_MOVED_FROM events.
            for (auto i = movedFromEvents.begin(); i != movedFromEvents.end(); ++i)
            {
               if (i->cookie == event->cookie)
               {
                  // Replace the provisional deletion with the matched move.
                  events[i->eventIndex] = WatcherEvent(WatcherEvent::MOVE, i->path, this->getEventPath(event), false);

                  if (event->mask & IN_ISDIR)
                     for (Dir* parent : this->getDirs(event->wd))
                     {
                        bool restored = false;
                        // Reattach each copy to its own root tree. A destination
                        // may have additional owners that did not watch the source.
                        for (auto& detached : i->directories)
                           if (detached.root == parent->getRoot() && detached.directory)
                           {
                              detached.directory->move(parent, event->name);
                              detached.directory.release();
                              restored = true;
                              break;
                           }
                        if (!restored && !parent->children.contains(event->name))
                           try
                           {
                              new Dir(this, parent, event->name);
                           }
                           catch (UnableToWatchException&)
                           {
                              failedRoots.insert(parent->getRoot()->name);
                           }
                     }

                  movedFromEvents.erase(i);

                  // exit the IN_MOVED_TO process
                  goto end_moved_to;
               }
            }
            // An unmatched destination enters the watched tree. This also
            // restores watches when the move pair spans two reads.
            events << WatcherEvent(WatcherEvent::NEW, this->getEventPath(event), false);

            if (event->mask & IN_ISDIR)
               this->addChildWatches(event->wd, event->name, failedRoots);
         }

         end_moved_to:

         if (event->mask & IN_DELETE)
         {
            L_DEBU(QString("inotify event (dir): IN_DELETE (path=%1)").arg(this->getEventPath(event)));
            events << WatcherEvent(WatcherEvent::DELETED, this->getEventPath(event), false);
            if (event->mask & IN_ISDIR)
               for (Dir* parent : this->getDirs(event->wd))
                  delete parent->children.value(event->name);
         }

         if ((event->mask & IN_CREATE) && !QFileInfo(this->getEventPath(event)).isSymLink())
         {
            L_DEBU(QString("inotify event (dir): IN_CREATE (path=%1)").arg(this->getEventPath(event)));
            events << WatcherEvent(WatcherEvent::NEW, this->getEventPath(event), false);
            if (event->mask & IN_ISDIR)
               this->addChildWatches(event->wd, event->name, failedRoots);
         }

         if (event->mask & IN_CLOSE_WRITE)
         {
            L_DEBU(QString("inotify event (dir): IN_CLOSE_WRITE (path=%1)").arg(this->getEventPath(event)));
            events << WatcherEvent(WatcherEvent::CONTENT_CHANGED, this->getEventPath(event), false);
         }

         if (!dir->parent && (event->mask & IN_MOVE_SELF))
         {
            L_DEBU(QString("inotify event (dir): IN_MOVE_SELF (path=%1)").arg(this->getEventPath(event)));
            const QString path = dir->getFullPath();
            retirePaths(path);
         }
      }
      // Watched files.
      else for (File* file : this->getFiles(event->wd))
      {
         if ((event->mask & (IN_MOVE_SELF | IN_DELETE_SELF | IN_ATTRIB)) && !file->matchesPath())
         {
            // Atomic replacement can unlink an inode that is still open or
            // hard-linked elsewhere; in that case only IN_ATTRIB is reported.
            // Other hard links can still name the original inode and keep their watches.
            const QString path = file->path;
            this->rmPath(path);
            const QFileInfo info(path);
            if (info.isFile() && !info.isSymLink())
            {
               bool restored = false;
               try
               {
                  this->files.insert(path, new File(this, path));
                  restored = true;
               }
               catch (UnableToWatchException&) {}
               // Rescan catches changes made before the new watch was added.
               events << WatcherEvent(restored ? WatcherEvent::RESCAN : WatcherEvent::WATCH_LOST, path, true);
            }
            else
               events << WatcherEvent(WatcherEvent::DELETED, path, true);
            continue;
         }

         if (event->mask & IN_MODIFY)
         {
            L_DEBU(QString("inotify event (file): IN_MODIFY (path=%1)").arg(file->path));
            events << WatcherEvent(WatcherEvent::CONTENT_CHANGED, file->path, true);
         }

      }
   }

   // Unmatched moves leave the watched tree. Destroying their detached branches
   // releases their watches, without touching any replacement at the old path.
   movedFromEvents.clear();

   // Resolve owners after moves have finished: a lost watch may have been
   // temporarily detached when its terminal event arrived.
   for (int wd : lostWatches)
   {
      for (Dir* dir : this->getDirs(wd))
         failedRoots.insert(dir->getRoot()->name);
      // The kernel already removed this watch. Cleanup must not remove it again.
      this->watchReferences.remove(wd);
   }
   for (auto i = this->files.begin(); i != this->files.end();)
      if (lostWatches.contains(i.value()->wd))
      {
         events << WatcherEvent(WatcherEvent::WATCH_LOST, i.key(), true);
         delete i.value();
         i = this->files.erase(i);
      }
      else
         ++i;

   // Retire incomplete roots only after processing the batch, so pending moves
   // and directory pointers remain valid. Notify each surviving registration
   // once; FileUpdater will rescan it and switch to periodic scanning.
   for (QMutableListIterator<Dir*> i(this->dirs); i.hasNext();)
   {
      Dir* root = i.next();
      if (failedRoots.contains(root->name))
      {
         events << WatcherEvent(WatcherEvent::WATCH_LOST, root->name, false);
         delete root;
         i.remove();
      }
   }
   // Restore replacements after interpreting all events against the old trees.
   // Keep each notification's original position relative to other path changes.
   for (const PendingRestoration& pending : pathsToRestore)
   {
      const RemovedPath& registration = pending.registration;
      const QFileInfo info(QDir::cleanPath(registration.path));
      if (info.isSymLink() || !(registration.isWatchedFile ? info.isFile() : info.isDir()))
         continue;
      bool restored = false;
      try
      {
         if (registration.isWatchedFile)
            this->files.insert(registration.path, new File(this, registration.path));
         else
            this->dirs << new Dir(this, nullptr, registration.path);
         restored = true;
      }
      catch (UnableToWatchException&) {}
      events[pending.eventIndex] = WatcherEvent(restored ? WatcherEvent::RESCAN : WatcherEvent::WATCH_LOST,
         registration.path, registration.isWatchedFile);
   }
   return events;
}

int DirWatcherLinux::addWatch(const QString& path, uint32_t mask)
{
   // Strip trailing separators so neither the check nor inotify resolves a
   // directory symlink just because the caller supplied a trailing slash.
   const QString cleanPath = QDir::cleanPath(path);
   if (QFileInfo(cleanPath).isSymLink())
      throw UnableToWatchException();
   const QByteArray pathArray = cleanPath.toUtf8();

   // Adding a descendant must not replace the mask of an existing root watch.
   const int wd = inotify_add_watch(this->fileDescriptor, pathArray.constData(), mask | IN_MASK_ADD | IN_DONT_FOLLOW);

   if (wd < 0)
   {
      switch (errno)
      {
      case EACCES:
         L_ERRO(QString("inotify_add_watch: Read access to the given file is not permitted: %1").arg(path));
         break;
      case EBADF:
         L_ERRO(QString("inotify_add_watch: The given file descriptor is not valid: %1").arg(path));
         break;
      case EFAULT:
         L_ERRO(QString("inotify_add_watch: pathname points outside of the process's accessible address space: %1").arg(path));
         break;
      case EINVAL:
         L_ERRO(QString("inotify_add_watch: The given event mask contains no valid events; or fd is not an inotify file descriptor: %1").arg(path));
         break;
      case ENOENT:
         L_ERRO(QString("inotify_add_watch: A directory component in pathname does not exist or is a dangling symbolic link: %1").arg(path));
         break;
      case ENOMEM:
         L_ERRO(QString("inotify_add_watch: Insufficient kernel memory was available: %1").arg(path));
         break;
      case ENOSPC:
         L_ERRO(QString("inotify_add_watch: The user limit on the total number of inotify watches was reached or the kernel failed to allocate a needed resource: %1").arg(path));
         break;
      }
      throw UnableToWatchException();
   }

   ++this->watchReferences[wd];
   return wd;
}

void DirWatcherLinux::rmWatcher(int watcher)
{
   auto reference = this->watchReferences.find(watcher);
   if (reference == this->watchReferences.end() || --reference.value() > 0)
      return;

   this->watchReferences.erase(reference);
   // A deleted inode may already have had its watch removed by the kernel.
   if (inotify_rm_watch(this->fileDescriptor, watcher) < 0 && errno != EINVAL)
      L_WARN(QString("Unable to remove an inotify watcher."));
}

/**
  * @struct FM::DirWatcherLinux::Dir
  * Implementation of a node for the directory tree index. A node
  * represent a directory.
  */

/**
  * Contructor.
  * @param dwl     the DirWatcherLinux who use the directory tree index
  * @param parent  the parent Dir
  * @param name    the name of the Dir
  * @exception UnableToWatchException
  */
DirWatcherLinux::Dir::Dir(DirWatcherLinux* dwl, Dir* parent, const QString& name) :
   dwl(dwl), parent(parent), name(name)
{
   this->wd = dwl->addWatch(this->getFullPath(), (this->parent ? EVENTS_OBS : ROOT_EVENTS_OBS) | IN_ONLYDIR);

   for (QListIterator<QString> i(QDir(this->getFullPath()).entryList(QDir::Dirs | QDir::Hidden | QDir::NoDotAndDotDot | QDir::NoSymLinks)); i.hasNext();)
      try
      {
         new Dir(this->dwl, this, i.next());
      }
      catch (UnableToWatchException&)
      {
         for (QHashIterator<QString, Dir*> j(this->children); j.hasNext();)
         {
            auto child = j.next();
            child.value()->parent = nullptr;
            delete child.value();
         }
         this->dwl->rmWatcher(this->wd);
         throw;
      }


   if (this->parent)
      this->parent->children.insert(this->name, this);
}

/**
  * Destructor. Used to delete a branch.
  */
DirWatcherLinux::Dir::~Dir()
{
   if (this->wd >= 0)
   {
      this->dwl->rmWatcher(this->wd);

      if (this->parent)
         this->parent->children.remove(this->name);

      for (QHashIterator<QString, Dir*> i(this->children); i.hasNext();)
      {
         auto child = i.next();
         child.value()->parent = nullptr;
         delete child.value();
      }
   }
}

/**
  * Return the full path of the directory.
  * @return QString the full path
  */
QString DirWatcherLinux::Dir::getFullPath()
{
   QString fullPath = this->name;

   if (this->parent) {
      fullPath.prepend(this->parent->getFullPath().append("/"));
   }
   return fullPath;
}

DirWatcherLinux::Dir* DirWatcherLinux::Dir::getRoot()
{
   Dir* root = this;
   while (root->parent)
      root = root->parent;
   return root;
}

/**
  * Move and rename a directory in the tree without inserting an intermediate path.
  * @param to the new parent of the directory
  * @param newName the new name of the directory
  */
void DirWatcherLinux::Dir::move(Dir* to, const QString& newName)
{
   if (this->parent)
      this->parent->children.remove(this->name);
   this->parent = to;
   this->name = newName;
   to->children.insert(this->name, this);
}

/**
  * @exception UnableToWatchException
  */
DirWatcherLinux::File::File(DirWatcherLinux* dwl, const QString& path) :
   dwl(dwl), path(path)
{
   struct stat status;
   if (lstat(QFile::encodeName(path).constData(), &status) < 0 || !S_ISREG(status.st_mode))
      throw UnableToWatchException();
   this->device = status.st_dev;
   this->inode = status.st_ino;
   this->wd = dwl->addWatch(path, EVENTS_FILE);
   // Fail safely if the pathname changed while the watch was being installed.
   if (!this->matchesPath())
   {
      dwl->rmWatcher(this->wd);
      throw UnableToWatchException();
   }
}

bool DirWatcherLinux::File::matchesPath() const
{
   struct stat status;
   return lstat(QFile::encodeName(this->path).constData(), &status) == 0 &&
      S_ISREG(status.st_mode) && status.st_dev == this->device && status.st_ino == this->inode;
}

DirWatcherLinux::File::~File()
{
   if (this->wd >= 0)
   {
      this->dwl->rmWatcher(this->wd);
   }
}
