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
#include <QFileInfo>

#include <priv/FileUpdater/WaitConditionLinux.h>
#include <priv/Log.h>

#include <sys/select.h>
#include <sys/inotify.h>
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
const uint32_t DirWatcherLinux::EVENTS_FILE = IN_MODIFY | IN_MOVE_SELF | IN_DELETE_SELF;

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
         this->files.insert(path, file);

      }
      return true;
   }
   catch (UnableToWatchException&)
   {
      return false;
   }
}

/**
  * Return 'nullptr' if not found.
  */
DirWatcherLinux::File* DirWatcherLinux::getFile(int wd) const
{
   for (auto i = this->files.begin(); i != this->files.end(); ++i)
      if (i.value()->wd == wd)
         return i.value();
   return nullptr;
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

/**
  * Return the full path of the file notified by an inotify event.
  * Return a null QString if not found.
  * @param path the full path
  */
QString DirWatcherLinux::getEventPath(inotify_event* event)
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

   // Event for a watched file.
   File* file = this->getFile(event->wd);
   if (file)
   {
      for (auto i = this->files.constBegin(); i != this->files.constEnd(); ++i)
         if (i.value()->wd == event->wd)
            return i.key();
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

   fd_set fds;
   int fd_max;
   struct timeval time;

   // Convert timeout in timeval.
   time.tv_sec = timeout / 1000;
   time.tv_usec = (timeout % 1000) * 1000;

   // Zero-out the fd_set.
   FD_ZERO(&fds);

   // Add the inotify fd to the fd_set.
   fd_max = -1;
   if (this->initialized)
   {
      FD_SET(this->fileDescriptor, &fds);
      fd_max = this->fileDescriptor;
   }

   // Add fd for all WaitCondition in fd_set and ajust fd_max if needed.
   for (int i = 0; i < ws.size(); i++)
   {
      int wcfd = dynamic_cast<WaitConditionLinux*>(ws[i])->getFd();
      L_DEBU(QString("DirWatcherLinux::waitEvent: add WaitCondition(fd=%1) to select fd_set").arg(wcfd));
      FD_SET(wcfd, &fds);
      if (wcfd > fd_max)
         fd_max = wcfd;
   }

   // Active select to wait events in unlocked mode.
   L_DEBU("DirWatcherLinux::waitEvent: active select");
   locker.unlock();
   int sel = select(fd_max + 1, &fds, NULL, NULL, (timeout==-1 ? 0 : &time));
   locker.relock();

   if (sel < 0)
   {
      L_ERRO(QString("DirWatcherLinux::waitEvent: select error."));
      return QList<WatcherEvent>();
   }
   else if (!sel)
   {
      // select is released by timeout.
      L_DEBU("DirWatcherLinux::waitEvent: exit select by timeout");
      QList<WatcherEvent> events;
      events << WatcherEvent(WatcherEvent::TIMEOUT, false);
      return events;
   }

   // Test if select is released by a WaitCondition.
   for (int i = 0; i < ws.size(); i++)
   {
      int wcfd = dynamic_cast<WaitConditionLinux*>(ws[i])->getFd();
      if (FD_ISSET(wcfd, &fds))
      {
         L_DEBU(QString("DirWatcherLinux::waitEvent: exit select by WaitCondition release (fd=%1)").arg(wcfd));
         static char dummy[4096];
         while (read(wcfd, dummy, sizeof(dummy)) > 0);
         return QList<WatcherEvent>();
      }
   }

   L_DEBU("DirWatcherLinux::waitEvent: exit select by inotify");

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
   struct PendingMove
   {
      uint32_t cookie;
      QString path;
      qsizetype eventIndex;
      std::unique_ptr<Dir> directory;
   };
   std::vector<PendingMove> movedFromEvents;

   for (int i = 0; i < len;)
   {
      struct inotify_event* event = (struct inotify_event*)&buf[i];
      i += EVENT_SIZE + event->len;

      Dir* dir = nullptr;
      File* file = nullptr;

      // Watched directories.
      if (dir = this->getDir(event->wd))
      {
         if (event->mask & IN_MOVED_FROM)
         {
            L_DEBU(QString("inotify event (dir): IN_MOVED_FROM (path=%1)").arg(this->getEventPath(event)));
            const QString path = this->getEventPath(event);
            // Hide the old subtree immediately, including from later events in
            // this read. Keep ownership until the move is matched or abandoned.
            Dir* movedDir = event->mask & IN_ISDIR ? dir->children.take(event->name) : nullptr;
            if (movedDir)
               movedDir->parent = nullptr;
            movedFromEvents.push_back({event->cookie, path, events.size(), std::unique_ptr<Dir>(movedDir)});
            // Preserve ordering if the old path is recreated before this read ends.
            events << WatcherEvent(WatcherEvent::DELETED, path, false);
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

                  if (i->directory)
                  {
                     i->directory->move(dir, event->name);
                     i->directory.release(); // The destination tree now owns it.
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
               try
               {
                  new Dir(this, dir, event->name);
               }
               catch (UnableToWatchException&) {}
         }

         end_moved_to:

         if (event->mask & IN_DELETE)
         {
            L_DEBU(QString("inotify event (dir): IN_DELETE (path=%1)").arg(this->getEventPath(event)));
            events << WatcherEvent(WatcherEvent::DELETED, this->getEventPath(event), false);
            if (event->mask & IN_ISDIR)
               delete dir->children.value(event->name);
         }

         if ((event->mask & IN_CREATE) && !QFileInfo(this->getEventPath(event)).isSymLink())
         {
            L_DEBU(QString("inotify event (dir): IN_CREATE (path=%1)").arg(this->getEventPath(event)));
            events << WatcherEvent(WatcherEvent::NEW, this->getEventPath(event), false);
            if (event->mask & IN_ISDIR)
               try
               {
                  new Dir(this, dir, event->name);
               }
               catch (UnableToWatchException&) {}
         }

         if (event->mask & IN_CLOSE_WRITE)
         {
            L_DEBU(QString("inotify event (dir): IN_CLOSE_WRITE (path=%1)").arg(this->getEventPath(event)));
            events << WatcherEvent(WatcherEvent::CONTENT_CHANGED, this->getEventPath(event), false);
         }

         if (!dir->parent && (event->mask & IN_DELETE_SELF || event->mask & IN_MOVE_SELF))
         {
            L_DEBU(QString("inotify event (dir): IN_DELETE_SELF || IN_MOVE_SELF (path=%1)").arg(this->getEventPath(event)));
            // processed only for ROOT directory
            events << WatcherEvent(WatcherEvent::DELETED, this->getEventPath(event), false);
            this->rmPath(this->getEventPath(event));
         }
      }
      // Watched files.
      else if (file = this->getFile(event->wd))
      {
         if (event->mask & IN_MOVE_SELF)
         {
            L_DEBU(QString("inotify event (file): IN_MOVE_SELF (path=%1)").arg(this->getEventPath(event)));
            // TODO
         }

         if (event->mask & IN_MODIFY)
         {
            L_DEBU(QString("inotify event (file): IN_MODIFY (path=%1)").arg(this->getEventPath(event)));
            events << WatcherEvent(WatcherEvent::CONTENT_CHANGED, this->getEventPath(event), true);
         }

         if (event->mask & IN_DELETE_SELF)
         {
            const QString& path = this->getEventPath(event);
            L_DEBU(QString("inotify event (file): IN_DELETE_SELF (path=%1)").arg(path));
            events << WatcherEvent(WatcherEvent::DELETED, path, true);
            this->rmPath(path);
         }
      }
   }

   // Unmatched moves leave the watched tree. Destroying their detached branches
   // releases their watches, without touching any replacement at the old path.
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

   for (QListIterator<QString> i(QDir(this->getFullPath()).entryList(QDir::Dirs | QDir::NoDotAndDotDot | QDir::NoSymLinks)); i.hasNext();)
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
   this->wd = dwl->addWatch(path, EVENTS_FILE);
}

DirWatcherLinux::File::~File()
{
   if (this->wd >= 0)
   {
      this->dwl->rmWatcher(this->wd);
   }
}
