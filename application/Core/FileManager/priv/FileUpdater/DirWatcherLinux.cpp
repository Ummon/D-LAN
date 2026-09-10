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

#include <priv/FileUpdater/WaitConditionLinux.h>
#include <priv/Log.h>

#include <sys/select.h>
#include <sys/inotify.h>
#include <unistd.h>
#include <errno.h>

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
const uint32_t DirWatcherLinux::EVENTS_FILE = IN_MOVE | IN_DELETE | IN_CREATE | IN_MODIFY | IN_CLOSE_WRITE;

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

   // Close file descriptor.
   if (this->fileDescriptor >= 0 && close(this->fileDescriptor) < 0)
       L_WARN(QString("DirWatcherLinux::~DirWatcherLinux: Unable to close file descriptor (inotify)"));
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
      const QFileInfo pathInfo(path);
      if (pathInfo.isSymLink())
         return false;

      if (pathInfo.isDir())
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

DirWatcherLinux::File* DirWatcherLinux::getFile(int wd, const QString& name) const
{
   for (auto i = this->files.begin(); i != this->files.end(); ++i)
      if (i.value()->wd == wd && i.value()->currentName == name)
         return i.value();
   return nullptr;
}

DirWatcherLinux::Dir* DirWatcherLinux::getDir(int wd) const
{
   QList<Dir*> pending(this->dirs);
   while (!pending.isEmpty())
   {
      Dir* dir = pending.takeFirst();
      if (dir->wd == wd)
         return dir;

      for (auto i = dir->children.constBegin(); i != dir->children.constEnd(); ++i)
         pending << i.value();
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
      return QDir(file->parentPath).filePath(file->currentName);
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
   FD_SET(this->fileDescriptor, &fds);
   fd_max = this->fileDescriptor;

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

   char buf[BUF_LEN];
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

   QList<WatcherEvent> events;
   QList<inotify_event*> movedFromEvents;
   bool overflow = false;

   for (int i = 0; i < len;)
   {
      struct inotify_event* event = (struct inotify_event*)&buf[i];
      i += EVENT_SIZE + event->len;

      if (event->mask & IN_Q_OVERFLOW)
      {
         overflow = true;
         continue;
      }

      Dir* dir = nullptr;
      File* file = nullptr;

      if (event->mask & (IN_IGNORED | IN_UNMOUNT))
      {
         if ((dir = this->getDir(event->wd)))
         {
            Dir* root = dir;
            while (root->parent)
               root = root->parent;

            events << WatcherEvent(WatcherEvent::WATCH_LOST, root->getFullPath(), false);
            this->rmPath(root->getFullPath());
         }
         else
         {
            QList<QString> paths;
            for (auto i = this->files.constBegin(); i != this->files.constEnd(); ++i)
               if (i.value()->wd == event->wd)
                  paths << i.key();

            for (const QString& path : paths)
            {
               file = this->files.value(path);
               if (file)
                  events << WatcherEvent(WatcherEvent::WATCH_LOST,
                     QDir(file->parentPath).filePath(file->currentName), true);
               this->rmPath(path);
            }
         }
         continue;
      }

      if (!this->getDir(event->wd) && (event->mask & IN_MOVED_TO))
      {
         bool matched = false;
         for (QMutableListIterator<inotify_event*> i(movedFromEvents); i.hasNext();)
         {
            inotify_event* fromEvent = i.next();
            File* movedFile = this->getFile(fromEvent->wd, fromEvent->name);
            if (movedFile && fromEvent->cookie == event->cookie && fromEvent->wd == event->wd)
            {
               const QString oldPath = QDir(movedFile->parentPath).filePath(movedFile->currentName);
               movedFile->currentName = event->name;
               const QString newPath = QDir(movedFile->parentPath).filePath(movedFile->currentName);
               events << WatcherEvent(WatcherEvent::MOVE, oldPath, newPath, true);
               i.remove();
               matched = true;
               break;
            }
         }
         if (matched)
            continue;
      }

      if (!this->getDir(event->wd) && !event->len &&
          (event->mask & (IN_DELETE_SELF | IN_MOVE_SELF)))
      {
         QList<QString> paths;
         for (auto i = this->files.constBegin(); i != this->files.constEnd(); ++i)
            if (i.value()->wd == event->wd)
               paths << i.key();

         for (const QString& path : paths)
         {
            file = this->files.value(path);
            if (file)
               events << WatcherEvent(WatcherEvent::WATCH_LOST,
                  QDir(file->parentPath).filePath(file->currentName), true);
            this->rmPath(path);
         }
         continue;
      }

      // Watched directories.
      if (dir = this->getDir(event->wd))
      {
         if (event->mask & IN_MOVED_FROM)
         {
            L_DEBU(QString("inotify event (dir): IN_MOVED_FROM (path=%1)").arg(this->getEventPath(event)));
            // Add the event to movedToEvents.
            movedFromEvents << event;
         }

         if (event->mask & IN_MOVED_TO)
         {
            L_DEBU(QString("inotify event (dir): IN_MOVED_TO (path=%1)").arg(this->getEventPath(event)));
            // Check list of IN_MOVED_FROM events.
            for (QMutableListIterator<inotify_event*> i(movedFromEvents); i.hasNext();)
            {
               struct inotify_event *fromEvent = i.next();
               if (fromEvent->cookie == event->cookie)
               {
                  // If an IN_MOVES_FROM event is linked, create a MOVE WatcherEvent.
                  events << WatcherEvent(WatcherEvent::MOVE, this->getEventPath(fromEvent), this->getEventPath(event), false);

                  // If moved object is a directory, apply change to the local directory index
                  if (event->mask & IN_ISDIR)
                  {
                     // Retrieve moved directory by child map of from directory,
                     // because actually the name hasn't changed.
                     Dir* fromDir = this->getDir(fromEvent->wd);
                     Dir* movedDir = fromDir ? fromDir->children.value(fromEvent->name) : nullptr;

                     // If the name of moved directory has changed, rename it.
                     if (movedDir && fromEvent->name != event->name)
                        movedDir->rename(event->name);

                     // If the path of moved directory has changed, move it.
                     if (movedDir && movedDir->parent->getFullPath() != dir->getFullPath())
                        movedDir->move(dir);
                  }

                  for (auto file = this->files.begin(); file != this->files.end(); ++file)
                     if (file.value()->wd == fromEvent->wd && file.value()->currentName == fromEvent->name)
                        file.value()->currentName = event->name;

                  i.remove();

                  // exit the IN_MOVED_TO process
                  goto end_moved_to;
               }
            }
            // if no IN_MOVED_FROM event is linked, create a NEW WatcherEvent.
            // IN_MOVED_FROM event without IN_MOVE_TO event have to be processed at
            // the end of the loop, when every IN_MOVED_TO event is processed.
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

         if (event->mask & IN_CREATE)
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

         if (event->mask & IN_DELETE_SELF || event->mask & IN_MOVE_SELF)
         {
            L_DEBU(QString("inotify event (dir): IN_DELETE_SELF || IN_MOVE_SELF (path=%1)").arg(this->getEventPath(event)));
            // processed only for ROOT directory
            events << WatcherEvent(WatcherEvent::DELETED, this->getEventPath(event), false);
            this->rmPath(this->getEventPath(event));
         }
      }
      // Watched files are watched through their parent directory. Ignore events for
      // unrelated names when no directory watcher owns the same descriptor.
      else if (event->len && (file = this->getFile(event->wd, event->name)))
      {
         if (event->mask & IN_MOVED_FROM)
         {
            L_DEBU(QString("inotify event (file): IN_MOVED_FROM (path=%1)").arg(this->getEventPath(event)));
            movedFromEvents << event;
         }

         if (event->mask & IN_MODIFY)
         {
            L_DEBU(QString("inotify event (file): IN_MODIFY (path=%1)").arg(this->getEventPath(event)));
            events << WatcherEvent(WatcherEvent::CONTENT_CHANGED, this->getEventPath(event), true);
         }

         if (event->mask & IN_CLOSE_WRITE)
         {
            L_DEBU(QString("inotify event (file): IN_CLOSE_WRITE (path=%1)").arg(this->getEventPath(event)));
            events << WatcherEvent(WatcherEvent::CONTENT_CHANGED, this->getEventPath(event), true);
         }

         if (event->mask & (IN_CREATE | IN_MOVED_TO))
         {
            L_DEBU(QString("inotify event (file): IN_CREATE || IN_MOVED_TO (path=%1)").arg(this->getEventPath(event)));
            events << WatcherEvent(WatcherEvent::NEW, this->getEventPath(event), true);
         }

         if (event->mask & IN_DELETE)
         {
            const QString path = this->getEventPath(event);
            L_DEBU(QString("inotify event (file): IN_DELETE (path=%1)").arg(path));
            events << WatcherEvent(WatcherEvent::DELETED, path, true);
         }
      }
   }

   // Because every IN_MOVED_FROM event with a linked IN_MOVED_TO event was removed of
   // the list, it contains only alone IN_MOVED_FROM event.
   for (QMutableListIterator<struct inotify_event*> i(movedFromEvents); i.hasNext();)
   {
      struct inotify_event* e = i.next();
      if (Dir* parent = this->getDir(e->wd))
      {
         const QString path = this->getEventPath(e);
         events << WatcherEvent(WatcherEvent::DELETED, path, false);
         delete parent->children.value(e->name);
      }
      else if (File* file = this->getFile(e->wd, e->name))
      {
         const QString path = QDir(file->parentPath).filePath(file->currentName);
         events << WatcherEvent(WatcherEvent::DELETED, path, true);
      }
   }

   if (overflow)
   {
      events.clear();
      for (Dir* dir : this->dirs)
         events << WatcherEvent(WatcherEvent::RESCAN, dir->getFullPath(), false);
      for (auto i = this->files.constBegin(); i != this->files.constEnd(); ++i)
         events << WatcherEvent(WatcherEvent::RESCAN, i.key(), true);
   }

   return events;
}

int DirWatcherLinux::addWatch(int fileDescriptor, const QString& path, uint32_t mask)
{
   const QByteArray& pathArray = path.toUtf8();

   const int wd = inotify_add_watch(fileDescriptor, pathArray.constData(), mask);

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

   return wd;
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
   this->wd = addWatch(dwl->fileDescriptor, this->getFullPath(), (this->parent ? EVENTS_OBS : ROOT_EVENTS_OBS));

   for (QListIterator<QString> i(QDir(this->getFullPath()).entryList(
      QDir::Dirs | QDir::NoDotAndDotDot | QDir::NoSymLinks)); i.hasNext();)
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

         if (this->wd >= 0)
         {
            inotify_rm_watch(this->dwl->fileDescriptor, this->wd);
            this->wd = -1;
         }
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
      if (inotify_rm_watch(this->dwl->fileDescriptor, this->wd))
         L_WARN(QString("Dir::~Dir: Unable to remove an inotify watcher."));

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
  * Rename the directory.
  * @param newName the new name
  */
void DirWatcherLinux::Dir::rename(const QString& newName)
{
   this->parent->children.remove(this->name);
   this->name = newName;
   this->parent->children.insert(this->name, this);
}

/**
  * Move a directory in the tree.
  * @param to the new parent of the directory
  */
void DirWatcherLinux::Dir::move(Dir* to)
{
   this->parent->children.remove(this->name);
   this->parent = to;
   to->children.insert(this->name, this);
}

/**
  * @exception UnableToWatchException
  */
DirWatcherLinux::File::File(DirWatcherLinux* dwl, const QString& path) :
   dwl(dwl), path(path), parentPath(QFileInfo(path).absolutePath()), currentName(QFileInfo(path).fileName())
{
   this->wd = addWatch(dwl->fileDescriptor, this->parentPath, EVENTS_FILE | IN_MASK_ADD);
}

DirWatcherLinux::File::~File()
{
   bool watchIsShared = this->dwl->getDir(this->wd) != nullptr;
   for (auto i = this->dwl->files.constBegin(); i != this->dwl->files.constEnd(); ++i)
      if (i.value() != this && i.value()->wd == this->wd)
         watchIsShared = true;

   if (this->wd >= 0 && !watchIsShared)
   {
      if (inotify_rm_watch(this->dwl->fileDescriptor, this->wd))
         L_WARN(QString("File::~File: Unable to remove an inotify watcher."));
   }
}
