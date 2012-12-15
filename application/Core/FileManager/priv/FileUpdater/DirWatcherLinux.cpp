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
#include <errno.h>

/**
  * @class FM::DirWatcherLinux
  * @author Hervé Martinet
  *
  * Implementation of 'DirWatcher' for the linux platform with inotify.
  */

const int DirWatcherLinux::EVENT_SIZE = (sizeof (struct inotify_event));
const size_t DirWatcherLinux::BUF_LEN = (1024 * (EVENT_SIZE + 16));
const uint32_t DirWatcherLinux::EVENTS_OBS = IN_MOVE|IN_DELETE|IN_CREATE|IN_CLOSE_WRITE;
const uint32_t DirWatcherLinux::ROOT_EVENTS_OBS = EVENTS_OBS|IN_MOVE_SELF|IN_DELETE_SELF;

class UnableToWatchException {};

/**ath=/home/gburri/Downloads//Leonard - 35 albums/07 - Y a-t-il un gÃ©nie dans la salle
  * Constructor.
  */
DirWatcherLinux::DirWatcherLinux()
   : mutex(QMutex::Recursive)
{
   // Initialize inotify
   this->initialized = true;
   this->fileDescriptor = inotify_init();
   if (fileDescriptor < 0) {
      L_WARN(QString("Unable to initialize inotify, DirWatcher not used."));
      this->initialized = false;
   }
}

/**
  * Destructor.
  */
DirWatcherLinux::~DirWatcherLinux ()
{
   QMutexLocker locker(&this->mutex);

   for (QMutableListIterator<Dir*> i(rootDirs); i.hasNext();)
   {
      Dir* dir = i.next();
      delete dir;
      i.remove();
   }

   // Close file descriptor
   if (close(this->fileDescriptor) < 0) {
       L_WARN(QString("DirWatcherLinux::~DirWatcherLinux : Unable to close file descriptor (inotify)."));
   }
}

/**
  * @copydoc FM::DirWatcher::addDir(..)
  */
bool DirWatcherLinux::addDir(const QString& path)
{
   QMutexLocker locker(&this->mutex);

   if (!this->initialized) return false;

   try
   {
      Dir* dir = new Dir(this, nullptr, path);
      rootDirs << dir;
      return true;
   }
   catch (UnableToWatchException&)
   {
      return false;
   }
}

/**
  * @copydoc FM::DirWatcher::rmDir(..)
  */
void DirWatcherLinux::rmDir(const QString& path)
{
   QMutexLocker locker(&this->mutex);

   for (QMutableListIterator<Dir*> i(rootDirs); i.hasNext();)
   {
      Dir* dir = i.next();
      if (dir->name == path)
      {
         delete dir;
         i.remove();
         break;
      }
   }
}

/**
  * Return the full path of the file notified by an inotify event.
  * @param path the full path
  */
QString DirWatcherLinux::getEventPath(inotify_event* event)
{
   QMutexLocker locker(&this->mutex);

   QString p = this->dirs.value(event->wd)->getFullPath();
   if (event->len)
      p.append('/').append(event->name);

   return p;
}

/**
  * @copydoc FM::DirWatcher::nbWatchedDir()
  */
int DirWatcherLinux::nbWatchedDir()
{
   QMutexLocker locker(&this->mutex);
   return this->rootDirs.size();
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
      L_DEBU(QString("DirWatcherLinux::waitEvent : add WaitCondition(fd=%1) to select fd_set").arg(wcfd));
      FD_SET(wcfd, &fds);
      if (wcfd > fd_max) fd_max = wcfd;
   }

   // Active select to wait events in unlocked mode.
   L_DEBU("DirWatcherLinux::waitEvent : active select");
   locker.unlock();
   int sel = select(fd_max + 1, &fds, NULL, NULL, (timeout==-1 ? 0 : &time));
   locker.relock();

   if (sel < 0)
   {
      L_ERRO(QString("DirWatcherLinux::waitEvent : select error."));
      return QList<WatcherEvent>();
   }
   else if (!sel)
   {
      // select is released by timeout.
      L_DEBU("DirWatcherLinux::waitEvent : exit select by timeout");
      QList<WatcherEvent> events;
      events.append(WatcherEvent(WatcherEvent::TIMEOUT));
      return events;
   }

   // Test if select is released by a WaitCondition.
   for (int i = 0; i < ws.size(); i++)
   {
      int wcfd = dynamic_cast<WaitConditionLinux*>(ws[i])->getFd();
      if (FD_ISSET(wcfd, &fds))
      {
         L_DEBU(QString("DirWatcherLinux::waitEvent : exit select by WaitCondition release (fd=%1)").arg(wcfd));
         static char dummy[4096];
         while (read(wcfd, dummy, sizeof(dummy)) > 0);
         return QList<WatcherEvent>();
      }
   }

   L_DEBU("DirWatcherLinux::waitEvent : exit select by inotify");

   char buf[BUF_LEN];
   int len = read(this->fileDescriptor, buf, BUF_LEN);
   if (len < 0)
   {
      if (errno == EINTR)
         // Need to reissue system call.
         return QList<WatcherEvent>();
      else
         L_ERRO(QString("DirWatcherLinux::waitEvent : read inotify event failed."));
   }
   else if (!len)
   {
      L_ERRO(QString("DirWatcherLinux::waitEvent : BUF_LEN to small?"));
   }

   QList<WatcherEvent> events;
   QList<inotify_event*> movedFromEvents;

   for (int i = 0; i < len;)
   {
      struct inotify_event* event = (struct inotify_event*)&buf[i];
      i += EVENT_SIZE + event->len;

      if (!this->dirs.contains(event->wd))
         continue;

      if (event->mask & IN_MOVED_FROM)
      {
         L_DEBU(QString("inotify event: IN_MOVED_FROM (path=%1)").arg(this->getEventPath(event)));
         // Add the event to movedToEvents.
         movedFromEvents << event;
      }

      if (event->mask & IN_MOVED_TO)
      {
         L_DEBU(QString("inotify event: IN_MOVED_TO (path=%1)").arg(this->getEventPath(event)));
         // Check list of IN_MOVED_FROM events.
         for (QMutableListIterator<inotify_event*> i(movedFromEvents); i.hasNext();)
         {
            struct inotify_event *fromEvent = i.next();
            if (fromEvent->cookie == event->cookie)
            {
               // If an IN_MOVES_FROM event is linked, create a MOVE WatcherEvent.
               events << WatcherEvent(WatcherEvent::MOVE, getEventPath(fromEvent), this->getEventPath(event));

               // If moved object is a directory, apply change to the local directory index
               if (event->mask & IN_ISDIR)
               {
                  // Retrieve to directory by watch descriptor.
                  Dir* toDir = this->dirs.value(event->wd);

                  // Retrieve moved directory by child map of from directory,
                  // because actually the name hasn't changed.
                  Dir* movedDir = this->dirs.value(fromEvent->wd)->childs.value(fromEvent->name);

                  // If the name of moved directory has changed, rename it.
                  if (movedDir && fromEvent->name != event->name)
                     movedDir->rename(event->name);

                  // If the path of moved directory has changed, move it.
                  if (movedDir && toDir && movedDir->parent->getFullPath() != toDir->getFullPath())
                     movedDir->move(toDir);
               }

               i.remove();

               // exit the IN_MOVED_TO process
               goto end_moved_to;
            }
         }
         // if no IN_MOVED_FROM event is linked, create a NEW WatcherEvent.
         // IN_MOVED_FROM event without IN_MOVE_TO event have to be processed at
         // the end of the loop, when every IN_MOVED_TO event is processed.
         events << WatcherEvent(WatcherEvent::NEW, this->getEventPath(event));

         if (event->mask & IN_ISDIR)
            try
            {
               new Dir(this, this->dirs.value(event->wd), event->name);
            }
            catch (UnableToWatchException&) {}
      }

      end_moved_to:

      if (event->mask & IN_DELETE)
      {
         L_DEBU(QString("inotify event: IN_DELETE (path=%1)").arg(this->getEventPath(event)));
         events << WatcherEvent(WatcherEvent::DELETED, this->getEventPath(event));
         if (event->mask & IN_ISDIR)
            delete this->dirs.value(event->wd)->childs.value(event->name);
      }

      if (event->mask & IN_CREATE)
      {
         L_DEBU(QString("inotify event: IN_CREATE (path=%1)").arg(this->getEventPath(event)));
         events << WatcherEvent(WatcherEvent::NEW, this->getEventPath(event));
         if (event->mask & IN_ISDIR)
            try
            {
               new Dir(this, this->dirs.value(event->wd), event->name);
            }
            catch (UnableToWatchException&) {}
      }

      if (event->mask & IN_CLOSE_WRITE)
      {
         L_DEBU(QString("inotify event: IN_CLOSE_WRITE (path=%1)").arg(this->getEventPath(event)));
         events << WatcherEvent(WatcherEvent::CONTENT_CHANGED, this->getEventPath(event));
      }

      if (event->mask & IN_DELETE_SELF || event->mask & IN_MOVE_SELF)
      {
         L_DEBU(QString("inotify event: IN_DELETE_SELF || IN_MOVE_SELF (path=%1)").arg(this->getEventPath(event)));
         // processed only for ROOT directory
         events << WatcherEvent(WatcherEvent::DELETED, this->getEventPath(event));
         dirs.value(event->wd)->dwl->rmDir(this->getEventPath(event));
      }
   }

   // Cause every IN_MOVED_FROM event with a linked IN_MOVED_TO event was removed of
   // the list, it contains only alone IN_MOVED_FROM event.
   for (QMutableListIterator<struct inotify_event*> i(movedFromEvents); i.hasNext();)
   {
      struct inotify_event* e = i.next();
      events << WatcherEvent(WatcherEvent::DELETED, this->getEventPath(e));
   }

   return events;
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
   const QByteArray& array = this->getFullPath().toUtf8();

   this->wd = inotify_add_watch(
      dwl->fileDescriptor,
      array.constData(),
      (this->parent ? EVENTS_OBS : ROOT_EVENTS_OBS)
   );

   if (this->wd < 0)
   {
      switch (errno)
      {
      case EACCES:
         L_ERRO(QString("inotify_add_watch: Read access to the given file is not permitted: %1").arg(this->getFullPath()));
         break;
      case EBADF:
         L_ERRO(QString("inotify_add_watch: The given file descriptor is not valid: %1").arg(this->getFullPath()));
         break;
      case EFAULT:
         L_ERRO(QString("inotify_add_watch: pathname points outside of the process's accessible address space: %1").arg(this->getFullPath()));
         break;
      case EINVAL:
         L_ERRO(QString("inotify_add_watch: The given event mask contains no valid events; or fd is not an inotify file descriptor: %1").arg(this->getFullPath()));
         break;
      case ENOENT:
         L_ERRO(QString("inotify_add_watch: A directory component in pathname does not exist or is a dangling symbolic link: %1").arg(this->getFullPath()));
         break;
      case ENOMEM:
         L_ERRO(QString("inotify_add_watch: Insufficient kernel memory was available: %1").arg(this->getFullPath()));
         break;
      case ENOSPC:
         L_ERRO(QString("inotify_add_watch: The user limit on the total number of inotify watches was reached or the kernel failed to allocate a needed resource: %1").arg(this->getFullPath()));
         break;
      }
      throw UnableToWatchException();
   }

   for (QListIterator<QString> i(QDir(this->getFullPath()).entryList(QDir::Dirs | QDir::NoDotAndDotDot)); i.hasNext();)
      try
      {
         new Dir(this->dwl, this, i.next());
      }
      catch (UnableToWatchException&)
      {
         for (QMapIterator<QString, Dir*> j(this->childs); j.hasNext();)
         {
            auto child = j.next();
            child.value()->parent = nullptr;
            delete child.value();
         }
         throw;
      }


   if (this->parent)
      this->parent->childs.insert(this->name, this);

   dwl->dirs.insert(this->wd, this);
}

/**
  * Destructor. Used to delete a branch.
  */
DirWatcherLinux::Dir::~Dir()
{
   if (this->wd >= 0)
   {
      this->dwl->dirs.remove(this->wd);
      if (inotify_rm_watch(this->dwl->fileDescriptor, this->wd))
         L_WARN(QString("Dir::~Dir : Unable to remove an inotify watcher."));
   
      if (this->parent)
         this->parent->childs.remove(this->name);

      for (QMapIterator<QString, Dir*> i(this->childs); i.hasNext();)
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
   this->parent->childs.remove(this->name);
   this->name = newName;
   this->parent->childs.insert(this->name, this);
}

/**
  * Move a directory in the tree.
  * @param to the new parent of the directory
  */
void DirWatcherLinux::Dir::move(Dir* to)
{
   this->parent->childs.remove(this->name);
   this->parent = to;
   to->childs.insert(this->name, this);
}
