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
  
#include <QtCore/QtCore> // For the Q_OS_* defines.

#ifdef Q_OS_LINUX

#include <priv/FileUpdater/DirWatcherLinux.h>
using namespace FM;

#include <QMutexLocker>
#include <QtCore/QDebug>

#include <priv/FileUpdater/WaitConditionLinux.h>
#include <priv/Log.h>

#include <sys/select.h>
#include <sys/inotify.h>
#include <errno.h>

/**
 * @class DirWatcherLinux
 * @author Hervé Martinet
 *
 * Implementation of 'DirWatcher' for the linux platform with inotify.
 */

const int DirWatcherLinux::EVENT_SIZE = (sizeof (struct inotify_event));
const size_t DirWatcherLinux::BUF_LEN = (1024 * (EVENT_SIZE + 16));
const uint32_t DirWatcherLinux::EVENTS_OBS = IN_MOVE|IN_DELETE|IN_CREATE|IN_CLOSE_WRITE;
const uint32_t DirWatcherLinux::ROOT_EVENTS_OBS = EVENTS_OBS|IN_MOVE_SELF|IN_DELETE_SELF;


/**
 * Constructor.
 */
DirWatcherLinux::DirWatcherLinux()
   : mutex(QMutex::Recursive)
{
   // Initialize inotify
   initialized = true;
   fileDescriptor = inotify_init();
   if (fileDescriptor < 0) {
      L_WARN(QString("Unable to initialize inotify, DirWatcher not used."));
      initialized = false;
   }
}

/**
 * Destructor.
 */
DirWatcherLinux::~DirWatcherLinux ()
{
   QMutexLocker locker(&this->mutex);

   // Close file descriptor
   if (close(fileDescriptor) < 0) {
       L_ERRO(QString("DirWatcherLinux::~DirWatcherLinux : Unable to close file descriptor (inotify)."));
   }
}

/**
 * @inheritDoc
 */
bool DirWatcherLinux::addDir(const QString& path)
{
   QMutexLocker locker(&this->mutex);

   if (!initialized) return false;

   Dir* dir = new Dir(this, NULL, path);
   if (dir->wd < 0) return false;
   rootDirs.insert(path, dir);

   return true;
}

/**
 * @inheritDoc
 */
void DirWatcherLinux::rmDir(const QString& path)
{
   QMutexLocker locker(&this->mutex);

   Dir* dir = rootDirs.take(path);
   delete dir;
}

/**
 * Return the full path of the file notified by an inotify event.
 * @param path the full path
 */
QString DirWatcherLinux::getEventPath(inotify_event *event) {
   QMutexLocker locker(&this->mutex);

   QString p = dirs.value(event->wd)->getFullPath();
   if (event->len)
      p.append('/').append(event->name);

   return p;
}

/**
 * @inheritDoc
 */
int DirWatcherLinux::nbWatchedDir()
{
   QMutexLocker locker(&this->mutex);
   return this->rootDirs.size();
}

/**
 * @inheritDoc
 */
const QList<WatcherEvent> DirWatcherLinux::waitEvent(QList<WaitCondition*> ws)
{
   return this->waitEvent(-1, ws);
}

/**
 * @inheritDoc
 */
const QList<WatcherEvent> DirWatcherLinux::waitEvent(int timeout, QList<WaitCondition*> ws)
{
   QMutexLocker locker(&this->mutex);

   fd_set fds;
   int fd_max;
   char buf[BUF_LEN];
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
   bool wsReleased = false;
   for (int i = 0; i < ws.size(); i++)
   {
      int wcfd = dynamic_cast<WaitConditionLinux*>(ws[i])->getFd();
      if(FD_ISSET(wcfd, &fds))
      {
         L_DEBU(QString("DirWatcherLinux::waitEvent : exit select by WaitCondition release (fd=%1)").arg(wcfd));
         static char dummy[4096];
         while (read(wcfd, dummy, sizeof(dummy)) > 0);
         wsReleased = true;
      }
   }
   if (wsReleased) return QList<WatcherEvent>();

   L_DEBU("DirWatcherLinux::waitEvent : exit select by inotify");
   int len = read(fileDescriptor, buf, BUF_LEN);
   if (len < 0)
   {
      if (errno == EINTR)
         /* Need to reissue system call */
         return QList<WatcherEvent>();
      else
         L_ERRO(QString("DirWatcherLinux::waitEvent : read inotify event failed."));
   }
   else if (!len)
         L_ERRO(QString("DirWatcherLinux::waitEvent : BUF_LEN to small ?"));

   int i = 0;
   QList<WatcherEvent> events;
   QList<inotify_event*> movedFromEvents;
   while (i < len)
   {
      struct inotify_event *event;

      event = (struct inotify_event *) &buf[i];

      if (event->mask & IN_MOVED_FROM)
      {
         // Add the event to movedToEvents.
         movedFromEvents << event;
      }
      if (event->mask & IN_MOVED_TO)
      {
         // Check list of IN_MOVED_FROM events.
         for (QMutableListIterator<inotify_event*> i(movedFromEvents); i.hasNext();)
         {
            struct inotify_event *fromEvent = i.next();
            if (fromEvent->cookie == event->cookie)
            {
               // If an IN_MOVES_FROM event is linked, create a MOVE WatcherEvent.
               events << WatcherEvent(WatcherEvent::MOVE, getEventPath(fromEvent), getEventPath(event));

               // If moved object is a directory, apply change to the local directory index
               if (event->mask & IN_ISDIR)
               {
                  // Retrieve to directory by watch descriptor.
                  Dir* toDir = dirs.value(event->wd);

                  // Retrieve moved directory by child map of from directory,
                  // because actually the name hasn't changed.
                  Dir* movedDir = dirs.value(fromEvent->wd)->childs.value(fromEvent->name);

                  // If the name of moved directory has changed, rename it.
                  if (fromEvent->name != event->name)
                     movedDir->rename(event->name);

                  // If the path of moved directosy has changed, move it.
                  if (movedDir->parent->getFullPath() != toDir->getFullPath())
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
         events << WatcherEvent(WatcherEvent::NEW, getEventPath(event));
      }
      end_moved_to:
      if (event->mask & IN_DELETE)
      {
         events << WatcherEvent(WatcherEvent::DELETED, getEventPath(event));
         if (event->mask & IN_ISDIR)
            delete dirs.value(event->wd)->childs.value(event->name);
      }
      if (event->mask & IN_CREATE)
      {
         events << WatcherEvent(WatcherEvent::NEW, getEventPath(event));
         if (event->mask & IN_ISDIR)
            new Dir(this, dirs.value(event->wd), event->name);
      }
      if (event->mask & IN_CLOSE_WRITE)
         events << WatcherEvent(WatcherEvent::CONTENT_CHANGED, getEventPath(event));
//         if (event->mask & IN_MOVE_SELF)
//            // TODO: Process only for root directory move.
//         if (event->mask & IN_DELETE_SELF)
//            // TODO: Process only for root directory delete.

      i += EVENT_SIZE + event->len;
   }

   // Cause every IN_MOVED_FROM event with a linked IN_MOVED_TO event was removed of
   // the list, it contains only alone IN_MOVED_FROM event.
   for (QMutableListIterator<struct inotify_event*> i(movedFromEvents); i.hasNext();)
   {
      struct inotify_event *e = i.next();
      events << WatcherEvent(WatcherEvent::DELETED, getEventPath(e));
   }

   return events;
}

/**
 * @struct Dir
 * Implementation of a node for the directory tree index. A node
 * represent a directory.
 */

/**
 * Contructor.
 * @param dwl     the DirWatcherLinux who use the directory tree index
 * @param parent  the parent Dir
 * @param name    the name of the Dir
 */
DirWatcherLinux::Dir::Dir(DirWatcherLinux* dwl, Dir* parent, const QString& name) : dwl(dwl), parent(parent), name(name)
{
   if (this->parent)
      this->parent->childs.insert(this->name, this);

   this->wd = inotify_add_watch(
         dwl->fileDescriptor,
         getFullPath().toStdString().c_str(),
         (this->parent ? EVENTS_OBS : ROOT_EVENTS_OBS));
   if (wd < 0) return;
   dwl->dirs.insert(wd, this);

   for (QListIterator<QString> i(QDir(this->getFullPath()).entryList(QDir::Dirs | QDir::NoDotAndDotDot)); i.hasNext();)
      new Dir(this->dwl, this, (QString) i.next());
}

/**
 * Destructor. Used to delete a branch.
 */
DirWatcherLinux::Dir::~Dir()
{
   dwl->dirs.remove(wd);
   if (inotify_rm_watch(dwl->fileDescriptor, wd))
       L_ERRO(QString("DirWatcherLinux::~DirWatcherLinux : Unable to remove an inotify watcher."));

   for (QListIterator<Dir*> i(childs.values()); i.hasNext();)
      delete ((Dir*) i.next());
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

#endif
