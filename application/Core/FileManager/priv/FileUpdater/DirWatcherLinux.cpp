#include <QtCore/QtCore> // For the Q_OS_* defines.

#ifdef Q_OS_LINUX

#include "DirWatcherLinux.h"
using namespace FM;

#include <QMutexLocker>

#include <sys/select.h>
#include <sys/inotify.h>
#include <errno.h>

/* Size of the event structure, not counting name. */
#define EVENT_SIZE (sizeof (struct inotify_event))

/* Reasonable guess as to size of 1024 events. */
#define BUF_LEN (1024 * (EVENT_SIZE + 16))

/* Inotify events catched for subdirectories. */
#define EVENTS_OBS IN_MOVE|IN_DELETE|IN_CREATE|IN_DELETE

/* Inotify events catched for root directories. */
#define ROOT_EVENTS_OBS EVENTS_OBS|IN_MOVE_SELF|IN_DELETE_SELF

DirWatcherLinux::DirWatcherLinux()
   : mutex(QMutex::Recursive)
{
   /* Initialize inotify */
   initialized = true;
   fileDescriptor = inotify_init();
   if (fileDescriptor < 0) {
      L_WARN(QString("Unable to initialize inotify, DirWatcher not use."));
      initialized = false;
   }
}

DirWatcherLinux::~DirWatcherLinux()
{
   QMutexLocker locker(&this->mutex);

   /* Close file descriptor */
   if (close(fileDescriptor) < 0) {
       L_ERRO(QString("DirWatcherLinux::~DirWatcherLinux : Unable to close file descriptor (inotify)."));
   }
}

bool DirWatcherLinux::addDir(const QString& path)
{
   QMutexLocker locker(&this->mutex);

   if (!initialized) return false;

   Dir* dir = new Dir(this, NULL, path);
   if (dir->wd < 0) return false;
   rootDirs.insert(path, dir);

   return true;
}

void DirWatcherLinux::rmDir(const QString& path)
{
   QMutexLocker locker(&this->mutex);

   Dir* dir = rootDirs.take(path);
   delete dir;
}

QString DirWatcherLinux::getEventPath(inotify_event *event) {
   QMutexLocker locker(&this->mutex);

   QString p = dirs.value(event->wd)->getFullPath();
   if (event->len)
      p.append('/').append(event->name);

   return p;
}

int DirWatcherLinux::nbWatchedDir()
{
   QMutexLocker locker(&this->mutex);
   return this->rootDirs.size();
}

const QList<WatcherEvent> DirWatcherLinux::waitEvent(QList<WaitCondition*> ws)
{
   return this->waitEvent(-1, ws);
}

const QList<WatcherEvent> DirWatcherLinux::waitEvent(int timeout, QList<WaitCondition*> ws)
{
   this->mutex.lock();

   fd_set fds;
   char buf[BUF_LEN];
   struct timeval time;

   /* Convert timeout in timeval */
   time.tv_sec = timeout / 1000;
   time.tv_usec = (timeout % 1000) * 1000;


   while (1) {
      /* Zero-out the fd_set. */
      FD_ZERO(&fds);

      /* Add the inotify fd to the fd_set. */
      FD_SET(fileDescriptor, &fds);

      this->mutex.unlock();
      int sel = select(fileDescriptor + 1, &fds, NULL, NULL, (timeout==-1 ? 0 : &time));
      this->mutex.lock();
      if (sel < 0)
      {
         L_ERRO(QString("DirWatcherLinux::waitEvent : select error."));
         continue;
      }
      else if (!sel)
      {
         QList<WatcherEvent> events;
         events.append(WatcherEvent(WatcherEvent::TIMEOUT));
         return events;
      }

      int len = read(fileDescriptor, buf, BUF_LEN);
      if (len < 0)
      {
         if (errno == EINTR)
            /* Need to reissue system call */
            continue;
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
            /* Add the event to movedToEvents. */
            movedFromEvents << event;
         }
         if (event->mask & IN_MOVED_TO)
         {
            /* Check list of IN_MOVED_FROM events. */
            for (QMutableListIterator<inotify_event*> i(movedFromEvents); i.hasNext();)
            {
               struct inotify_event *fromEvent = i.next();
               if (fromEvent->cookie == event->cookie)
               {
                  /* If an IN_MOVES_FROM event is linked, create a MOVE WatcherEvent. */
                  events << WatcherEvent(WatcherEvent::MOVE, getEventPath(fromEvent), getEventPath(event));

                  /* If moved object is a directory, apply change to the local directory index */
                  if (event->mask & IN_ISDIR)
                  {
                     /* Retrieve from and to directory by watch descriptor. */
                     Dir* fromDir = dirs.value(fromEvent->wd);
                     Dir* toDir = dirs.value(event->wd);

                     /* Retrieve moved directory by child map of from directory,
                        because actually the name isn't changed. */
                     Dir* movedDir = fromDir->childs.value(fromEvent->name);

                     /* If the name of moved directory has changed, rename it. */
                     if (fromEvent->name != event->name)
                        movedDir->rename(event->name);

                     /* If the path of moved directosy has changed, move it. */
                     if (fromDir->getFullPath() != toDir->getFullPath())
                        movedDir->move(fromDir, toDir);
                  }
                  i.remove();
                  /* exit the IN_MOVED_TO process */
                  goto end_moved_to;
               }
            }
            /* if no IN_MOVED_FROM event is linked, create a NEW WatcherEvent.
               IN_MOVED_FROM event without IN_MOVE_TO event have to be processed at
               the end of the loop, when every IN_MOVED_TO event is processed.  */
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

      /* Cause every IN_MOVED_FROM event with a linked IN_MOVED_TO event was removed of
         the list, it contains only alone IN_MOVED_FROM event. */
      for (QMutableListIterator<struct inotify_event*> i(movedFromEvents); i.hasNext();)
      {
         struct inotify_event *e = i.next();
         events << WatcherEvent(WatcherEvent::DELETED, getEventPath(e));
      }

      return events;
   }

   return QList<WatcherEvent>();
}

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

DirWatcherLinux::Dir::~Dir()
{
   dwl->dirs.remove(wd);
   if (inotify_rm_watch(dwl->fileDescriptor, wd))
       L_ERRO(QString("DirWatcherLinux::~DirWatcherLinux : Unable to remove an inotify watcher."));

   for (QListIterator<Dir*> i(childs.values()); i.hasNext();)
      delete ((Dir*) i.next());
}

QString DirWatcherLinux::Dir::getFullPath()
{
   QString fullPath = this->name;

   if (this->parent) {
      fullPath.prepend(this->parent->getFullPath().append("/"));
   }
   return fullPath;
}

void DirWatcherLinux::Dir::rename(const QString& newName)
{
   this->parent->childs.remove(this->name);
   this->name = newName;
   this->parent->childs.insert(this->name, this);
}

void DirWatcherLinux::Dir::move(Dir* from, Dir* to)
{
   from->childs.remove(this->name);
   this->parent = to;
   to->childs.insert(this->name, this);
}

#endif
