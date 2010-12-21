#include <QtCore/QtCore> // For the Q_OS_* defines.

#ifdef Q_OS_LINUX

#include "DirWatcherLinux.h"
using namespace FM;

#include <QMutexLocker>

#include <sys/select.h>
#include <sys/inotify.h>
#include <errno.h>

/* size of the event structure, not counting name */
#define EVENT_SIZE (sizeof (struct inotify_event))

/* reasonable guess as to size of 1024 events */
#define BUF_LEN (1024 * (EVENT_SIZE + 16))


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

   int watcher = inotify_add_watch(fileDescriptor, path.toStdString().c_str(), IN_ALL_EVENTS);
   if (watcher < 0) {
       return false;
   }
   Dir* dir = new Dir(path, watcher);
   dirs << dir;
   return true;
}

void DirWatcherLinux::rmWatcher(int watcher)
{
   if (inotify_rm_watch(fileDescriptor, watcher)) {
       L_ERRO(QString("DirWatcherLinux::~DirWatcherLinux : Unable to remove an inotify watcher."));
   }
}

void DirWatcherLinux::rmDir(const QString& path)
{
   QMutexLocker locker(&this->mutex);

   for (QMutableListIterator<Dir*> i(this->dirs); i.hasNext();)
   {
      Dir* dir = i.next();
      if (dir->fullPath == path)
      {
         rmWatcher(dir->watcher);
         i.remove();
         delete dir;
         break;
      }
   }
}

QString DirWatcherLinux::getWatcherPath(int watcher) {
   for (QListIterator<Dir*> i(this->dirs); i.hasNext();)
   {
      Dir* dir = i.next();
      if (dir->watcher == watcher)
      {
         return dir->fullPath;
      }
   }
   return QString();
}

int DirWatcherLinux::nbWatchedDir()
{
   QMutexLocker locker(&this->mutex);
   return this->dirs.size();
}

const QList<WatcherEvent> DirWatcherLinux::waitEvent(QList<WaitCondition*> ws)
{
   return this->waitEvent(-1, ws);
}

const QList<WatcherEvent> DirWatcherLinux::waitEvent(int timeout, QList<WaitCondition*> ws)
{
   QMutexLocker locker(&this->mutex);

   fd_set fds;
   char buf[BUF_LEN];
   struct timeval time;

   // Convert timeout in timeval
   time.tv_sec = timeout / 1000;
   time.tv_usec = (timeout % 1000) * 1000;


   while (1) {
      /* zero-out the fd_set */
      FD_ZERO(&fds);

      /* add the inotify fd to the fd_set */
      FD_SET(fileDescriptor, &fds);

      int sel = select(fileDescriptor + 1, &fds, NULL, NULL, (timeout==-1 ? 0 : &time));
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
            /* need to reissue system call */
            continue;
         else
            L_ERRO(QString("DirWatcherLinux::waitEvent : read inotify event failed."));
      }
      else if (!len)
            L_ERRO(QString("DirWatcherLinux::waitEvent : BUF_LEN to small ?"));

      int i = 0;
      while (i < len)
      {
         struct inotify_event *event;

         event = (struct inotify_event *) &buf[i];

         QString path = getWatcherPath(event->wd);
         if (event->len)
            path.append('/').append(event->name);

         if (event->mask & IN_CLOSE_WRITE)
            events << WatcherEvent(WatcherEvent::CONTENT_CHANGED, path);
         if (event->mask & IN_MOVED_FROM)
         L_DEBU("inotify event : " + path + "(IN_MOVED_FROM)" + event->cookie);
         if (event->mask & IN_MOVED_TO)
         L_DEBU("inotify event : " + path + "(IN_MOVED_TO)" + event->cookie);
         if (event->mask & IN_MOVE_SELF)
         L_DEBU("inotify event : " + path + "(IN_MOVE_SELF)" + event->cookie);
         if (event->mask & IN_DELETE)
            events << WatcherEvent(WatcherEvent::DELETED, path);
         if (event->mask & IN_CREATE)
            events << WatcherEvent(WatcherEvent::NEW, path);
         if (event->mask & IN_DELETE_SELF)
         L_DEBU("inotify event : " + path + "(IN_DELETE_SELF)" + event->cookie);
//         if (event->mask & IN_UNMOUNT)
//         L_DEBU("inotify event : " + path + "(IN_UNMOUNT)" + event->cookie);
//         if (event->mask & IN_Q_OVERFLOW)
//         L_DEBU("inotify event : " + path + "(IN_Q_OVERFLOW)" + event->cookie);
//         if (event->mask & IN_IGNORED)
//         L_DEBU("inotify event : " + path + "(IN_IGNORED)" + event->cookie);
//         if (event->mask & IN_ISDIR)
//         L_DEBU("inotify event : " + path + "(IN_ISDIR)" + event->cookie);
//         else
//         L_DEBU("inotify event : " + path + "(IN_ISFILE)" + event->cookie);

         i += EVENT_SIZE + event->len;
      }
   }

   return QList<WatcherEvent>();
}

#endif
