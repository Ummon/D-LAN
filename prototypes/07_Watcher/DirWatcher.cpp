#include "DirWatcher.h"

#include <QtCore/QtDebug>

#if defined(Q_OS_WIN32)
   #include <DirWatcherWin.h>
#endif

DirWatcher::DirWatcher()
{

}

DirWatcher::~DirWatcher()
{
}

DirWatcher* DirWatcher::getNewWatcher()
{
#if defined(Q_OS_WIN32)
   return new DirWatcherWin();
#else
#error There is no implementation of 'DirWatcher'
#endif
}

WatcherEvent::WatcherEvent(WatcherEvent::Type type, const QString& path1)
      : path1(path1)
{
}

WatcherEvent::WatcherEvent(WatcherEvent::Type type, const QString& path1, const QString& path2)
      : path1(path1), path2(path2)
{
}
