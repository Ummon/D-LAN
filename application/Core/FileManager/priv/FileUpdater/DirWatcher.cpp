#include <priv/FileUpdater/DirWatcher.h>
using namespace FM;

#include <QtCore/QtDebug>

#include <priv/FileManager.h>

#if defined(Q_OS_WIN32)
   #include <priv/FileUpdater/DirWatcherWin.h>
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
   LOG_WARN("Cannot create a watcher for the current platform, no implementation.");
   return 0;
#endif
}

WatcherEvent::WatcherEvent()
      : type(WatcherEvent::UNKNOWN)
{}

WatcherEvent::WatcherEvent(Type type)
      : type(type)
{}

WatcherEvent::WatcherEvent(WatcherEvent::Type type, const QString& path1)
      : type(type), path1(path1)
{}

WatcherEvent::WatcherEvent(WatcherEvent::Type type, const QString& path1, const QString& path2)
      : type(type), path1(path1), path2(path2)
{}
