#include "DirWatcher.h"

#include <QtCore/QtDebug>

#if defined(Q_OS_WIN32)
   #include <DirWatcherWin.h>
#endif

/**
  * @class DirWatcher
  * An abstract directory watcher.
  * Can watch several directories recursively.
  *
  * There must be an implementation for the current platform,
  * if any exist, an error will occur during compilation.
  * See the factory 'getNewWatcher'.
  *
  * Event types :
  *  - Rename dir
  *  - Rename file
  *  - New file
  *  - Delete file
  *  - The content of a file changed
  */

DirWatcher::DirWatcher()
{

}

DirWatcher::~DirWatcher()
{
}

/**
  * Build a new watcher.
  * The implementation depends of the platform.
  */
DirWatcher* DirWatcher::getNewWatcher()
{
#if defined(Q_OS_WIN32)
   return new DirWatcherWin();
#else
   #error There is no implementation of 'DirWatcher' for the current platform
#endif
}


/**
  * @struct WatcherEvent
  * When a event occurs this struct is returned.
  */

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
