/**
  * Aybabtu - A decentralized LAN file sharing software.
  * Copyright (C) 2010-2011 Greg Burri <greg.burri@gmail.com>
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

#if !defined(FILEMANAGER_DIRWATCHERLINUX_H) and defined(Q_OS_LINUX)
#define FILEMANAGER_DIRWATCHERLINUX_H

#include <priv/FileUpdater/DirWatcher.h>
#include <priv/Log.h>

#include <sys/inotify.h>

namespace FM
{
   class DirWatcherLinux : public DirWatcher
   {
   public:
       DirWatcherLinux();
       ~DirWatcherLinux();

       bool addDir(const QString& path);
       void rmDir(const QString& path);
       int nbWatchedDir();
       const QList<WatcherEvent> waitEvent(QList<WaitCondition*> ws = QList<WaitCondition*>());
       const QList<WatcherEvent> waitEvent(int timeout, QList<WaitCondition*> ws = QList<WaitCondition*>());

   private:
       struct Dir
       {
          DirWatcherLinux* dwl;
          Dir* parent;
          QMap<QString, Dir*> childs;
          QString name;
          int wd; // inotify watch descriptor

          Dir(DirWatcherLinux* dwl, Dir* parent, const QString& name);
          ~Dir();
          QString getFullPath();
          void rename(const QString& newName);
          void move(Dir* to);
       };

       bool initialized;
       int fileDescriptor;

       QMap<int, Dir*> dirs; // The watched dirs, indexed by watch descriptor.
       QMap<QString, Dir*> rootDirs; // The watched root dirs, indexed by full path.

       void rmWatcher(int watcher);
       QString getEventPath(inotify_event *event);

       QMutex mutex;
   };
}

#endif // FILEMANAGER_DIRWATCHERLINUX_H
