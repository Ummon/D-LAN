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

#ifndef FILEMANAGER_DIRWATCHERLINUX_H
#define FILEMANAGER_DIRWATCHERLINUX_H

#include <priv/FileUpdater/DirWatcher.h>

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
       static const int EVENT_SIZE; // Size of the event structure, not counting name.
       static const size_t BUF_LEN; // Reasonable guess as to size of 1024 events.
       static const uint32_t EVENTS_OBS; // Inotify events catched for subdirectories.
       static const uint32_t ROOT_EVENTS_OBS; // Inotify events catched for root directories.

       struct Dir
       {
          Dir(DirWatcherLinux* dwl, Dir* parent, const QString& name);
          ~Dir();
          QString getFullPath();
          void rename(const QString& newName);
          void move(Dir* to);

          DirWatcherLinux* dwl;
          Dir* parent;
          QMap<QString, Dir*> childs;
          QString name;
          int wd;
       };

       QMap<int, Dir*> dirs; // The watched dirs, indexed by watch descriptor.
       QList<Dir*> rootDirs; // The watched root dirs, indexed by full path.

       void rmWatcher(int watcher);
       QString getEventPath(inotify_event *event);

       QMutex mutex;

       bool initialized;
       int fileDescriptor;
   };
}

#endif
