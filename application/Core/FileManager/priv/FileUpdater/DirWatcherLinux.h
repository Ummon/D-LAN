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

#pragma once

#include <QHash>
#include <QList>
#include <QMutex>

#include <priv/FileUpdater/DirWatcher.h>

#include <sys/inotify.h>

namespace FM
{
   class DirWatcherLinux : public DirWatcher
   {
   public:
      DirWatcherLinux();
      ~DirWatcherLinux();

      bool addPath(const QString& path);
      void rmPath(const QString& path);
      int nbWatchedPath();
      const QList<WatcherEvent> waitEvent(QList<WaitCondition*> ws = QList<WaitCondition*>());
      const QList<WatcherEvent> waitEvent(int timeout, QList<WaitCondition*> ws = QList<WaitCondition*>());

   private:
      static const int EVENT_SIZE; // Size of the event structure, not counting name.
      static const size_t BUF_LEN; // Reasonable guess as to size of 1024 events.
      static const uint32_t EVENTS_OBS; // Inotify events caught for subdirectories.
      static const uint32_t ROOT_EVENTS_OBS; // Inotify events caught for root directories.
      static const uint32_t EVENTS_FILE; // Inotify events caught for files.

      static int addWatch(int fileDescriptor, const QString& path, uint32_t mask);

      struct Dir
      {
         Dir(DirWatcherLinux* dwl, Dir* parent, const QString& name);
         ~Dir();
         QString getFullPath();
         void rename(const QString& newName);
         void move(Dir* to);

         DirWatcherLinux* dwl;
         Dir* parent;
         QHash<QString, Dir*> children;
         QString name;
         int wd; // Watch descriptor.
      };

      struct File
      {
         File(DirWatcherLinux* dwl, const QString& path);
         ~File();

         DirWatcherLinux* dwl;
         const QString path;
         int wd; // Watch descriptor.
      };

      QList<Dir*> dirs; // The watched root dirs, indexed by full path.
      QHash<QString, File*> files; // Files indexed by their path.

      File* getFile(int wd) const;
      Dir* getDir(int wd) const;

      void rmWatcher(int watcher);
      QString getEventPath(inotify_event *event);

      QMutex mutex;

      bool initialized;
      int fileDescriptor;
   };
}
