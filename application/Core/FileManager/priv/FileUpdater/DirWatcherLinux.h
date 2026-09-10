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
#include <QRecursiveMutex>
#include <QSet>

#include <priv/FileUpdater/DirWatcher.h>

#include <sys/inotify.h>
#include <sys/types.h>

class DirWatcherLinuxTests;

namespace FM
{
   class DirWatcherLinux : public DirWatcher
   {
   public:
      DirWatcherLinux();
      ~DirWatcherLinux();

      bool addPath(const QString& path, const QString& filename = QString()) override;
      void rmPath(const QString& path, const QString& filename = QString()) override;
      int nbWatchedPath();
      const QList<WatcherEvent> waitEvent(QList<WaitCondition*> ws = QList<WaitCondition*>());
      const QList<WatcherEvent> waitEvent(int timeout, QList<WaitCondition*> ws = QList<WaitCondition*>());

   private:
      friend class ::DirWatcherLinuxTests; // Inject unmount events without privileged mounts.

      static const int EVENT_SIZE; // Size of the event structure, not counting name.
      static const size_t BUF_LEN; // Reasonable guess as to size of 1024 events.
      static const uint32_t EVENTS_OBS; // Inotify events caught for subdirectories.
      static const uint32_t ROOT_EVENTS_OBS; // Inotify events caught for root directories.
      static const uint32_t EVENTS_FILE; // Inotify events caught for files.

      int addWatch(const QString& path, uint32_t mask);

      struct Dir
      {
         Dir(DirWatcherLinux* dwl, Dir* parent, const QString& name);
         ~Dir();
         QString getFullPath();
         Dir* getRoot();
         void move(Dir* to, const QString& newName);

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
         bool matchesPath() const;

         DirWatcherLinux* dwl;
         const QString path;
         int wd; // Watch descriptor.
         dev_t device;
         ino_t inode;
      };

      QList<Dir*> dirs; // The watched root dirs, indexed by full path.
      QHash<QString, File*> files; // Files indexed by their path.
      QHash<int, int> watchReferences; // Overlapping paths can share an inotify watch descriptor.

      File* getFile(int wd) const;
      Dir* getDir(int wd) const;
      QList<Dir*> getDirs(int wd) const;
      void addChildWatches(int parentWd, const QString& name, QSet<QString>& failedRoots);
      QList<WatcherEvent> removeWatchedPathsUnder(const QString& path);

      void rmWatcher(int watcher);
      void clearWatches();
      QList<WatcherEvent> recoverFromOverflow();
      QList<WatcherEvent> processInotifyEvents(const char* buf, int len);
      QString getEventPath(const inotify_event* event);

      QRecursiveMutex mutex;

      bool initialized;
      int fileDescriptor;
   };
}
