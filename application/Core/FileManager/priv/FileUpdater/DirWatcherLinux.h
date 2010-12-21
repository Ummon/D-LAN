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

namespace FM
{
   /**
     * Implementation of 'DirWatcher' for the linux platform.
     */
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
          Dir(QString fullPath, int watcher) : fullPath(fullPath), watcher(watcher)
          {
          }
          ~Dir()
          {
          }

          QString fullPath;
          int watcher;
       };

       bool initialized;
       int fileDescriptor;

       QList<Dir*> dirs; // The watched dirs.

       void rmWatcher(int watcher);
       QString getWatcherPath(int watcher);

       QMutex mutex;
   };
}

#endif // FILEMANAGER_DIRWATCHERLINUX_H
