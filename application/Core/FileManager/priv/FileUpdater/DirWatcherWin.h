/**
  * D-LAN - A decentralized LAN file sharing software.
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
  
#include <QtCore/QtCore> // Only for the Q_OS_* defines.

#if !defined(FILEMANAGER_DIRWATCHERWIN_H) and defined(Q_OS_WIN32)
#define FILEMANAGER_DIRWATCHERWIN_H

#include <QMutex>

#include <priv/FileUpdater/DirWatcher.h>
#include <priv/Log.h>

#include <windows.h>

namespace FM
{
   static const int NOTIFY_BUFFER_SIZE = 2048;
   static const int MAX_WAIT_CONDITION = 4;

   class DirWatcherWin : public DirWatcher
   {
   public:
      DirWatcherWin();
      ~DirWatcherWin();

      bool addDir(const QString& path);
      void rmDir(const QString& path);
      int nbWatchedDir();
      const QList<WatcherEvent> waitEvent(QList<WaitCondition*> ws = QList<WaitCondition*>());
      const QList<WatcherEvent> waitEvent(int timeout, QList<WaitCondition*> ws = QList<WaitCondition*>());

   private:
      struct Dir
      {
         Dir(const HANDLE file, const HANDLE event, const QString& fullPath);
         ~Dir();

         const HANDLE file;
         OVERLAPPED overlapped;
         const QString fullPath;
      };

      bool watch(Dir* dir);

      QList<Dir*> dirs; ///< The watched dirs.
      QList<Dir*> dirsToDelete; ///< Dirs to delete.

      char notifyBuffer[NOTIFY_BUFFER_SIZE]; ///< Is this data can be shares among some 'ReadDirectoryChangesW'?
      DWORD nbBytesNotifyBuffer;

      QMutex mutex;
   };
}

#endif
