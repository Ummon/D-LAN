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
  
#ifndef FILEMANAGER_DIRWATCHERWIN_H
#define FILEMANAGER_DIRWATCHERWIN_H

#include <QtCore/QtCore> // Only for the Q_OS_* defines.

#include <QMutex>

#include <priv/FileUpdater/DirWatcher.h>
#include <priv/Log.h>

#include <windows.h>

namespace FM
{
   static const int NOTIFY_BUFFER_SIZE = 2048;
   static const int MAX_WAIT_CONDITION = 4;

   /**
     * Implementation of 'DirWatcher' for the windows platform.
     * Inspired by : http://stackoverflow.com/questions/863135/why-does-readdirectorychangesw-omit-events.
     */
   class DirWatcherWin : public DirWatcher
   {
   public:
      DirWatcherWin();
      ~DirWatcherWin();

      /**
        * @exception DirNotFoundException
        */
      bool addDir(const QString& path);
      void rmDir(const QString& path);
      int nbWatchedDir();
      const QList<WatcherEvent> waitEvent(QList<WaitCondition*> ws = QList<WaitCondition*>());
      const QList<WatcherEvent> waitEvent(int timeout, QList<WaitCondition*> ws = QList<WaitCondition*>());

   private:
      struct Dir
      {
         Dir(HANDLE file, HANDLE event, QString fullPath) : file(file), fullPath(fullPath)
         {
            memset(&this->overlapped, 0, sizeof(OVERLAPPED));
            overlapped.hEvent = event;
         }
         ~Dir()
         {
            // Should we wait with GetOverlappedResult or do a test with HasOverlappedIoCompleted ?
            CancelIo(this->file);

            if (!CloseHandle(this->file)) L_ERRO(QString("CloseHandle(dir.file) return an error : %1").arg(GetLastError()));
            if (!CloseHandle(this->overlapped.hEvent)) L_ERRO(QString("CloseHandle(dir.overlapped.hEvent) return an error : %1").arg(GetLastError()));
         }

         HANDLE file;
         OVERLAPPED overlapped;
         QString fullPath;
      };

      bool watch(Dir* dir);

      QList<Dir*> dirs; // The watched dirs.
      QList<Dir*> dirsToDelete; // Dirs to delete.

      // Is this data can be shares among some 'ReadDirectoryChangesW'?
      char notifyBuffer[NOTIFY_BUFFER_SIZE];
      DWORD nbBytesNotifyBuffer;

      QMutex mutex;
   };
}

#endif
