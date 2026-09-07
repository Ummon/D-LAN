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

#include <QRecursiveMutex>

#include <priv/FileUpdater/DirWatcher.h>
#include <priv/Log.h>

#include <windows.h>

class CacheTest;

namespace FM
{
   static const int NOTIFY_BUFFER_SIZE = 32 * 1024; // 32 kB.
   static const int MAX_WAIT_CONDITION = 4;

   class DirWatcherWin : public DirWatcher
   {
   public:
      DirWatcherWin();
      ~DirWatcherWin();

      bool isReliable() const;
      bool addPath(const QString& path, const QString& filename = QString(""));
      void rmPath(const QString& path, const QString& filename = QString(""));
      int nbWatchedPath();
      const QList<WatcherEvent> waitEvent(QList<WaitCondition*> ws = QList<WaitCondition*>());
      const QList<WatcherEvent> waitEvent(int timeout, QList<WaitCondition*> ws = QList<WaitCondition*>());

   private:
      friend class ::CacheTest;
      struct Dir
      {
         Dir(const HANDLE handle, const HANDLE event, const QString& fullPath, const QString& filename);
         ~Dir();

         const HANDLE handle;
         OVERLAPPED overlapped;
         const QString fullPath;
         const QString filename;
         QString currentFilename; // Follows renames; filename remains the registration key for rmPath().
         QString pendingRenameName; // A rename pair can span notification buffers.
         alignas(sizeof(DWORD)) BYTE buffer[NOTIFY_BUFFER_SIZE];
      };

      bool watch(Dir* dir);
      QList<WatcherEvent> processCompletion(Dir* dir, DWORD bytesTransferred, DWORD error);

      static QString notifyActionToString(DWORD action);

      QList<Dir*> dirs; ///< The watched dirs.
      QList<Dir*> dirsToDelete; ///< Dirs to delete.

      // BYTE notifyBuffer[NOTIFY_BUFFER_SIZE]; ///< Is this data can be shares among some 'ReadDirectoryChangesW'?
      // DWORD nbBytesNotifyBuffer;

      QRecursiveMutex mutex;
   };
}
