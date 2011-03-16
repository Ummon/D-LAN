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
  
#include <QtCore/QDebug>

#include <QMutexLocker>

#if defined(Q_OS_WIN32)
   #include <priv/FileUpdater/DirWatcherWin.h>
using namespace FM;

#include <priv/Exceptions.h>
#include <priv/FileUpdater/WaitConditionWin.h>

/**
  * @class DirWatcherWin
  * Implementation of 'DirWatcher' for the windows platform.
  * Inspired by : http://stackoverflow.com/questions/863135/why-does-readdirectorychangesw-omit-events.
  */

DirWatcherWin::DirWatcherWin() :
   mutex(QMutex::Recursive)
{
}

DirWatcherWin::~DirWatcherWin()
{
   QMutexLocker locker(&this->mutex);

   foreach (Dir* d, this->dirs)
      delete d;
}

/**
  * @exception DirNotFoundException
  */
bool DirWatcherWin::addDir(const QString& path)
{
   QMutexLocker locker(&this->mutex);

   if (this->dirs.size() > MAXIMUM_WAIT_OBJECTS - MAX_WAIT_CONDITION)
      return false;

   TCHAR pathTCHAR[path.size() + 1];
   path.toWCharArray(pathTCHAR);
   pathTCHAR[path.size()] = 0;

   HANDLE fileHandle = CreateFile(pathTCHAR, // Pointer to the file name.
      FILE_LIST_DIRECTORY, // Access (read/write) mode.
      FILE_SHARE_READ | FILE_SHARE_WRITE, // Share mode.
      NULL, // security descriptor
      OPEN_EXISTING, // how to create
      FILE_FLAG_BACKUP_SEMANTICS | FILE_FLAG_OVERLAPPED, // file attributes
      NULL // file with attributes to copy
   );

   if (fileHandle == INVALID_HANDLE_VALUE)
      throw DirNotFoundException(path);

   HANDLE eventHandle = CreateEvent(NULL, FALSE, FALSE, NULL);
   if (eventHandle == NULL)
      throw DirNotFoundException(path);

   Dir* dir = new Dir(fileHandle, eventHandle, path);

   if (!this->watch(dir))
   {
      delete dir;
      return false;
   }

   this->dirs << dir;
   return true;
}

void DirWatcherWin::rmDir(const QString& path)
{
   QMutexLocker locker(&this->mutex);

   for (QListIterator<Dir*> i(this->dirs); i.hasNext();)
   {
      Dir* dir = i.next();
      if (dir->fullPath == path)
      {
         this->dirsToDelete << dir;
         break;
      }
   }
}

int DirWatcherWin::nbWatchedDir()
{
   QMutexLocker locker(&this->mutex);
   return this->dirs.size();
}

const QList<WatcherEvent> DirWatcherWin::waitEvent(QList<WaitCondition*> ws)
{
   return this->waitEvent(-1, ws);
}

const QList<WatcherEvent> DirWatcherWin::waitEvent(int timeout, QList<WaitCondition*> ws)
{
   QMutexLocker locker(&this->mutex);

   if (ws.size() > MAX_WAIT_CONDITION)
   {
      L_ERRO(QString("DirWatcherWin::waitEvent : No more than %1 condition(s), some directory will not be watched any more.").arg(MAX_WAIT_CONDITION));
      int n = this->dirs.size() + ws.size() - MAXIMUM_WAIT_OBJECTS;
      while (n --> 0) // The best C++ operator!
         this->dirsToDelete << this->dirs.takeLast();
   }

   if (!this->dirsToDelete.isEmpty())
   {
      for (QMutableListIterator<Dir*> i(this->dirs); i.hasNext();)
      {
         Dir* dir = i.next();
         if (this->dirsToDelete.contains(dir))
         {
            i.remove();
            delete dir;
         }
      }
      this->dirsToDelete.clear();
   }

   QList<Dir*> dirsCopy(this->dirs);

   // Builds an array of HANDLEs which will be given to the 'WaitForMultipleObjects' function.

   int numberOfDirs = dirsCopy.size();
   int numberOfHandles = numberOfDirs + ws.size(); // The total number of HANDLEs (watched directories + given wait conditions (ws)).

   HANDLE eventsArray[numberOfHandles];
   for(int i = 0; i < numberOfDirs; i++)
      eventsArray[i] = dirsCopy[i]->overlapped.hEvent;

   for (int i = 0; i < ws.size(); i++)
   {
      HANDLE hdl = ws[i]->getHandle();
      eventsArray[i + numberOfDirs] = hdl;
   }

   locker.unlock();

   DWORD waitStatus = WaitForMultipleObjects(numberOfHandles, eventsArray, FALSE, timeout == -1 ? INFINITE : timeout);

   locker.relock();

   // The cause of the wake up comes from a watched directory.
   if (!dirsCopy.empty() && waitStatus >= WAIT_OBJECT_0 && waitStatus <= WAIT_OBJECT_0 + (DWORD)numberOfDirs - 1)
   {
      Dir* dir = dirsCopy[waitStatus - WAIT_OBJECT_0]; // The dir where a modification occurred.

      QList<WatcherEvent> events;

      FILE_NOTIFY_INFORMATION* notifyInformation = (FILE_NOTIFY_INFORMATION*)this->notifyBuffer;

      QString previousPath; // Used for FILE_ACTION_RENAMED_OLD_NAME

      forever
      {
         // We need to add a null character termination because 'QString::fromStdWString' need one.
         int nbChar = notifyInformation->FileNameLength / sizeof(TCHAR);
         TCHAR filenameTCHAR[nbChar + 1];
         wcsncpy(filenameTCHAR, notifyInformation->FileName, nbChar);
         filenameTCHAR[nbChar] = 0;
         QString filename = QString::fromStdWString(filenameTCHAR);

//         L_WARN("---------");
//         L_WARN(QString("Action = %1").arg(notifyInformation->Action));
//         L_WARN(QString("filename = %1").arg(filename));
//         L_WARN(QString("offset = %1").arg(notifyInformation->NextEntryOffset));
//         L_WARN("---------");

         QString path = dir->fullPath;
         path.append('/').append(filename);

         switch (notifyInformation->Action)
         {
         case FILE_ACTION_ADDED:
            events << WatcherEvent(WatcherEvent::NEW, path);
            break;
         case FILE_ACTION_REMOVED:
            events << WatcherEvent(WatcherEvent::DELETED, path);
            break;
         case FILE_ACTION_MODIFIED:
            events << WatcherEvent(WatcherEvent::CONTENT_CHANGED, path);
            break;
         case FILE_ACTION_RENAMED_OLD_NAME:
            previousPath = path;
            break;
         case FILE_ACTION_RENAMED_NEW_NAME:
            events << WatcherEvent(WatcherEvent::MOVE, previousPath, path);
            break;
         default:
            L_WARN(QString("File event action unkown : %1").arg(notifyInformation->Action));
         }

         if (!notifyInformation->NextEntryOffset)
            break;

         // The next notify information data is given in the current notify information..
         notifyInformation = (FILE_NOTIFY_INFORMATION*)((LPBYTE)notifyInformation + notifyInformation->NextEntryOffset);
      }

      this->watch(dir);

      return events;
   }
   // The cause of the wake up comes from a given wait condition.
   else if (!ws.isEmpty() && waitStatus >= WAIT_OBJECT_0 + (DWORD)numberOfDirs && waitStatus <= WAIT_OBJECT_0 + (DWORD)numberOfHandles - 1)
   {
      return QList<WatcherEvent>();
   }
   else if (waitStatus == WAIT_TIMEOUT)
   {
      QList<WatcherEvent> events;
      events.append(WatcherEvent(WatcherEvent::TIMEOUT));
      return events;
   }
   else if (waitStatus == WAIT_FAILED)
   {
      L_ERRO(QString("WaitForMultipleObjects(..) failed, error code : %1").arg(GetLastError()));
   }
   else
   {
      L_ERRO(QString("WaitForMultipleObjects(..), status : %1").arg(waitStatus));
   }

   QList<WatcherEvent> events;
   events << WatcherEvent(WatcherEvent::UNKNOWN);
   return QList<WatcherEvent>();
}

DirWatcherWin::Dir::Dir(const HANDLE file, const HANDLE event, const QString& fullPath) :
   file(file), fullPath(fullPath)
{
   memset(&this->overlapped, 0, sizeof(OVERLAPPED));
   overlapped.hEvent = event;
}

DirWatcherWin::Dir::~Dir()
{
   // Should we wait with GetOverlappedResult or do a test with HasOverlappedIoCompleted ?
   CancelIo(this->file);

   if (!CloseHandle(this->file)) L_ERRO(QString("CloseHandle(dir.file) return an error : %1").arg(GetLastError()));
   if (!CloseHandle(this->overlapped.hEvent)) L_ERRO(QString("CloseHandle(dir.overlapped.hEvent) return an error : %1").arg(GetLastError()));
}

bool DirWatcherWin::watch(Dir* dir)
{
   return ReadDirectoryChangesW(
      dir->file, // The file handle;
      &this->notifyBuffer, // The buffer where the information is put when an event occur.
      NOTIFY_BUFFER_SIZE, // Size of the previous buffer.
      TRUE, // Watch subtree.
      FILE_NOTIFY_CHANGE_FILE_NAME | FILE_NOTIFY_CHANGE_DIR_NAME | FILE_NOTIFY_CHANGE_LAST_WRITE /* | FILE_NOTIFY_CHANGE_CREATION | FILE_NOTIFY_CHANGE_SIZE*/,
      &this->nbBytesNotifyBuffer, // Not used in asynchronous mode.
      &dir->overlapped,
      NULL
   );
}

#endif
