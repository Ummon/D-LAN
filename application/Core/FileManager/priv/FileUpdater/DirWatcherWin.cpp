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

#include <priv/FileUpdater/DirWatcherWin.h>
using namespace FM;

#include <QtCore/QDebug>

#include <QMutexLocker>
#include <cstddef>

#include <Common/StringUtils.h>

#include <priv/Exceptions.h>
#include <priv/FileUpdater/WaitConditionWin.h>

/**
  * @class FM::DirWatcherWin
  *
  * Implementation of 'DirWatcher' for the Windows platform.
  * Inspired by : http://stackoverflow.com/questions/863135/why-does-readdirectorychangesw-omit-events.
  */

DirWatcherWin::DirWatcherWin()
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
bool DirWatcherWin::addPath(const QString& path, const QString& filename)
{
   QMutexLocker locker(&this->mutex);

   if (this->dirs.size() > MAXIMUM_WAIT_OBJECTS - MAX_WAIT_CONDITION)
      return false;

   HANDLE fileHandle =
      CreateFile(
         Common::StringUtils::towcharList(path).constData(), // Pointer to the directory.
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
   {
      CloseHandle(fileHandle);
      throw DirNotFoundException(path);
   }

   Dir* dir = new Dir(fileHandle, eventHandle, path, filename);

   if (!this->watch(dir))
   {
      delete dir;
      return false;
   }

   this->dirs << dir;
   return true;
}

void DirWatcherWin::rmPath(const QString& path, const QString& filename)
{
   QMutexLocker locker(&this->mutex);

   for (QListIterator<Dir*> i(this->dirs); i.hasNext();)
   {
      Dir* dir = i.next();
      if (dir->fullPath == path && dir->filename == filename)
      {
         this->dirsToDelete << dir;
         break;
      }
   }
}

int DirWatcherWin::nbWatchedPath()
{
   QMutexLocker locker(&this->mutex);
   return this->dirs.size();
}

const QList<WatcherEvent> DirWatcherWin::waitEvent(QList<WaitCondition*> ws)
{
   return this->waitEvent(-1, ws);
}

/**
  * Warning: If there is too much time between two calls of 'waitEvent(..)' and
  * there is some disk activities (new files/folders) the buffer 'NOTIFY_BUFFER_SIZE'
  * may become full. A lost batch requests a rescan; a failed watch switches to polling.
  */
const QList<WatcherEvent> DirWatcherWin::waitEvent(int timeout, QList<WaitCondition*> ws)
{
   QMutexLocker locker(&this->mutex);

   if (ws.size() > MAX_WAIT_CONDITION)
   {
      L_ERRO(
         QString("DirWatcherWin::waitEvent: No more than %1 condition(s), some directory will not be watched any more.")
            .arg(MAX_WAIT_CONDITION)
      );
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

   // The total number of HANDLEs (watched directories + given wait conditions (ws)).
   int numberOfHandles = numberOfDirs + ws.size();

   QList<HANDLE> eventsArray(numberOfHandles);
   for(int i = 0; i < numberOfDirs; i++)
      eventsArray[i] = dirsCopy[i]->overlapped.hEvent;

   for (int i = 0; i < ws.size(); i++)
   {
      HANDLE hdl = static_cast<WaitConditionWin*>(ws[i])->getHandle();
      eventsArray[i + numberOfDirs] = hdl;
   }

   locker.unlock();

   DWORD waitStatus =
      WaitForMultipleObjects(
         numberOfHandles,
         eventsArray.data(),
         FALSE,
         timeout == -1 ? INFINITE : timeout
      );

   locker.relock();

   // The cause of the wake up comes from a watched directory.
   if (!dirsCopy.empty() && waitStatus >= WAIT_OBJECT_0 && waitStatus <= WAIT_OBJECT_0 + (DWORD)numberOfDirs - 1)
   {
      Dir* dir = dirsCopy[waitStatus - WAIT_OBJECT_0]; // The dir where a modification occurred.

      DWORD bytesTransferred = 0;
      const bool completed = GetOverlappedResult(dir->handle, &dir->overlapped, &bytesTransferred, FALSE) != 0;
      const DWORD error = completed ? ERROR_SUCCESS : GetLastError();
      return this->processCompletion(dir, bytesTransferred, error);
   }
   // The cause of the wake up comes from a given wait condition.
   else if (
      !ws.isEmpty() &&
      waitStatus >= WAIT_OBJECT_0 + (DWORD)numberOfDirs &&
      waitStatus <= WAIT_OBJECT_0 + (DWORD)numberOfHandles - 1
   )
   {
      return QList<WatcherEvent>();
   }
   else if (waitStatus == WAIT_TIMEOUT)
   {
      QList<WatcherEvent> events;
      events.append(WatcherEvent(WatcherEvent::TIMEOUT, false));
      return events;
   }
   else if (waitStatus == WAIT_FAILED)
   {
      L_ERRO(QString("WaitForMultipleObjects(..) failed, error code: %1").arg(GetLastError()));
   }
   else
   {
      L_ERRO(QString("WaitForMultipleObjects(..), status: %1").arg(waitStatus));
   }

   return QList<WatcherEvent> { WatcherEvent(WatcherEvent::UNKNOWN, false) };
}

QList<WatcherEvent> DirWatcherWin::processCompletion(Dir* dir, DWORD bytesTransferred, DWORD error)
{
   // Validate the entire batch before interpreting any rename pair or emitting events.
   // Zero bytes (and ERROR_NOTIFY_ENUM_DIR) mean changes were lost: enumerate again.
   bool valid = error == ERROR_SUCCESS && bytesTransferred > 0 && bytesTransferred <= NOTIFY_BUFFER_SIZE;
   DWORD offset = 0;
   constexpr DWORD headerSize = offsetof(FILE_NOTIFY_INFORMATION, FileName);
   while (valid)
   {
      const DWORD remaining = bytesTransferred - offset;
      if (remaining < headerSize)
      {
         valid = false;
         break;
      }
      const auto record = reinterpret_cast<const FILE_NOTIFY_INFORMATION*>(dir->buffer + offset);
      if (record->FileNameLength == 0 || record->FileNameLength % sizeof(wchar_t) != 0 ||
          record->FileNameLength > remaining - headerSize)
      {
         valid = false;
         break;
      }
      if (record->NextEntryOffset == 0)
         break;
      if (record->NextEntryOffset % alignof(DWORD) != 0 ||
          record->NextEntryOffset < headerSize + record->FileNameLength ||
          record->NextEntryOffset > remaining - headerSize)
      {
         valid = false;
         break;
      }
      offset += record->NextEntryOffset;
   }

   QList<WatcherEvent> events;
   if (valid)
   {
      const BYTE* pToBuffer = dir->buffer;

      forever
      {
         auto notifyInformation = reinterpret_cast<const FILE_NOTIFY_INFORMATION*>(pToBuffer);

         const QString filename = QString::fromWCharArray(notifyInformation->FileName,
            notifyInformation->FileNameLength / sizeof(wchar_t));

         const bool isWatchedFile = !dir->filename.isEmpty();
         const QString path = dir->fullPath + filename;

         // Pair before filtering: the new name no longer matches a watched file's
         // old name. Also retain the new name for later events in this batch.
         if (notifyInformation->Action == FILE_ACTION_RENAMED_OLD_NAME)
            dir->pendingRenameName = filename;
         else if (notifyInformation->Action == FILE_ACTION_RENAMED_NEW_NAME)
         {
            if (!dir->pendingRenameName.isEmpty() &&
                (!isWatchedFile || dir->pendingRenameName == dir->currentFilename))
            {
               events << WatcherEvent(WatcherEvent::MOVE, dir->fullPath + dir->pendingRenameName, path, isWatchedFile);
               if (isWatchedFile)
                  dir->currentFilename = filename;
            }
            else if (!isWatchedFile || filename == dir->currentFilename)
               // An unmatched new name (or a replacement arriving at the watched path).
               events << WatcherEvent(WatcherEvent::NEW, path, isWatchedFile);
            dir->pendingRenameName.clear();
         }
         else if (!isWatchedFile || filename == dir->currentFilename)
         {

            L_DEBU("---------");
            L_DEBU(QString("action = %1").arg(notifyActionToString(notifyInformation->Action)));
            L_DEBU(QString("path = %1").arg(path));
            L_DEBU(QString("offset = %1").arg(notifyInformation->NextEntryOffset));
            L_DEBU("---------");

            switch (notifyInformation->Action)
            {
            case FILE_ACTION_ADDED: // 1.
               events << WatcherEvent(WatcherEvent::NEW, path, isWatchedFile);
               break;
            case FILE_ACTION_REMOVED: // 2.
               events << WatcherEvent(WatcherEvent::DELETED, path, isWatchedFile);
               break;
            case FILE_ACTION_MODIFIED: // 3.
               events << WatcherEvent(WatcherEvent::CONTENT_CHANGED, path, isWatchedFile);
               break;
            default:
               L_WARN(QString("File event action unknown: %1").arg(notifyInformation->Action));
            }
         }

         if (!notifyInformation->NextEntryOffset)
            break;

         // The next notify information data is given in the current notify information . . .
         pToBuffer += notifyInformation->NextEntryOffset;
      }
   }
   else
   {
      dir->pendingRenameName.clear();
      L_WARN(QString("Directory notifications lost for %1 (error %2, bytes %3); rescanning")
         .arg(dir->fullPath).arg(error).arg(bytesTransferred));
      events << WatcherEvent(WatcherEvent::RESCAN, dir->fullPath + dir->currentFilename, !dir->filename.isEmpty());
   }

   // A failed completion other than overflow is not a functioning watch. A failed
   // rearm likewise needs polling, even when the preceding batch was valid.
   if ((error != ERROR_SUCCESS && error != ERROR_NOTIFY_ENUM_DIR) || !this->watch(dir))
   {
      L_WARN(QString("Directory watch stopped for %1; falling back to polling").arg(dir->fullPath + dir->currentFilename));
      events << WatcherEvent(WatcherEvent::WATCH_LOST, dir->fullPath + dir->currentFilename, !dir->filename.isEmpty());
      this->dirs.removeOne(dir);
      this->dirsToDelete.removeAll(dir);
      delete dir;
   }
   return events;
}

DirWatcherWin::Dir::Dir(const HANDLE handle, const HANDLE event, const QString& fullPath, const QString& filename)
   : handle(handle), fullPath(fullPath), filename(filename), currentFilename(filename)
{
   memset(&this->overlapped, 0, sizeof(OVERLAPPED));
   overlapped.hEvent = event;
}

DirWatcherWin::Dir::~Dir()
{
   // The pending 'ReadDirectoryChangesW' writes asynchronously into 'overlapped' and 'buffer' which are part of this
   // object: the cancellation must be complete before the memory is freed, otherwise the heap gets corrupted.
   // 'CancelIoEx' is used because 'CancelIo' only cancels the requests issued by the calling thread.
   if (CancelIoEx(this->handle, &this->overlapped) || GetLastError() != ERROR_NOT_FOUND)
   {
      DWORD bytesTransferred;
      GetOverlappedResult(this->handle, &this->overlapped, &bytesTransferred, TRUE);
   }

   if (!CloseHandle(this->handle))
      L_ERRO(QString("CloseHandle(dir.file) return an error: %1").arg(GetLastError()));

   if (!CloseHandle(this->overlapped.hEvent))
      L_ERRO(QString("CloseHandle(dir.overlapped.hEvent) return an error: %1").arg(GetLastError()));
}

bool DirWatcherWin::watch(Dir* dir)
{
   // A directory: watch recursively.
   if (dir->filename.isEmpty())
   {
      return ReadDirectoryChangesW(
         dir->handle, // The file handle;
         dir->buffer, // The buffer where the information is put when an event occur.
         NOTIFY_BUFFER_SIZE, // Size of the previous buffer.
         TRUE, // Watch subtree.
         FILE_NOTIFY_CHANGE_FILE_NAME |
            FILE_NOTIFY_CHANGE_DIR_NAME |
            FILE_NOTIFY_CHANGE_SIZE |
            FILE_NOTIFY_CHANGE_LAST_WRITE |
            FILE_NOTIFY_CHANGE_CREATION,
         // &this->nbBytesNotifyBuffer,
         nullptr,
         &dir->overlapped,
         nullptr
      ) != 0;
   }
   else // A File: watch non-recursively.
   {
      return ReadDirectoryChangesW(
         dir->handle, // The file handle;
         dir->buffer, // The buffer where the information is put when an event occur.
         NOTIFY_BUFFER_SIZE, // Size of the previous buffer.
         FALSE, // Watch subtree.
         FILE_NOTIFY_CHANGE_FILE_NAME |
            FILE_NOTIFY_CHANGE_SIZE |
            FILE_NOTIFY_CHANGE_LAST_WRITE,
         // &this->nbBytesNotifyBuffer,
         nullptr,
         &dir->overlapped,
         nullptr
      ) != 0;
   }
}

QString DirWatcherWin::notifyActionToString(DWORD action)
{
   switch (action)
   {
   case FILE_ACTION_ADDED: return QString("ADDED");
   case FILE_ACTION_REMOVED: return QString("REMOVED");
   case FILE_ACTION_MODIFIED: return QString("MODIFIED");
   case FILE_ACTION_RENAMED_OLD_NAME: return QString("RENAMED_OLD_NAME");
   case FILE_ACTION_RENAMED_NEW_NAME: return QString("RENAMED_NEW_NAME");
   default: return QString("<unknown action: %1>").arg(action);
   }
}
