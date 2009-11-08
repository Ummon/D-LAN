#include <QtCore/QDebug>

#if defined(Q_OS_WIN32)
#include <priv/FileUpdater/DirWatcherWin.h>
using namespace FM;

#include <priv/Exceptions.h>
#include <priv/Log.h>
#include <priv/FileUpdater/WaitConditionWin.h>

DirWatcherWin::DirWatcherWin()
{
}

DirWatcherWin::~DirWatcherWin()
{
}

void DirWatcherWin::addDir(const QString& path)
{
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

   this->dirs.append(Dir(fileHandle, eventHandle, path));

   this->watch(this->dirs.count() - 1);
}

void DirWatcherWin::rmDir(const QString& path)
{
   for (QMutableListIterator<Dir> i(this->dirs); i.hasNext();)
   {
      Dir& dir = i.next();
      if (dir.fullPath == path)
      {
         // Should we wait with GetOverlappedResult or do a test with HasOverlappedIoCompleted ?
         CancelIo(dir.file);

         if (!CloseHandle(dir.file)) LOG_ERR(QString("CloseHandle(dir.file) return an error : %1").arg(GetLastError()));
         if (!CloseHandle(dir.overlapped.hEvent)) LOG_ERR(QString("CloseHandle(dir.overlapped.hEvent) return an error : %1").arg(GetLastError()));

         i.remove();
         break;
      }
   }
}

int DirWatcherWin::nbWatchedDir()
{
   return this->dirs.size();
}

const QList<WatcherEvent> DirWatcherWin::waitEvent(WaitCondition* w)
{
   return this->waitEvent(INFINITE, w);
}

const QList<WatcherEvent> DirWatcherWin::waitEvent(int timeout, WaitCondition* waitCondition)
{
   int n = this->dirs.size();
   int m = n + (waitCondition ? 1 : 0);

   HANDLE eventsArray[m];
   for(int i = 0; i < n; i++)
      eventsArray[i] = this->dirs[i].overlapped.hEvent;

   if (waitCondition)
   {
      HANDLE hdl = waitCondition->getHandle();
      eventsArray[n] = hdl;
   }

   DWORD waitStatus = WaitForMultipleObjects(m, eventsArray, FALSE, timeout);

   if (waitStatus >= WAIT_OBJECT_0 && waitStatus <= WAIT_OBJECT_0 + (DWORD)n - 1)
   {
      int n = waitStatus - WAIT_OBJECT_0;

      qDebug() << "File changed : " << n;

      FILE_NOTIFY_INFORMATION* notifyInformation = (FILE_NOTIFY_INFORMATION*)this->notifyBuffer;
      forever
      {
         qDebug() << "Action = " << notifyInformation->Action;

         // We need to add a null character termination because 'QString::fromStdWString' need one.
         int nbChar = notifyInformation->FileNameLength / sizeof(TCHAR);
         TCHAR filenameTCHAR[nbChar + 1];
         wcsncpy(filenameTCHAR, notifyInformation->FileName, nbChar);
         filenameTCHAR[nbChar] = 0;
         QString filename = QString::fromStdWString(filenameTCHAR);

         qDebug() << "filename = " << filename;
         qDebug() << "offset = " << notifyInformation->NextEntryOffset;

         if (!notifyInformation->NextEntryOffset)
            break;

         notifyInformation = (FILE_NOTIFY_INFORMATION*)((LPBYTE)notifyInformation + notifyInformation->NextEntryOffset);
      }
      this->watch(n);

      QList<WatcherEvent> events;
      events.append(WatcherEvent(WatcherEvent::NEW_FILE, "TAISTE"));
      return events;
   }
   else if (waitCondition && waitStatus == WAIT_OBJECT_0 + (DWORD)n)
   {
      return QList<WatcherEvent>();
   }
   else if (waitStatus == WAIT_TIMEOUT)
   {
      QList<WatcherEvent> events;
      events.append(WatcherEvent(WatcherEvent::TIMEOUT));
      return events;
   }
   else
   {
      throw DirWatcherException(QString("WaitForMultipleObjects(..), status : %1").arg(waitStatus));
   }
}

void DirWatcherWin::watch(int num)
{
   if (!ReadDirectoryChangesW(
      this->dirs[num].file, // The file handle;
      &this->notifyBuffer, // The buffer where the information is put when an event occur.
      NOTIFY_BUFFER_SIZE,
      TRUE, // Watch subtree.
      FILE_NOTIFY_CHANGE_FILE_NAME | FILE_NOTIFY_CHANGE_DIR_NAME | FILE_NOTIFY_CHANGE_SIZE | FILE_NOTIFY_CHANGE_LAST_WRITE | FILE_NOTIFY_CHANGE_CREATION,
      &this->nbBytesNotifyBuffer, // not used in asynchronous mode.
      &this->dirs[num].overlapped,
      NULL
   ))
      throw DirWatcherException(QString("ReadDirectoryChangesW(..), GetLastError : %1").arg(GetLastError()));
}

#endif
