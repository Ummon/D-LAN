#include <QtCore/QDebug>

#if defined(Q_OS_WIN32)

#include <priv/Exceptions.h>
#include <priv/FileUpdater/DirWatcherWin.h>
#include <priv/FileUpdater/WaitConditionWin.h>
using namespace FM;

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

   this->dirs.append(Dir(fileHandle, eventHandle));

   this->watch(this->dirs.count() - 1);
}

void DirWatcherWin::rmDir(const QString& path)
{
   // TODO
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
      eventsArray[i] = this->dirs[i].event;

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
   OVERLAPPED overlapped;
   memset(&overlapped, 0, sizeof(OVERLAPPED));
   overlapped.hEvent = this->dirs[num].event;

   if (!ReadDirectoryChangesW(
      this->dirs[num].file,
      &this->notifyBuffer,
      2048,
      TRUE,
      FILE_NOTIFY_CHANGE_FILE_NAME | FILE_NOTIFY_CHANGE_DIR_NAME |
      FILE_NOTIFY_CHANGE_SIZE | FILE_NOTIFY_CHANGE_LAST_WRITE | FILE_NOTIFY_CHANGE_CREATION,
      &this->nbBytesNotifyBuffer,
      &overlapped,
      NULL
   ))
      throw DirWatcherException(QString("ReadDirectoryChangesW(..), GetLastError : %1").arg(GetLastError()));
}

#endif
