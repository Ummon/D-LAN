#include <QtCore/QtCore> // Only for the Q_OS_* defines.

#if !defined(FILEMANAGER_DIRWATCHERWIN_H) and defined(Q_OS_WIN32)
#define FILEMANAGER_DIRWATCHERWIN_H

#include <priv/FileUpdater/DirWatcher.h>
#include <priv/Log.h>

#include <windows.h>

namespace FM
{
   static const int NOTIFY_BUFFER_SIZE = 2048;

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
      bool watch(int num);

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
      QList<Dir*> dirs; // The watched dirs.

      // Is this data can be shares among some 'ReadDirectoryChangesW'?
      char notifyBuffer[NOTIFY_BUFFER_SIZE];
      DWORD nbBytesNotifyBuffer;
   };
}

#endif
