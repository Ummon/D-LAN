#include <QtCore/QtCore> // Only for the Q_OS_* defines.

#if !defined(FILEMANAGER_DIRWATCHERWIN_H) and defined(Q_OS_WIN32)
#define FILEMANAGER_DIRWATCHERWIN_H

#include <priv/FileUpdater/DirWatcher.h>

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
      void addDir(const QString& path);

      void rmDir(const QString& path);
      int nbWatchedDir();
      const QList<WatcherEvent> waitEvent(WaitCondition* w = 0);
      const QList<WatcherEvent> waitEvent(int timeout, WaitCondition* w = 0);

   private:
      void watch(int num);

      struct Dir
      {
         Dir(HANDLE file, HANDLE event, QString fullPath) : file(file), event(event), fullPath(fullPath) {}
         HANDLE file;
         HANDLE event;
         QString fullPath;
      };
      QList<Dir> dirs; // The watched dirs.

      // Is this data can be shares among some 'ReadDirectoryChangesW'?
      char notifyBuffer[NOTIFY_BUFFER_SIZE];
      DWORD nbBytesNotifyBuffer;
   };
}

#endif
