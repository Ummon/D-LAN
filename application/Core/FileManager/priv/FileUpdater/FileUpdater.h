#ifndef FILEMANAGER_FILEUPDATER_H
#define FILEMANAGER_FILEUPDATER_H

#include <QThread>
#include <QWaitCondition>
#include <QMutex>
#include <QString>
#include <QList>

namespace FileManager
{
   class FileManager;
   class DirWatcher;
   class SharedDirectory;

   class FileUpdater : public QThread
   {
   public:
      FileUpdater(FileManager* fileManager);
      void addRoot(SharedDirectory* dir);
      void rmRoot(SharedDirectory* dir);

      void run();

   private:
      /**
        * Scan recursively all the directories and files contained
        * in dir. Create the associated cached tree structure under
        * given 'SharedDirectory'.
        */
      void scan(SharedDirectory* dir);

      FileManager* fileManager;
      DirWatcher* dirWatcher;

      QWaitCondition dirNotEmpty;
      QMutex mutex;

      QList<SharedDirectory*> dirsToScan;
      //QList<SharedDirectory*> dirsToDelete;
   };
}
#endif
