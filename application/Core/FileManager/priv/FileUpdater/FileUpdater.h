#ifndef FILEMANAGER_FILEUPDATER_H
#define FILEMANAGER_FILEUPDATER_H

#include <QThread>
#include <QWaitCondition>
#include <QMutex>
#include <QString>
#include <QLinkedList>

#include <priv/FileUpdater/DirWatcher.h>

namespace FM
{
   class FileManager;
   class DirWatcher;
   class SharedDirectory;
   class Directory;
   class File;
   class WaitCondition;

   class FileUpdater : public QThread
   {
      static const int MINIMUM_DURATION_WHEN_HASHING = 30; ///< In seconds.

   public:
      FileUpdater(FileManager* fileManager);
      ~FileUpdater();

      /**
        * @exception DirNotFoundException
        */
      void addRoot(SharedDirectory* dir);

      void rmRoot(SharedDirectory* dir);

   protected:
      void run();

   private:
      void createNewFile(Directory* dir, const QString& filename, qint64 size);

      /**
        * It will take some file from 'fileWithoutHashes' and compute theirs hashes.
        * The duration of the compuation is minimum 'minimumDurationWhenHashing'.
        */
      void computeSomeHashes();

      /**
        * Scan recursively all the directories and files contained
        * in dir. Create the associated cached tree structure under
        * given 'SharedDirectory'.
        */
      void scan(SharedDirectory* dir);

      void treatEvents(const QList<WatcherEvent>& events);

      FileManager* fileManager;
      DirWatcher* dirWatcher;

      WaitCondition* dirEvent; ///< Using to wait when a sharing directory is added or deleted.
      QMutex mutex;

      QLinkedList<SharedDirectory*> dirsToScan; ///< When a new shared directory is added, it is put in this list until it is scanned.
      /*SharedDirectory* currentScanningDir;
      QWaitCondition scanningStopped;
      QMutex scanningMutex;*/

      QLinkedList<SharedDirectory*> dirsToRemove;

      QLinkedList<File*> fileWithoutHashes;
   };
}
#endif
