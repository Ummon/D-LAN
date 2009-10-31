#ifndef FILEMANAGER_FILEUPDATER_H
#define FILEMANAGER_FILEUPDATER_H

#include <QThread>
#include <QWaitCondition>
#include <QMutex>
#include <QString>
#include <QList>

#include <Protos/files_cache.pb.h>
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

      /**
        * Retrieve the file cache frome the given data.
        * Muste be called before starting the fileUpdater.
        * This object must unallocated the hashes;
        */
      void retrieveFromFile(const Protos::FileCache::Hashes* fileCache, const QList<SharedDirectory*>& sharedDirectories);

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

      const Protos::FileCache::Hashes* fileCache; ///< The hashes from the saved file cache. Used only at the begining of 'run()'.

      WaitCondition* dirEvent; ///< Using to wait when a sharing directory is added or deleted.
      QMutex mutex;

      QList<SharedDirectory*> dirsToScan; ///< When a new shared directory is added, it is put in this list until it is scanned.
      /*SharedDirectory* currentScanningDir;
      QWaitCondition scanningStopped;
      QMutex scanningMutex;*/

      QList<SharedDirectory*> dirsToRemove;

      QList<File*> fileWithoutHashes;
   };
}
#endif
