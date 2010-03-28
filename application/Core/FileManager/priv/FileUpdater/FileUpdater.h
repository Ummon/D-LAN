#ifndef FILEMANAGER_FILEUPDATER_H
#define FILEMANAGER_FILEUPDATER_H

#include <QThread>
#include <QWaitCondition>
#include <QMutex>
#include <QString>
#include <QList>
#include <QTime>

#include <Protos/files_cache.pb.h>
#include <priv/FileUpdater/DirWatcher.h>

namespace FM
{
   class FileManager;
   class SharedDirectory;
   class Directory;
   class File;
   class WaitCondition;

   class FileUpdater : public QThread
   {
      Q_OBJECT

   public:
      FileUpdater(FileManager* fileManager);
      ~FileUpdater();

      void stop();

      /**
        * Set the file cache to retrieve the hashes frome it.
        * Muste be called before starting the fileUpdater.
        * The shared dirs in fileCache must be previously added by 'addRoot(..)'.
        * This object must unallocated the hashes.
        */
      void setFileCache(const Protos::FileCache::Hashes* fileCache);

   public slots:
      /**
        * @exception DirNotFoundException
        */
      void addRoot(SharedDirectory* dir);

      void rmRoot(SharedDirectory* dir, Directory* dir2 = 0);

   signals:
      void persistCache();

   protected:
      void run();

   private:

      /**
        * It will take some file from 'fileWithoutHashes' and compute theirs hashes.
        * The duration of the compuation is minimum 'minimumDurationWhenHashing'.
        * @return true if some file has been hashed
        */
      bool computeSomeHashes();

      /**
        * Stop the current hashing process or the next hashing process.
        */
      void stopHashing();
      /*void suspendHashing();
      void resumeHashing();*/

      /**
        * Synchronize the cache with the file system.
        * Scan recursively all the directories and files contained
        * in dir. Create the associated cached tree structure under
        * given 'SharedDirectory'.
        * Ths directories of files may already exist in the cache.
        */
      void scan(Directory* dir);

      /**
        * If you omit 'dir' then all scanning will be removed
        * from the queue.
        */
      void stopScanning(Directory* dir = 0);

      /**
        * Try to restore the chunk hashes from 'fileCache'.
        * 'fileCache' is set from 'retrieveFromFile(..)'.
        */
      void restoreFromFileCache(SharedDirectory* dir);

      void treatEvents(const QList<WatcherEvent>& events);

      FileManager* fileManager;
      DirWatcher* dirWatcher;

      const Protos::FileCache::Hashes* fileCache; ///< The hashes from the saved file cache. Used only at the begining of 'run()'.

      bool toStop; ///< Set to true when the service must be stopped.

      WaitCondition* dirEvent; ///< Using to wait when a sharing directory is added or deleted.
      QMutex mutex;

      QList<Directory*> dirsToScan; ///< When a new shared directory is added, it is put in this list until it is scanned.
      Directory* currentScanningDir;
      QWaitCondition scanningStopped;
      QMutex scanningMutex;

      QMutex hashingMutex;
      File* currentHashingFile;
      bool toStopHashing;

      QList<SharedDirectory*> dirsToRemove;

      QList<File*> fileWithoutHashes;
   };
}
#endif
