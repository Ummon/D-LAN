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
   class DirWatcher;
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
        * @exception DirNotFoundException
        */
      void addRoot(SharedDirectory* dir);

      void rmRoot(SharedDirectory* dir);

      /**
        * Set the file cache to retrieve the hashes frome it.
        * Muste be called before starting the fileUpdater.
        * The shared dirs in fileCache must be previously added by 'addRoot(..)'.
        * This object must unallocated the hashes.
        */
      void setFileCache(const Protos::FileCache::Hashes* fileCache);

   signals:
      void persistCache();

   protected:
      void run();

   private:
      void createNewFile(Directory* dir, const QString& filename, qint64 size);

      /**
        * It will take some file from 'fileWithoutHashes' and compute theirs hashes.
        * The duration of the compuation is minimum 'minimumDurationWhenHashing'.
        * @return true if some file has been hashed
        */
      bool computeSomeHashes();

      /**
        * Scan recursively all the directories and files contained
        * in dir. Create the associated cached tree structure under
        * given 'SharedDirectory'.
        */
      void scan(SharedDirectory* dir);

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
      WaitCondition* stopEvent; ///< Using to stop the thread.
      QMutex mutex;

      QList<SharedDirectory*> dirsToScan; ///< When a new shared directory is added, it is put in this list until it is scanned.
      SharedDirectory* currentScanningDir;
      QWaitCondition scanningStopped;
      QMutex scanningMutex;

      QList<QString> dirsToRemove;

      QList<File*> fileWithoutHashes;
   };
}
#endif
