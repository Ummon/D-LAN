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
   class Entry;
   class WaitCondition;

   class FileUpdater : public QThread
   {
      Q_OBJECT

   public:
      FileUpdater(FileManager* fileManager);
      ~FileUpdater();

      void stop();
      void setFileCache(const Protos::FileCache::Hashes* fileCache);
      void prioritizeAFileToHash(File* file);

   public slots:
      void addRoot(SharedDirectory* dir);
      void rmRoot(SharedDirectory* dir, Directory* dir2 = 0);

   signals:
      void persistCache();

   protected:
      void run();

   private:
      bool computeSomeHashes();

      void stopHashing();
      /*void suspendHashing();
      void resumeHashing();*/

      void scan(Directory* dir);

      void stopScanning(Directory* dir = 0);

      void deleteEntry(Entry* entry);
      void removeFromDirsToScan(Directory* dir);
      void removeFromFilesWithoutHashes(Directory* dir);

      void restoreFromFileCache(SharedDirectory* dir);

      void treatEvents(const QList<WatcherEvent>& events);

      FileManager* fileManager;
      DirWatcher* dirWatcher;

      const Protos::FileCache::Hashes* fileCache; ///< The hashes from the saved file cache. Used only temporally at the begining of 'run()'.

      bool toStop; ///< Set to true when the service must be stopped.

      WaitCondition* dirEvent; ///< Using to wait when a sharing directory is added or deleted.
      QMutex mutex; ///< Prevent the access from many thread to the internal data like 'filesWithoutHashes' for example.

      QList<Directory*> dirsToScan; ///< When a new shared directory is added, it is put in this list until it is scanned.
      Directory* currentScanningDir;
      QWaitCondition scanningStopped;
      QMutex scanningMutex;

      QMutex hashingMutex;
      File* currentHashingFile;
      bool toStopHashing;

      QList<SharedDirectory*> dirsToRemove;

      QList<File*> filesWithoutHashes;
   };
}
#endif
