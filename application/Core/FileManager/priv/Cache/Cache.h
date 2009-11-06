#ifndef FILEMANAGER_CACHE_H
#define FILEMANAGER_CACHE_H

#include <QObject>
#include <QList>
#include <QStringList>
#include <QMutex>

#include <Protos/files_cache.pb.h>

#include <priv/Cache/SharedDirectory.h>

namespace FM
{
   class SharedDirectory;
   class Entry;
   class Chunk;
   class FileUpdater;
   class FileManager;

   class Cache : public QObject
   {
      Q_OBJECT
   public:
      Cache(FileManager* fileManager, FileUpdater* fileUpdater);

      QStringList getSharedDirs(SharedDirectory::Rights rights);

      /**
        * @exception DirsNotFoundException
        */
      void setSharedDirs(const QStringList& dirs, SharedDirectory::Rights rights);

      /**
        * Define the shared directories from the persisted given data.
        * The directories and files are not created here but later by the fileUpdater, see the FileManager ctor.
        */
      void retrieveFromFile(const Protos::FileCache::Hashes& hashes);

      /**
        * Populate the given structure to be persisted later.
        */
      void saveInFile(Protos::FileCache::Hashes& hashes);

      quint64 getAmount();

      SharedDirectory* getSuperSharedDirectory(const QString& path);
      QList<SharedDirectory*> getSubSharedDirectories(const QString& path);

      void onEntryAdded(Entry* entry);
      void onEntryRemoved(Entry* entry);
      void onChunkAdded(Chunk* chunk);

      /**
        * Return the big cach lock.
        */
      QMutex& getMutex() { return this->lock; }

   signals:
      void entryAdded(Entry* entry);
      void entryRemoved(Entry* entry);
      void chunkAdded(Chunk* chunk);

   private:
      void removeSharedDir(SharedDirectory* dir);

      /**
        * Create new shared directories and inform the fileUpdater.
        * @exception DirsNotFoundException
        */
      void createSharedDirs(const QStringList& dirs, const QList<SharedDirectory::Rights>& rights, const QList<Common::Hash>& ids = QList<Common::Hash>());
      void createSharedDirs(const QStringList& dirs, SharedDirectory::Rights rights);
      void createSharedDirs(const Protos::FileCache::Hashes& hashes);

      QList<SharedDirectory*> sharedDirs;

      FileManager* fileManager;
      FileUpdater* fileUpdater;

      QMutex lock; ///< To protect all the data into the cache, files and directories.
   };
}
#endif
