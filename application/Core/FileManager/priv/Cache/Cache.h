#ifndef FILEMANAGER_CACHE_H
#define FILEMANAGER_CACHE_H

#include <QObject>
#include <QList>
#include <QStringList>
#include <QMutex>

#include <Protos/files_cache.pb.h>
#include <Protos/core_protocol.pb.h>

#include <priv/Cache/SharedDirectory.h>

namespace FM
{
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

      Protos::Core::GetEntriesResult getEntries(const Protos::Common::DirEntry& entry);
      Protos::Core::GetEntriesResult getEntries();

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

      quint64 getAmount() const;

      /**
        * Returns a directory wich correspond to the path, it will choose the shared directory which :
        *  - Has at least the needed space.
        *  - Has the most directories in common with 'path'.
        * The missing directories will be automatically created.
        * @param path Must be a cleaned path (QDir::cleanPath).
        * @return The directory, 0 if error.
        * @exception NoReadWriteSharedDirectoryException
        * @exception InsufficientStorageSpaceException
        */
      Directory* getDirectory(const QString& path, qint64 spaceNeeded);

      SharedDirectory* getSuperSharedDirectory(const QString& path);

      QList<SharedDirectory*> getSubSharedDirectories(const QString& path);

      /**
        * If path matches a shared directory or one of its sub directories then true is returned.
        */
      bool isShared(const QString& path) const;

      void onEntryAdded(Entry* entry);
      void onEntryRemoved(Entry* entry);
      void onChunkHashKnown(Chunk* chunk);
      void onChunkRemoved(Chunk* chunk);

      /**
        * Return the big cach lock.
        */
      QMutex& getMutex() { return this->lock; }

      /**
        * Will inform the fileUpdater and delete 'dir'.
        * If 'dir2' is given 'dir' content (sub dirs + files) will be give to 'dir2'.
        */
      void removeSharedDir(SharedDirectory* dir, Directory* dir2 = 0);

   signals:
      void entryAdded(Entry* entry);
      void entryRemoved(Entry* entry);
      void chunkHashKnown(Chunk* chunk);
      void chunkRemoved(Chunk* chunk);

   private:

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
