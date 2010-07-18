#ifndef FILEMANAGER_CACHE_H
#define FILEMANAGER_CACHE_H

#include <QObject>
#include <QList>
#include <QStringList>
#include <QMutex>
#include <QSharedPointer>

#include <Protos/files_cache.pb.h>
#include <Protos/core_protocol.pb.h>

#include <Common/Uncopyable.h>
#include <priv/FileUpdater/DirWatcher.h>
#include <priv/Cache/SharedDirectory.h>

namespace FM
{
   class Entry;
   class Chunk;
   class FileUpdater;
   class FileManager;

   class Cache : public QObject, Common::Uncopyable
   {
      Q_OBJECT
   public:
      Cache(FileManager* fileManager);

   public:
      Protos::Core::GetEntriesResult getEntries(const Protos::Common::Entry& dir);
      Protos::Core::GetEntriesResult getEntries();
      Entry* getEntry(const QString& path);

      QStringList getSharedDirs(SharedDirectory::Rights rights);
      void setSharedDirs(const QStringList& dirs, SharedDirectory::Rights rights);
      void removeSharedDir(SharedDirectory* dir, Directory* dir2 = 0);

      SharedDirectory* getSuperSharedDirectory(const QString& path);
      QList<SharedDirectory*> getSubSharedDirectories(const QString& path);
      bool isShared(const QString& path) const;

      Directory* getDirectory(const QString& path, qint64 spaceNeeded);
      Directory* getFittestDirectory(const QString& path);

      void retrieveFromFile(const Protos::FileCache::Hashes& hashes);
      void saveInFile(Protos::FileCache::Hashes& hashes);

      quint64 getAmount() const;      

      void onEntryAdded(Entry* entry);
      void onEntryRemoved(Entry* entry);
      void onChunkHashKnown(QSharedPointer<Chunk> chunk);
      void onChunkRemoved(QSharedPointer<Chunk> chunk);

      /**
        * Return the big cach lock.
        */
      QMutex& getMutex() { return this->lock; }

   signals:
      void entryAdded(Entry* entry);
      void entryRemoved(Entry* entry);
      void chunkHashKnown(QSharedPointer<Chunk> chunk);
      void chunkRemoved(QSharedPointer<Chunk> chunk);

      void newSharedDirectory(SharedDirectory* dir);
      void sharedDirectoryRemoved(SharedDirectory* dir, Directory* dir2);

   private:
      void createSharedDirs(const QStringList& dirs, const QList<SharedDirectory::Rights>& rights, const QList<Common::Hash>& ids = QList<Common::Hash>());
      void createSharedDirs(const QStringList& dirs, SharedDirectory::Rights rights);
      void createSharedDirs(const Protos::FileCache::Hashes& hashes);

      QList<SharedDirectory*> sharedDirs;

      FileManager* fileManager;

      QMutex lock; ///< To protect all the data into the cache, files and directories.
   };
}
#endif
