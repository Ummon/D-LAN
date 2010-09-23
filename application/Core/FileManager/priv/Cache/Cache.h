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
#include <priv/Cache/Chunk.h>

namespace FM
{
   class Entry;
   class FileUpdater;
   class FileManager;

   class Cache : public QObject, Common::Uncopyable
   {
      Q_OBJECT
   public:
      Cache(FileManager* fileManager);

   public:
      Protos::Core::GetEntriesResult getEntries(const Protos::Common::Entry& dir) const;
      Protos::Core::GetEntriesResult getEntries() const;
      Entry* getEntry(const QString& path) const;
      File* getFile(const Protos::Common::Entry&) const;
      QList< QSharedPointer<IChunk> > newFile(const Protos::Common::Entry& remoteEntry);

      QStringList getSharedDirs(SharedDirectory::Rights rights) const;
      void setSharedDirs(const QStringList& dirs, SharedDirectory::Rights rights);
      void removeSharedDir(SharedDirectory* dir, Directory* dir2 = 0);

      SharedDirectory* getSuperSharedDirectory(const QString& path) const;
      QList<SharedDirectory*> getSubSharedDirectories(const QString& path) const;
      bool isShared(const QString& path) const;

      Directory* getFittestDirectory(const QString& path) const;

      void retrieveFromFile(const Protos::FileCache::Hashes& hashes);
      void saveInFile(Protos::FileCache::Hashes& hashes) const;

      quint64 getAmount() const;

      void onEntryAdded(Entry* entry);
      void onEntryRemoved(Entry* entry);
      void onChunkHashKnown(QSharedPointer<Chunk> chunk);
      void onChunkRemoved(QSharedPointer<Chunk> chunk);

      /**
        * Return the big cache lock.
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

      Directory* getWriteableDirectory(const QString& path, qint64 spaceNeeded) const;

      QList<SharedDirectory*> sharedDirs;

      FileManager* fileManager;

      mutable QMutex lock; ///< To protect all the data into the cache, files and directories.
   };
}
#endif
