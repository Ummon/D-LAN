#ifndef FILEMANAGER_CACHE_H
#define FILEMANAGER_CACHE_H

#include <QObject>
#include <QList>
#include <QStringList>
#include <QMutex>

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

      quint64 getAmount();

      void onEntryAdded(Entry* entry);
      void onEntryRemoved(Entry* entry);
      void onChunkAdded(Chunk* chunk);

   signals:
      void entryAdded(Entry* entry);
      void entryRemoved(Entry* entry);
      void chunkAdded(Chunk* chunk);

   private:
      QList<SharedDirectory*> sharedDirs;

      FileManager* fileManager;
      FileUpdater* fileUpdater;

      QMutex lock; // To protect some data into the cache.
   };
}
#endif
