#ifndef FILEMANAGER_FILEMANAGER_H
#define FILEMANAGER_FILEMANAGER_H

#include <QObject>
#include <QSharedPointer>
#include <QList>
#include <QBitArray>

#include <Protos/common.pb.h>
#include <Protos/core_protocol.pb.h>

#include <IFileManager.h>
#include <priv/Cache/SharedDirectory.h>
#include <priv/FileUpdater/FileUpdater.h>
#include <priv/Cache/Cache.h>
#include <priv/ChunkIndex/Chunks.h>
#include <priv/WordIndex/WordIndex.h>

namespace FM
{
   class Entry;
   class Chunk;
   class File;
   class Directory;
   class IChunk;

   class FileManager : public IFileManager
   {
      Q_OBJECT
   public :

      FileManager();

      QStringList getSharedDirsReadOnly();
      QStringList getSharedDirsReadWrite();
      void setSharedDirsReadOnly(const QStringList& dirs);
      void setSharedDirsReadWrite(const QStringList& dirs);
      IChunk* getChunk(const Common::Hash& hash);
      // IGetHashesResult* getHashes(const  Protos::Common::FileEntry& entry);
      // Protos::Core::GetEntriesResult* getEntries(const Protos::Common::DirEntry& entry);
      Protos::Common::FindResult find(const QString& words);
      QBitArray haveChunks(const QList<Common::Hash>& hashes);
      quint64 getAmount();
      virtual QList< QSharedPointer<IChunk> > newFile(const Protos::Common::FileEntry& remoteEntry);

      /**
        * Used to retrieve a file by the fileUpdater when a filesystem event occurs.
        */
      File* getFile(const QString& path, const QString& name);

      /**
        * Used to retrieve a directory by the fileUpdater when a filesystem event occurs.
        */
      Directory* getDir(const QString& path, const QString& name);

   private slots:
      void entryAdded(Entry* entry);
      void entryRemoved(Entry* entry);
      void chunkAdded(Chunk* chunk);

   private:
      /**
        * Take raw terms in a string and split, trim and filter to
        * return a list of keyword.
        * Some character or word can be removed.
        * @example " The little  DUCK " => ["little", "duck"].
        */
      static QStringList splitInWords(const QString& words);

      /**
        * Load the cache from a file.
        * It will give the file cache to the fileUpdater and ask it
        * to load the cache.
        */
      void loadCacheFromFile();

   private slots:
      /**
        * Save the cache from a file.
        * Called by the fileUpdater when it needs to persist the cache.
        * /!\ Called in the fileUpdater thread.
        */
      void persistCacheToFile();

   private:

      FileUpdater fileUpdater;
      Cache cache; ///< The files and directories.
      Chunks chunks; ///< The indexed chunks.
      WordIndex<Entry*> wordIndex; ///< The word index.
   };
}
#endif
