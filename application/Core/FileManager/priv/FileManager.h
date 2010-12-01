#ifndef FILEMANAGER_FILEMANAGER_H
#define FILEMANAGER_FILEMANAGER_H

#include <QObject>
#include <QSharedPointer>
#include <QList>
#include <QBitArray>
#include <QMutex>
#include <QElapsedTimer>

#include <Protos/common.pb.h>
#include <Protos/core_protocol.pb.h>

#include <IFileManager.h>
#include <priv/FileUpdater/FileUpdater.h>
#include <priv/Cache/Cache.h>
#include <priv/ChunkIndex/Chunks.h>
#include <priv/WordIndex/WordIndex.h>

namespace FM
{
   class Entry;
   class Chunk;
   class Directory;
   class IChunk;
   class IGetHashesResult;

   class FileManager : public IFileManager
   {
      Q_OBJECT
   public:
      FileManager();
      ~FileManager();

      void setSharedDirsReadOnly(const QStringList& dirs);
      void setSharedDirsReadWrite(const QStringList& dirs);
      QStringList getSharedDirsReadOnly();
      QStringList getSharedDirsReadWrite();

      QSharedPointer<IChunk> getChunk(const Common::Hash& hash) const;
      QList< QSharedPointer<IChunk> > getAllChunks(const Common::Hash& hash) const;
      QList< QSharedPointer<IChunk> > newFile(const Protos::Common::Entry& remoteEntry);
      QSharedPointer<IGetHashesResult> getHashes(const Protos::Common::Entry& file);

      Protos::Common::Entries getEntries(const Protos::Common::Entry& dir);
      Protos::Common::Entries getEntries();

      QList<Protos::Common::FindResult> find(const QString& words, int maxNbResult, int maxSize);
      QBitArray haveChunks(const QList<Common::Hash>& hashes);
      quint64 getAmount();

      Directory* getFittestDirectory(const QString& path);
      Entry* getEntry(const QString& path);

   private slots:
      void newSharedDirectory(SharedDirectory*);
      void sharedDirectoryRemoved(SharedDirectory*, Directory*);
      void entryAdded(Entry* entry);
      void entryRemoved(Entry* entry);
      void chunkHashKnown(QSharedPointer<Chunk> chunk);
      void chunkRemoved(QSharedPointer<Chunk> chunk);

   private:
      static QStringList splitInWords(const QString& words);

      void loadCacheFromFile();

   private slots:
      void persistCacheToFile();
      void tryToPersistCacheToFile();

   private:
      const quint32 CHUNK_SIZE;
      const quint32 SAVE_CACHE_PERIOD;

      FileUpdater fileUpdater;
      Cache cache; ///< The files and directories.
      Chunks chunks; ///< The indexed chunks. It contains only completed chunks.
      WordIndex<Entry*> wordIndex; ///< The word index.
      bool cacheLoading; ///< Set to 'true' during cache loading. It avoids to persist the cache during loading.

      QElapsedTimer timerPersistCache;
      QMutex mutexPersistCache;
   };
}
#endif
