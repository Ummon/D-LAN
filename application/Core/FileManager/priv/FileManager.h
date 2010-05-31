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
      ~FileManager();

      void setSharedDirsReadOnly(const QStringList& dirs);
      void setSharedDirsReadWrite(const QStringList& dirs);
      QStringList getSharedDirsReadOnly();
      QStringList getSharedDirsReadWrite();

      IChunk* getChunk(const Common::Hash& hash);
      virtual QList< QSharedPointer<IChunk> > newFile(const Protos::Common::FileEntry& remoteEntry);
      // IGetHashesResult* getHashes(const  Protos::Common::FileEntry& entry);

      Protos::Core::GetEntriesResult getEntries(const Protos::Common::DirEntry& entry);
      Protos::Core::GetEntriesResult getEntries();

      Protos::Common::FindResult find(const QString& words);
      QBitArray haveChunks(const QList<Common::Hash>& hashes);
      quint64 getAmount();

      Directory* getFittestDirectory(const QString& path);
      Entry* getEntry(const QString& path);

   private slots:
      void entryAdded(Entry* entry);
      void entryRemoved(Entry* entry);
      void chunkHashKnown(Chunk* chunk);
      void chunkRemoved(Chunk* chunk);

   private:
      static QStringList splitInWords(const QString& words);

      void loadCacheFromFile();

   private slots:
      void persistCacheToFile();

   private:
      FileUpdater fileUpdater;
      Cache cache; ///< The files and directories.
      Chunks chunks; ///< The indexed chunks.
      WordIndex<Entry*> wordIndex; ///< The word index.
   };
}
#endif
