#ifndef FILEMANAGER_FILEMANAGER_H
#define FILEMANAGER_FILEMANAGER_H

#include <QSharedPointer>

#include <Common/LogManager/ILogger.h>

#include <IFileManager.h>
#include <priv/FileUpdater/FileUpdater.h>
#include <priv/ChunkIndex/Chunks.h>
#include <priv/WordIndex/WordIndex.h>

namespace FileManager
{
   class Entry;
   class SharedDirectory;
   class File;
   class Directory;

   class FileManager : public IFileManager
   {
   public :
      FileManager();

      IChunk* getChunk(const Common::Hash& hash);
      /*IGetHashesResult* getHashes(const  Protos::Common::FileEntry& entry);
      Protos::Core::GetEntriesResult* getEntries(const Protos::Common::DirEntry& entry);
      Protos::Common::FindResult find(const QString& words);
      QList<bool> haveChunks(const QList<Common::Hash>& hashes);
      quint64 getAmount();
      QList<Protos::Common::DirEntry> getSharedDirs();
      QList<Protos::Common::DirEntry> getDestinationDirs();
      IFile newFile(const Protos::Common::FileEntry& remotEntry);*/

      File* getFile(const QString& path, const QString& name);
      Directory* getDir(const QString& path, const QString& name);
      // QList<SharedDirectory*> getRoots(); // Useless for the moment.

      Chunks& getChunks();

      //WordIndex<Entry*>& getWordIndex();
      void addToWordIndex(Entry* entry);

      /**
        * Called by a newly created file;
        * It willadd
        */
      //void newFileAdded(); // not necessary for the moment

      /**
        * Called by a newly created directory;
        */
      //void newDirAdded(); // not necessary for the moment

      static QSharedPointer<LogManager::ILogger> logger;

      static const int  MAX_WORD_LENGTH = 3;

   private:
      FileUpdater fileUpdater;

      Chunks chunks;
      WordIndex<Entry*> wordIndex;

      QList<SharedDirectory*> sharedDirReadWrite;
      QList<SharedDirectory*> sharedDirReadOnly;
   };
}
#endif
