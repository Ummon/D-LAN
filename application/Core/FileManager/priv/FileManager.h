#ifndef FILEMANAGER_FILEMANAGER_H
#define FILEMANAGER_FILEMANAGER_H

#include <QSharedPointer>

#include <Common/LogManager/ILogger.h>

#include <IFileManager.h>
#include <priv/FileUpdater.h>

namespace FileManager
{
   class Chunks;
   class WordIndex;
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

      static QSharedPointer<LogManager::ILogger> logger;

   private:
      FileUpdater fileUpdater;

      Chunks* chunks;
      WordIndex* wordIndex;

      QList<SharedDirectory*> sharedDirReadWrite;
      QList<SharedDirectory*> sharedDirReadOnly;
   };
}
#endif
