#ifndef FILEMANAGER_FILEMANAGER_H
#define FILEMANAGER_FILEMANAGER_H

#include <QObject>
#include <QSharedPointer>
#include <QList>

#include <Protos/common.pb.h>
#include <Protos/core_protocol.pb.h>

#include <Common/LogManager/ILogger.h>
#include <IFileManager.h>
#include <priv/Cache/SharedDirectory.h>
#include <priv/FileUpdater/FileUpdater.h>
#include <priv/Cache/Cache.h>
#include <priv/ChunkIndex/Chunks.h>
#include <priv/WordIndex/WordIndex.h>

#define LOG_USER(mess)  FileManager::logger->log((mess), LogManager::EndUser)
#ifdef DEBUG
#  define LOG_DEBUG(mess) FileManager::logger->log((mess), LogManager::Debug)
#  define LOG_WARN(mess)  FileManager::logger->log((mess), LogManager::Warning)
#  define LOG_ERR(mess)   FileManager::logger->log((mess), LogManager::Error)
#  define LOG_FATAL(mess) FileManager::logger->log((mess), LogManager::FatalError)
#else
#  define LOG_DEBUG(mess)
#  define LOG_WARN(mess)
#  define LOG_ERR(mess)
#  define LOG_FATAL(mess)
#endif

namespace FM
{
   class Entry;
   class File;
   class Directory;
   class IChunk;

   class FileManager : public IFileManager
   {
      Q_OBJECT
   public :
      static const int  MAX_WORD_LENGTH = 3;
      static QSharedPointer<LogManager::ILogger> logger;

      FileManager();

      QStringList getSharedDirsReadOnly();
      QStringList getSharedDirsReadWrite();
      void setSharedDirsReadOnly(const QStringList& dirs);
      void setSharedDirsReadWrite(const QStringList& dirs);
      IChunk* getChunk(const Common::Hash& hash);
      /*IGetHashesResult* getHashes(const  Protos::Common::FileEntry& entry);
      Protos::Core::GetEntriesResult* getEntries(const Protos::Common::DirEntry& entry);*/
      Protos::Common::FindResult find(const QString& words);
      /*QList<bool> haveChunks(const QList<Common::Hash>& hashes);
      quint64 getAmount();
      QList<Protos::Common::DirEntry> getSharedDirs();
      QList<Protos::Common::DirEntry> getDestinationDirs();
      IFile newFile(const Protos::Common::FileEntry& remotEntry);*/

      File* getFile(const QString& path, const QString& name);
      Directory* getDir(const QString& path, const QString& name);

   public slots:
      void entryAdded(Entry* entry);
      void entryRemoved(Entry* entry);

   private:
      /**
        * Take raw terms in a string and split, trim and filter to
        * return a list of keyword.
        * Some character or word can be removed.
        * @example " The little   DUCK " => ["little", "duck"].
        */
      static QStringList splitInWords(const QString& words);

      FileUpdater fileUpdater;
      Cache cache; ///< The files and directories.
      Chunks chunks; ///< The indexed chunks.
      WordIndex<Entry*> wordIndex; ///< The word index.
   };
}
#endif
