/**
  * D-LAN - A decentralized LAN file sharing software.
  * Copyright (C) 2010-2012 Greg Burri <greg.burri@gmail.com>
  *
  * This program is free software: you can redistribute it and/or modify
  * it under the terms of the GNU General Public License as published by
  * the Free Software Foundation, either version 3 of the License, or
  * (at your option) any later version.
  *
  * This program is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  *
  * You should have received a copy of the GNU General Public License
  * along with this program.  If not, see <http://www.gnu.org/licenses/>.
  */

#pragma once

#include <limits>

#include <QObject>
#include <QSharedPointer>
#include <QList>
#include <QBitArray>
#include <QMutex>
#include <QTimer>

#include <Protos/common.pb.h>
#include <Protos/core_protocol.pb.h>

#include <Common/Uncopyable.h>

#include <Core/HashCache/IHashCache.h>

#include <IFileManager.h>
#include <priv/Log.h>
#include <priv/FileUpdater/FileUpdater.h>
#include <priv/Cache/Cache.h>
#include <priv/Cache/Entry.h>
#include <priv/ChunkIndex/Chunks.h>
#include <priv/WordIndex/WordIndex.h>
#include <priv/ExtensionIndex.h>
#include <priv/SizeIndex.h>

namespace FM
{
   class Chunk;
   class Directory;
   class IChunk;
   class IGetHashesResult;
   class IGetEntriesResult;

   class FileManager : public IFileManager, Common::Uncopyable
   {
      Q_OBJECT
   public:
      FileManager(QSharedPointer<HC::IHashCache> hashCache);
      ~FileManager();

      void setSharedPaths(const QList<IFileManager::SharedPath>& paths) override;
      QPair<Common::SharedEntry, QString> addASharedPath(const QString& absolutePath) override;

      QList<Common::SharedEntry> getSharedEntries() const override;
      QString getSharedEntry(const Common::Hash& ID) const override;

      QSharedPointer<IChunk> getChunk(const Common::Hash& hash) const override;

      QList<QSharedPointer<IChunk>> getAllChunks(
         const Protos::Common::Entry& localEntry,
         const QList<Common::Hash>& hashes
      ) const override;

      void setHashesAndKnownBytesToUnfinishedFile(
         const Protos::Common::Entry& localEntry,
         const QList<Common::Hash>& hashes,
         const QList<int> knownBytes
      ) override;

      QList<QSharedPointer<IChunk>> newFile(Protos::Common::Entry& entry) override;
      void newDirectory(Protos::Common::Entry& entry) override;
      QSharedPointer<IGetHashesResult> getHashes(const Protos::Common::Entry& file) override;

      QSharedPointer<IGetEntriesResult> getScannedEntries(
         const Protos::Common::Entry& dir,
         int maxNbHashesPerEntry = std::numeric_limits<int>::max()
      ) override;

      Protos::Common::Entries getEntries(
         const Protos::Common::Entry& dir,
         int maxNbHashesPerEntry = std::numeric_limits<int>::max()
      ) override;
      Protos::Common::Entries getEntries() override;

      inline QList<Protos::Common::FindResult> find(const QString& words, int maxNbResult, int maxSize) override
      {
         return
            this->find(
               words,
               QList<QString>(),
               0,
               std::numeric_limits<qint64>::max(),
               Protos::Common::FindPattern::FILE_DIR,
               maxNbResult,
               maxSize,
               true
            );
      }

      QList<Protos::Common::FindResult> find(
         const QString& words,
         const QList<QString>& extensions,
         qint64 minFileSize,
         qint64 maxFileSize,
         Protos::Common::FindPattern_Category category,
         int maxNbResult,
         int maxSize,
         bool setSharedEntryPath
      ) override;

      QBitArray haveChunks(const QList<Common::Hash>& hashes) override;
      qint64 getAmount() override;
      CacheStatus getCacheStatus() const override;
      int getProgress() const override;

      QString getWordIndex_debug() const override;
      QString getSimilarFiles_debug() const override;
      QString getCacheTree_debug() const override;

      Directory* getFittestDirectory(const QString& path);
      Entry* getEntry(const Common::Path& path) const;
      SharedEntry* getSharedEntry(const QString& path) const;

   private slots:
      void newSharedEntry(FM::SharedEntry*);
      void sharedEntryRemoved(FM::SharedEntry*, FM::Directory*);
      void deleteSharedEntry(FM::SharedEntry* sharedEntry);
      void entryAdded(FM::Entry* entry);
      void entryRemoved(FM::Entry* entry);
      void entryRenamed(FM::Entry* entry, const QString& oldName);

      void fileResizing(FM::File* file);
      void fileResized(FM::File* file, qint64 oldSize);

      void chunkHashKnown(const QSharedPointer<FM::Chunk>& chunk);
      void chunkRemoved(const QSharedPointer<FM::Chunk>& chunk);

      void setInitialFileCacheScanningComplete();

   private:
      LOG_INIT_H("FileManager")

      FileUpdater fileUpdater;
      Cache cache; ///< The files and directories.
      Chunks chunks; ///< The indexed chunks. It contains only completed chunks.

      WordIndex<Entry*> wordIndex;

      ExtensionIndex<File*> extensionIndex;
      SizeIndex sizeIndex;

      QMutex mutexCacheChanged;

      bool initialFileCacheScanningComplete = false;
   };
}
