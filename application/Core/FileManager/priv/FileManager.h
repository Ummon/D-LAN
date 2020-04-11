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
#include <priv/SizeIndexEntries.h>

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

      void setSharedPaths(const QStringList& paths);
      QPair<Common::SharedEntry, QString> addASharedPath(const QString& absolutePath);
      QList<Common::SharedEntry> getSharedEntries() const;
      SharedEntry* getSharedEntry(const Common::Hash& ID) const;

      QSharedPointer<IChunk> getChunk(const Common::Hash& hash) const;
      QList<QSharedPointer<IChunk>> getAllChunks(const Protos::Common::Entry& localEntry, const Common::Hashes& hashes) const;
      QList<QSharedPointer<IChunk>> newFile(Protos::Common::Entry& entry);
      void newDirectory(Protos::Common::Entry& entry);
      QSharedPointer<IGetHashesResult> getHashes(const Protos::Common::Entry& file);

      QSharedPointer<IGetEntriesResult> getScannedEntries(const Protos::Common::Entry& dir, int maxNbHashesPerEntry = std::numeric_limits<int>::max());
      Protos::Common::Entries getEntries(const Protos::Common::Entry& dir, int maxNbHashesPerEntry = std::numeric_limits<int>::max());
      Protos::Common::Entries getEntries();

      inline QList<Protos::Common::FindResult> find(const QString& words, int maxNbResult, int maxSize) { return this->find(words, QList<QString>(), 0, std::numeric_limits<qint64>::max(), Protos::Common::FindPattern::FILE_DIR, maxNbResult, maxSize); }
      QList<Protos::Common::FindResult> find(const QString& words, const QList<QString>& extensions, qint64 minFileSize, qint64 maxFileSize, Protos::Common::FindPattern_Category category, int maxNbResult, int maxSize);
      QBitArray haveChunks(const QList<Common::Hash>& hashes);
      quint64 getAmount();
      CacheStatus getCacheStatus() const;
      int getProgress() const;

      void dumpWordIndex() const;
      void printSimilarFiles() const;

      Directory* getFittestDirectory(const QString& path);
      Entry* getEntry(const QString& path) const;
      SharedEntry* getSharedEntry(const QString& path) const;

   private slots:
      void newSharedEntry(SharedEntry*);
      void sharedEntryRemoved(SharedEntry*, Directory*);
      void deleteSharedEntry(SharedEntry* sharedEntry);
      void entryAdded(Entry* entry);
      void entryRemoved(Entry* entry);
      void entryRenamed(Entry* entry, const QString& oldName);
      void entryResizing(Entry* entry);
      void entryResized(Entry* entry, qint64 oldSize);
      void chunkHashKnown(const QSharedPointer<Chunk>& chunk);
      void chunkRemoved(const QSharedPointer<Chunk>& chunk);

   private:
      void loadCacheFromFile();

   private slots:
      void persistCacheToFile();
      void forcePersistCacheToFile();
      void setCacheChanged();
      void fileCacheLoadingComplete();

   private:
      LOG_INIT_H("FileManager")

      FileUpdater fileUpdater;
      Cache cache; ///< The files and directories.
      Chunks chunks; ///< The indexed chunks. It contains only completed chunks.

      WordIndex<Entry*> wordIndex;
      ExtensionIndex<Entry*> extensionIndex;
      SizeIndexEntries sizeIndex;

      QTimer timerPersistCache;
      QMutex mutexPersistCache;
      QMutex mutexCacheChanged; ///< We use a second mutex (instead of using 'mutexPersistCache') to avoid deadlock created by "File -> chunkHashKnown()" and "persistCacheToFile() -> File".
      bool cacheLoading; ///< Set to 'true' during cache loading. It avoids to persist the cache during loading.
      bool cacheChanged;
   };
}
