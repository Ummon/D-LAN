/**
  * Aybabtu - A decentralized LAN file sharing software.
  * Copyright (C) 2010-2011 Greg Burri <greg.burri@gmail.com>
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
  
#ifndef FILEMANAGER_FILEMANAGER_H
#define FILEMANAGER_FILEMANAGER_H

#include <QObject>
#include <QSharedPointer>
#include <QList>
#include <QBitArray>
#include <QMutex>
#include <QTimer>

#include <Protos/common.pb.h>
#include <Protos/core_protocol.pb.h>

#include <Common/Uncopyable.h>

#include <IFileManager.h>
#include <priv/Log.h>
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

   class FileManager : public IFileManager, Common::Uncopyable
   {
      Q_OBJECT
   public:
      FileManager();
      ~FileManager();

      void setSharedDirs(const QStringList& dirs);
      QList<Common::SharedDir> getSharedDirs() const;
      QString getSharedDir(const Common::Hash& ID) const;

      QSharedPointer<IChunk> getChunk(const Common::Hash& hash) const;
      QList< QSharedPointer<IChunk> > getAllChunks(const Protos::Common::Entry& localEntry, const Common::Hashes& hashes) const;
      QList< QSharedPointer<IChunk> > newFile(Protos::Common::Entry& entry);
      QSharedPointer<IGetHashesResult> getHashes(const Protos::Common::Entry& file);

      Protos::Common::Entries getEntries(const Protos::Common::Entry& dir);
      Protos::Common::Entries getEntries();

      QList<Protos::Common::FindResult> find(const QString& words, int maxNbResult, int maxSize);
      QBitArray haveChunks(const QList<Common::Hash>& hashes);
      quint64 getAmount();
      CacheStatus getCacheStatus() const;

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
      void forcePersistCacheToFile();
      void setCacheChanged();

   private:
      LOG_INIT_H("FileManager");

      const quint32 CHUNK_SIZE;

      FileUpdater fileUpdater;
      Cache cache; ///< The files and directories.
      Chunks chunks; ///< The indexed chunks. It contains only completed chunks.
      WordIndex<Entry*> wordIndex; ///< The word index.

      QTimer timerPersistCache;
      QMutex mutexPersistCache;
      QMutex mutexCacheChanged; ///< We use a second mutex (instead of using 'mutexPersistCache') to avoid deadlock created by "File -> chunkHashKnown()" and "persistCacheToFile() -> File".
      bool cacheLoading; ///< Set to 'true' during cache loading. It avoids to persist the cache during loading.
      bool cacheChanged;
   };
}
#endif
