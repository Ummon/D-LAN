/**
  * D-LAN - A decentralized LAN file sharing software.
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
  
#ifndef FILEMANAGER_CACHE_H
#define FILEMANAGER_CACHE_H

#include <QObject>
#include <QList>
#include <QStringList>
#include <QMutex>
#include <QSharedPointer>

#include <Protos/files_cache.pb.h>
#include <Protos/core_protocol.pb.h>

#include <Common/Uncopyable.h>
#include <Common/SharedDir.h>

#include <priv/FileUpdater/DirWatcher.h>
#include <priv/Cache/SharedDirectory.h>
#include <priv/Cache/Chunk.h>

namespace FM
{
   class Entry;
   class FileUpdater;
   class FileManager;

   class Cache : public QObject, Common::Uncopyable
   {
      Q_OBJECT
   public:
      Cache(FileManager* fileManager);
      ~Cache();

      Protos::Common::Entries getEntries(const Protos::Common::Entry& dir) const;
      Protos::Common::Entries getEntries() const;
      Entry* getEntry(const QString& path) const;
      File* getFile(const Protos::Common::Entry&) const;
      QList< QSharedPointer<IChunk> > newFile(Protos::Common::Entry& fileEntry);

      QList<Common::SharedDir> getSharedDirs() const;
      SharedDirectory* getSharedDirectory(const Common::Hash& ID) const;
      void setSharedDirs(const QStringList& dirs);
      void removeSharedDir(SharedDirectory* dir, Directory* dir2 = 0);

      SharedDirectory* getSuperSharedDirectory(const QString& path) const;
      QList<SharedDirectory*> getSubSharedDirectories(const QString& path) const;
      bool isShared(const QString& path) const;

      Directory* getFittestDirectory(const QString& path) const;

      void retrieveFromFile(const Protos::FileCache::Hashes& hashes);
      void saveInFile(Protos::FileCache::Hashes& hashes) const;

      quint64 getAmount() const;

      void onEntryAdded(Entry* entry);
      void onEntryRemoved(Entry* entry);
      void onChunkHashKnown(QSharedPointer<Chunk> chunk);
      void onChunkRemoved(QSharedPointer<Chunk> chunk);

   signals:
      void entryAdded(Entry* entry);
      void entryRemoved(Entry* entry);
      void chunkHashKnown(QSharedPointer<Chunk> chunk);
      void chunkRemoved(QSharedPointer<Chunk> chunk);

      void newSharedDirectory(SharedDirectory* dir);
      void sharedDirectoryRemoved(SharedDirectory* dir, Directory* dir2);

   private:
      SharedDirectory* createSharedDir(const QString path, const Common::Hash& ID = Common::Hash(), int pos = -1);
      void createSharedDirs(const QStringList& dirs, const QList<Common::Hash>& ids = QList<Common::Hash>());
      void createSharedDirs(const Protos::FileCache::Hashes& hashes);

      Directory* getWriteableDirectory(const QString& path, qint64 spaceNeeded) const;

      QList<SharedDirectory*> sharedDirs;

      FileManager* fileManager;

      mutable QMutex mutex; ///< To protect all the data into the cache, files and directories.
   };
}
#endif
