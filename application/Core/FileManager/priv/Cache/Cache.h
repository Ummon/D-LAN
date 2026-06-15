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

#include <functional>

#include <QObject>
#include <QPair>
#include <QList>
#include <QStringList>
#include <QRecursiveMutex>
#include <QSharedPointer>

#include <Protos/common.pb.h>
#include <Protos/core_protocol.pb.h>

#include <Core/HashCache/IHashCache.h>

#include <Common/Uncopyable.h>
#include <Common/SharedEntry.h>

#include <priv/FileUpdater/DirWatcher.h>
#include <priv/Cache/Entry.h>
#include <priv/Cache/SharedEntry.h>
#include <priv/Cache/Chunk.h>
#include <priv/Cache/FilePool.h>

namespace FM
{
   class Entry;
   class FileUpdater;

   class Cache : public QObject, Common::Uncopyable
   {
      Q_OBJECT
   public:
      Cache(QSharedPointer<HC::IHashCache> hashCache);
      ~Cache();

      void forall(std::function<void(Entry*)> fun) const;

      Protos::Common::Entries getProtoSharedEntries() const;
      Protos::Common::Entries getProtoEntries(
         const Protos::Common::Entry& dir,
         int maxNbHashesPerEntry = std::numeric_limits<int>::max()
      ) const;

      Directory* getDirectory(const Protos::Common::Entry& dir) const;
      Entry* getEntry(const Common::Path& path) const;
      SharedEntry* getSharedEntry(const Common::Path& path) const;
      File* getFile(const Protos::Common::Entry& fileEntry) const;
      QList<QSharedPointer<IChunk>> newFile(Protos::Common::Entry& fileEntry);
      void newDirectory(Protos::Common::Entry& dirEntry);

      QList<Common::SharedEntry> getSharedEntries() const;
      SharedEntry* getSharedEntry(const Common::Hash& ID) const;

      void setSharedPaths(const QList<std::pair<QString, Common::Path>>& paths);
      QPair<Common::SharedEntry, QString> addASharedPath(const QString& absolutePath);

      void addExistingSharedEntry(const Protos::Common::SharedEntry& sharedEntry);
      void removeSharedEntry(SharedEntry* entry, Directory* dir = nullptr);

      SharedDirectory* getSuperSharedDirectory(const Common::Path& path) const;
      QList<SharedEntry*> getSubSharedEntries(const Common::Path& path) const;
      bool isShared(const Common::Path& path) const;

      Directory* getFittestDirectory(const Common::Path& path) const;

      quint64 getAmount() const;

      FilePool& getFilePool() { return this->filePool; }

      QString getTree_debug() const;

      void onEntryAdded(Entry* entry);
      void onEntryRemoved(Entry* entry);
      void onEntryRenamed(Entry* entry, const QString& oldName);
      void onFileResizing(File* file);
      void onFileResized(File* file, qint64 oldSize);

      void onChunkHashKnown(const QSharedPointer<Chunk>& chunk);
      void onChunkRemoved(const QSharedPointer<Chunk>& chunk);

      void onScanned(Directory* dir);

   public slots:
      void deleteEntry(FM::Entry* entry);

   signals:
      void entryAdded(FM::Entry* entry);
      void entryRemoved(FM::Entry* entry);
      void entryRenamed(FM::Entry* entry, const QString& oldName);
      void fileResizing(FM::File* file);
      void fileResized(FM::File* file, qint64 oldSize);

      /**
        * May be emitted from a separated thread.
        */
      void chunkHashKnown(const QSharedPointer<FM::Chunk>& chunk);
      void chunkRemoved(const QSharedPointer<FM::Chunk>& chunk);
      void directoryScanned(FM::Directory* dir);

      void newSharedEntry(FM::SharedEntry* entry);
      void sharedEntryRemoved(FM::SharedEntry* entry, FM::Directory* dir);

   private:
      static Common::SharedEntry makeSharedEntry(const SharedEntry* entry);
      SharedEntry* createSharedEntry(
         const Common::Path& path,
         const Common::Hash& ID = Common::Hash(),
         int pos = -1,
         const QString& name = QString()
      );
      void saveSharedEntries() const;

      Directory* getWriteableDirectory(const Common::Path& path, qint64 spaceNeeded = 0) const;

      QSharedPointer<HC::IHashCache> hashCache;

      QList<SharedEntry*> sharedEntries;

      FilePool filePool;

      const quint32 MINIMUM_FREE_SPACE;

      mutable QRecursiveMutex mutex; ///< To protect all the data in the cache, files and directories.
   };
}
