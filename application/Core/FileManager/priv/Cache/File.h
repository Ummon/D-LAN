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

#include <QString>
#include <QList>
#include <QMutex>
#include <QWaitCondition>
#include <QFile>
#include <QFileInfo>
#include <QSharedPointer>
#include <QDateTime>

#include <Protos/common.pb.h>
#include <Common/Hash.h>

#include <priv/Cache/Entry.h>

namespace FM
{
   class Chunk;
   class Directory;
   class SharedEntry;

   class File : public Entry
   {
      friend class Directory;
      friend class Chunk;

   public:
      File(
         SharedEntry* root,
         const QString& name,
         qint64 size,
         bool hidden,
         const QDateTime& dateLastModified,
         Directory* parentDirectory = nullptr,
         const QList<Common::Hash>& hashes = QList<Common::Hash>(),
         bool createPhysically = false
      );

      ~File() override;

      void del(bool invokeDelete = true) override;

      void setToUnfinished(qint64 size, const QList<Common::Hash>& hashes = QList<Common::Hash>());

      void saveHashes();
      void loadHashes();

      void populateEntry(Protos::Common::Entry* entry, bool setSharedDir = false) const override;
      void populateEntry(Protos::Common::Entry* entry, bool setSharedDir, int maxHashes) const;
      bool matchesEntry(const Protos::Common::Entry& entry) const;

      bool correspondTo(const QFileInfo& fileInfo, bool checkTheDateToo = true) const;
      void fileHasChangedOnDisk(const QFileInfo fileInfo);

      Common::Path getRelativePath() const override;
      Common::Path getAbsolutePath() const override;
      Entry* getEntry(const Common::Path& path) override;

      QString getExtension() const;

      void rename(const QString& newName) override;
      QDateTime getDateLastModified() const;

      void newDataWriterCreated();
      void newDataReaderCreated();

      void dataWriterDeleted();
      void dataReaderDeleted();

      qint64 write(const char* buffer, int nbBytes, qint64 offset);
      void flushWrittenData();
      qint64 read(char* buffer, qint64 offset, int maxBytesToRead);

      QList<QSharedPointer<Chunk>> getChunks() const;
      bool hasAllHashes() const;
      qint64 getRemainingBytesToHash() const;
      bool hasOneOrMoreHashes() const;

      bool isComplete() const;
      void chunkComplete(const Chunk* chunk);

      int getNbChunks() const;

      virtual void setSize(qint64 size) override;

      void deleteIfIncomplete();
      void removeUnfinishedFiles() override;

      void moveInto(Directory* directory) override;

      bool hasAParentDir(Directory* dir);

   private:
      friend class GetHashesResult; // Subscribe and validate a chunk generation under the file lock.
      friend class FileHasher; // Publish validated hashes while excluding generation retirement.
      QSharedPointer<QRecursiveMutex> getChunkMutex() const { return this->mutexStorage; }
      void setAsComplete();
      void deleteAllChunks();
      void closePhysicalFiles();
      bool openReadHandle();
      void createPhysicalFile();
      static void setFileAsSparse(const QFile& file);
      static void setFileAsHidden(const QString& filepath);
      void setHashes(const QList<Common::Hash>& hashes);
      int getFirstUnhashedChunk() const;
      void chunkHashChanged(const Chunk* chunk, bool hadHash, bool hasHash);
      void rebuildHashingProgress();

   protected:
      void setRootRecursively(SharedEntry* sharedEntry) override;

      // Called with writeLock held. May return a short write, zero, or -1; must not acquire Entry::mutex.
      virtual qint64 writePhysicalFile(const char* buffer, qint64 nbBytes);

      QList<QSharedPointer<Chunk>> chunks;
      QDateTime dateLastModified;

   private:
      // Protected by Entry::mutex; reset whenever the chunk generation is replaced.
      qint64 remainingBytesToHash = 0;
      mutable int firstUnhashedChunk = 0;
      // 'atomic' to avoid using the mutex in 'isComplete()', it can cause deadlocks when called by 'FileUpdater'.
      std::atomic<bool> complete;

      quint16 numDataWriter;
      quint16 numDataReader;
      QFile* fileInWriteMode;
      QFile* fileInReadMode;
      // When combined, acquire Entry::mutex before writeLock, then readLock.
      // I/O-only methods may take an I/O lock alone, but must not acquire Entry::mutex afterwards
      // (including indirectly through getCache() or getAbsolutePath()).
      QMutex writeLock; ///< Protect the file from concurrent access from different downloaders.
      QMutex readLock; ///< Protect the file from concurrent access from different uploaders.
   };

   class FileIterator
   {
   public:
      FileIterator(Entry* entry);
      virtual ~FileIterator() {}
      File* next();

   private:
      QList<File*> nextFiles;
      QList<Directory*> dirsToVisit;
   };
}
