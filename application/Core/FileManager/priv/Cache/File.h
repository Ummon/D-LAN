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
#include <QVector>
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
   class FileForHasher;

   class File : public Entry
   {
      friend class Directory;

   public:
      File(
         SharedEntry* root,
         const QString& name,
         qint64 size,
         const QDateTime& dateLastModified,
         Directory* parentDirectory = nullptr,
         const QList<Common::Hash>& hashes = QList<Common::Hash>(),
         bool createPhysically = false
      );

      ~File() override;

      void del(bool invokeDelete = true) override;

      FileForHasher* asFileForHasher();

      void setToUnfinished(qint64 size, const QList<Common::Hash>& hashes = QList<Common::Hash>());

      Directory* createSubDirs(const QStringList& names, bool physically = false);

      // bool restoreFromFileCache(const Protos::FileCache::Hashes::File& file);
      // void populateHashesFile(Protos::FileCache::Hashes_File& fileToFill) const;

      void populateEntry(Protos::Common::Entry* entry, bool setSharedDir = false) const override;
      void populateEntry(Protos::Common::Entry* entry, bool setSharedDir, int maxHashes) const;
      bool matchesEntry(const Protos::Common::Entry& entry) const;

      bool correspondTo(const QFileInfo& fileInfo, bool checkTheDateToo = true) const;
      void fileHasChangedOnDisk(const QFileInfo fileInfo);

      Common::Path getRelativePath() const override;
      Common::Path getAbsolutePath() const override;
      Entry* getEntry(const Common::Path& path) override;

      void rename(const QString& newName) override;
      QDateTime getDateLastModified() const;

      void newDataWriterCreated();
      void newDataReaderCreated();

      void dataWriterDeleted();
      void dataReaderDeleted();

      qint64 write(const char* buffer, int nbBytes, qint64 offset);
      qint64 read(char* buffer, qint64 offset, int maxBytesToRead);

      QVector<QSharedPointer<Chunk>> getChunks() const;
      bool hasAllHashes() const;
      bool hasOneOrMoreHashes() const;

      bool isComplete() const;
      void chunkComplete(const Chunk* chunk);

      int getNbChunks() const;

      void deleteIfIncomplete();
      void removeUnfinishedFiles() override;

      void moveInto(Directory* directory) override;

      // void changeDirectory(Directory* dir);
      bool hasAParentDir(Directory* dir);

   private:
      void setAsComplete();
      void deleteAllChunks();
      void createPhysicalFile();
      static void setFileAsSparse(const QFile& file);
      void setHashes(const QList<Common::Hash>& hashes);

   protected:
      void setRootRecursively(SharedEntry* sharedEntry) override;

      QVector<QSharedPointer<Chunk>> chunks;
      QDateTime dateLastModified;

   private:
      bool complete;

      quint16 numDataWriter;
      quint16 numDataReader;
      QFile* fileInWriteMode;
      QFile* fileInReadMode;
      QMutex writeLock; ///< Protect the file from concurrent access from different downloaders.
      QMutex readLock; ///< Protect the file from concurrent access from different uploaders.
   };

   /**
     * A class dedicated to the hasher.
     * It must not add any member data because an allocated 'File' object may be downcasted to a 'FileForHasher' type.
     */
   class FileForHasher : public File
   {
   public:
      void setSize(qint64 size);
      void updateDateLastModified(const QDateTime& date);
      void addChunk(const QSharedPointer<Chunk>& chunk);
      QSharedPointer<Chunk> removeLastChunk();
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
