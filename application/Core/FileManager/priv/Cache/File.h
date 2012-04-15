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
  
#ifndef FILEMANAGER_FILE_H
#define FILEMANAGER_FILE_H

#include <exception>

#include <QString>
#include <QMutex>
#include <QWaitCondition>
#include <QFile>
#include <QFileInfo>
#include <QList>
#include <QSharedPointer>
#include <QDateTime>

#include <Protos/common.pb.h>
#include <Protos/files_cache.pb.h>

#include <Common/Hashes.h>

#include <priv/Cache/Entry.h>

namespace FM
{
   class IChunk;
   class Chunk;
   class Directory;
   class SharedDirectory;
   class Cache;

   class File : public Entry
   {
   public:
      File(
         Directory* dir,
         const QString& name,
         qint64 size,
         const QDateTime& dateLastModified,
         const Common::Hashes& hashes = Common::Hashes(),
         bool createPhysically = false
      );

      virtual ~File();

      void setToUnfinished(qint64 size, const Common::Hashes& hashes = Common::Hashes());

      bool restoreFromFileCache(const Protos::FileCache::Hashes::File& file);
      void populateHashesFile(Protos::FileCache::Hashes_File& fileToFill) const;

      void populateEntry(Protos::Common::Entry* entry, bool setSharedDir = false) const;
      bool matchesEntry(const Protos::Common::Entry& entry) const;

      bool correspondTo(const QFileInfo& fileInfo, bool checkTheDateToo = true);

      QString getPath() const;
      QString getFullPath() const;
      SharedDirectory* getRoot() const;
      void changeName(const QString& newName);
      QDateTime getDateLastModified() const;

      void newDataWriterCreated();
      void newDataReaderCreated();

      void dataWriterDeleted();
      void dataReaderDeleted();

      qint64 write(const char* buffer, int nbBytes, qint64 offset);
      qint64 read(char* buffer, qint64 offset, int maxBytesToRead);

      QList< QSharedPointer<Chunk> > getChunks() const;
      bool hasAllHashes();
      bool hasOneOrMoreHashes();

      bool isComplete();
      void setAsComplete();
      void chunkComplete(const Chunk* chunk);

      int getNbChunks();

      void deleteIfIncomplete();
      void removeUnfinishedFiles();

      void changeDirectory(Directory* dir);
      bool hasAParentDir(Directory* dir);

      ///// Methods dedicated to the hasher.
      void setSize(qint64 size);
      void updateDateLastModified(const QDateTime& date);
      void addChunk(const QSharedPointer<Chunk>& chunk);
      QSharedPointer<Chunk> removeLastChunk();
      /////

   private:
      void deleteAllChunks();
      void createPhysicalFile();
      void setHashes(const Common::Hashes& hashes);

      Directory* dir;
      QList< QSharedPointer<Chunk> > chunks;
      QDateTime dateLastModified;

      // Used only when writing a file.
      int nbChunkComplete;
      bool complete;

      int numDataWriter;
      int numDataReader;
      QFile* fileInWriteMode;
      QFile* fileInReadMode;
      QMutex writeLock; ///< Protect the file from concurrent access from different downloaders.
      QMutex readLock; ///< Protect the file from concurrent access from different uploaders.
      mutable QMutex mutex;
   };
}
#endif
