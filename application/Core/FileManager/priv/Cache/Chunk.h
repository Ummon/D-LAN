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
  
#ifndef FILEMANAGER_CHUNK_H
#define FILEMANAGER_CHUNK_H

#include <exception>

#include <QByteArray>
#include <QMutex>

#include <Protos/files_cache.pb.h>

#include <Common/Hash.h>
#include <Common/Uncopyable.h>
#include <IChunk.h>
#include <priv/Constants.h>

namespace FM
{
   class File;
   class IDataReader;
   class IDataWriter;

   class Chunk : public IChunk, Common::Uncopyable
   {
   public:
      /**
        * Create a new empty chunk.
        */
      Chunk(File* file, int num, quint32 knownBytes);
      Chunk(File* file, int num, quint32 knownBytes, const Common::Hash& hash);

   public:
      ~Chunk();

      QString toStringLog() const;

      Chunk* restoreFromFileCache(const Protos::FileCache::Hashes_Chunk& chunk);

      void populateHashesChunk(Protos::FileCache::Hashes_Chunk& chunk) const;

      void removeItsIncompleteFile();
      bool populateEntry(Protos::Common::Entry* entry) const;
      QString getBasePath() const;

      QSharedPointer<IDataReader> getDataReader();
      QSharedPointer<IDataWriter> getDataWriter();

      void newDataWriterCreated();
      void newDataReaderCreated();

      void dataWriterDeleted();
      void dataReaderDeleted();

      void fileDeleted();

      int read(char* buffer, int offset);
      bool write(const char* buffer, int nbBytes);

      int getNum() const;
      int getNbTotalChunk() const;
      QList< QSharedPointer<Chunk> > getOtherChunks() const;

      bool hasHash() const;
      Common::Hash getHash() const;
      void setHash(const Common::Hash& hash);

      int getKnownBytes() const;
      void setKnownBytes(int bytes);

      int getChunkSize() const;
      bool isComplete() const;

      bool isOwnedBy(File* file) const;

      bool matchesEntry(const Protos::Common::Entry& entry) const;

   private:
      const int CHUNK_SIZE;

      mutable QMutex mutex; ///< Protect 'file' against multiple access.

      File* file;
      int num; // First is 0.
      int knownBytes; ///< Relative offset, 0 means we don't have any byte and CHUNK_SIZE means we have all the chunk data.
      Common::Hash hash;
   };
}
#endif
