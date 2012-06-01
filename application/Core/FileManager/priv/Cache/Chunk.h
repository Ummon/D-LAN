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
  
#ifndef FILEMANAGER_CHUNK_H
#define FILEMANAGER_CHUNK_H

#include <exception>

#include <QByteArray>

#include <Protos/files_cache.pb.h>

#include <Common/Settings.h>
#include <Common/Hash.h>
#include <Common/Uncopyable.h>

#include <IChunk.h>
#include <Exceptions.h>
#include <priv/Log.h>
#include <priv/Constants.h>
#include <priv/Cache/File.h>

namespace FM
{
   class File;
   class IDataReader;
   class IDataWriter;

   class Chunk : public IChunk, Common::Uncopyable
   {
   public:      
      static int CHUNK_SIZE;

      /**
        * Create a new empty chunk.
        */
      Chunk(File* file, int num, quint32 knownBytes);
      Chunk(File* file, int num, quint32 knownBytes, const Common::Hash& hash);

      ~Chunk();

      QString toStringLog() const;

      Chunk* restoreFromFileCache(const Protos::FileCache::Hashes_Chunk& chunk);

      void populateHashesChunk(Protos::FileCache::Hashes_Chunk& chunk) const;

      void removeItsIncompleteFile();
      bool populateEntry(Protos::Common::Entry* entry) const;
      QString getFilePath() const;

      QSharedPointer<IDataReader> getDataReader();
      QSharedPointer<IDataWriter> getDataWriter();

      void newDataWriterCreated();
      void newDataReaderCreated();

      void dataWriterDeleted();
      void dataReaderDeleted();

      void fileDeleted();

      inline int read(char* buffer, int offset);
      inline bool write(const char* buffer, int nbBytes);

      int getNum() const;
      int getNbTotalChunk() const;
      QVector<QSharedPointer<Chunk>> getOtherChunks() const;

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
      File* file;
      const int num; // First is 0.
      int knownBytes; ///< Relative offset, 0 means we don't have any byte and CHUNK_SIZE means we have all the chunk data.
      Common::Hash hash;
   };
}


/**
  * Fill the given buffer with read bytes.
  *
  * @exception IOErrorException
  * @exception ChunkDeletedException
  * @exception ChunkDataUnknownException
  * @param buffer The buffer.
  * @param offset The offset relative to the chunk.
  * @return The number of read bytes. If lesser than 'buffer.size' the end of file has been reached
  *         and the buffer will be partially filled.
  */
inline int FM::Chunk::read(char* buffer, int offset)
{
   static const int BUFFER_SIZE_READING = SETTINGS.get<quint32>("buffer_size_reading");

   if (!this->file)
      throw ChunkDeletedException();

   if (this->knownBytes == 0)
      throw ChunkDataUnknownException();

   if (offset >= this->knownBytes)
      return 0;

   const int bytesRemaining = this->getChunkSize() - offset;
   return this->file->read(buffer, offset + static_cast<qint64>(this->num) * CHUNK_SIZE, bytesRemaining >= BUFFER_SIZE_READING ? BUFFER_SIZE_READING : bytesRemaining);
}

/**
  * Write the given buffer after 'knownBytes'.
  * @exception IOErrorException
  * @exception ChunkDeletedException
  * @exception TryToWriteBeyondTheEndOfChunkException
  * @return 'true' if end of chunk reached.
  */
inline bool FM::Chunk::write(const char* buffer, int nbBytes)
{
   if (!this->file)
      throw ChunkDeletedException();

   const int CURRENT_CHUNK_SIZE = this->getChunkSize();

   if (this->knownBytes + nbBytes > CURRENT_CHUNK_SIZE)
      throw TryToWriteBeyondTheEndOfChunkException();

   this->knownBytes += this->file->write(buffer, nbBytes, this->knownBytes + static_cast<qint64>(this->num) * CHUNK_SIZE);

   if (this->knownBytes > CURRENT_CHUNK_SIZE) // Should never be true.
   {
      L_ERRO("Chunk::write(..) : this->knownBytes > getChunkSize");
      this->knownBytes = CURRENT_CHUNK_SIZE;
   }

   const bool COMPLETE = this->knownBytes == CURRENT_CHUNK_SIZE;

   if (COMPLETE)
      this->file->chunkComplete(this);

   return COMPLETE;
}

#endif
