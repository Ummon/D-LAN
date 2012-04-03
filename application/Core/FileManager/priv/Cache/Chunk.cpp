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
  
#include <priv/Cache/Chunk.h>
using namespace FM;

#include <Common/Settings.h>
#include <Common/ProtoHelper.h>

#include <IDataReader.h>
#include <IDataWriter.h>
#include <Exceptions.h>
#include <priv/Global.h>
#include <priv/Log.h>
#include <priv/Cache/File.h>
#include <priv/Cache/SharedDirectory.h>
#include <priv/Cache/DataReader.h>
#include <priv/Cache/DataWriter.h>

/**
  * @class FM::Chunk
  *
  * A chunk is a part of a file. It's identified by a hash which can be unknown when a chunk is created and be set later by 'setHash(..)'.
  * A chunk can be read or write, when a chunk is written the 'knownBytes' member is increased.
  * Each chunk of a file has a unique number which begins at 0 and define the order of data, chunk#1 represents the data right after chunk#0 and so on.
  */

Chunk::Chunk(File* file, int num, quint32 knownBytes) :
   CHUNK_SIZE(SETTINGS.get<quint32>("chunk_size")),
   mutex(QMutex::Recursive), file(file), num(num), knownBytes(knownBytes)
{
   QMutexLocker locker(&this->mutex);
   L_DEBU(QString("New chunk[%1] : %2. File : %3").arg(num).arg(hash.toStr()).arg(this->file->getFullPath()));
}

Chunk::Chunk(File* file, int num, quint32 knownBytes, const Common::Hash& hash) :
   CHUNK_SIZE(SETTINGS.get<quint32>("chunk_size")),
   mutex(QMutex::Recursive), file(file), num(num), knownBytes(knownBytes), hash(hash)
{
   QMutexLocker locker(&this->mutex);
   L_DEBU(QString("New chunk[%1] : %2. File : %3").arg(num).arg(hash.toStr()).arg(this->file->getFullPath()));
}

Chunk::~Chunk()
{
   QMutexLocker locker(&this->mutex);
   L_DEBU(QString("Chunk Deleted[%1] : %2. File : %3").arg(num).
      arg(this->hash.toStr()).
      arg(this->file ? this->file->getFullPath() : "<file deleted>")
   );
}

QString Chunk::toStringLog() const
{
   return QString("num = [%1], hash = %2, knownBytes = %3, size = %4").arg(this->num).arg(this->getHash().toStr()).arg(this->getKnownBytes()).arg(this->getChunkSize());
}

Chunk* Chunk::restoreFromFileCache(const Protos::FileCache::Hashes_Chunk& chunk)
{
   this->knownBytes = chunk.known_bytes();

   if (chunk.has_hash())
      this->hash = chunk.hash().hash();
   return this;
}

void Chunk::populateHashesChunk(Protos::FileCache::Hashes_Chunk& chunk) const
{
   chunk.set_known_bytes(this->knownBytes);
   if (!this->hash.isNull())
      chunk.mutable_hash()->set_hash(this->hash.getData(), Common::Hash::HASH_SIZE);
}

void Chunk::removeItsIncompleteFile()
{
   QMutexLocker locker(&this->mutex);
   if (this->file)
      this->file->deleteIfIncomplete();
}

bool Chunk::populateEntry(Protos::Common::Entry* entry) const
{
   QMutexLocker locker(&this->mutex);
   if (this->file)
   {
      this->file->populateEntry(entry);
      return true;
   }
   return false;
}

QString Chunk::getBasePath() const
{
   QMutexLocker locker(&this->mutex);
   if (this->file)
      return this->file->getRoot()->getFullPath();
   return QString();
}

QSharedPointer<IDataReader> Chunk::getDataReader()
{
   QMutexLocker locker(&this->mutex);
   return QSharedPointer<IDataReader>(new DataReader(*this));
}

QSharedPointer<IDataWriter> Chunk::getDataWriter()
{
   QMutexLocker locker(&this->mutex);
   return QSharedPointer<IDataWriter>(new DataWriter(*this));
}

void Chunk::newDataWriterCreated()
{
   QMutexLocker locker(&this->mutex);
   if (this->file)
      this->file->newDataWriterCreated();
}

void Chunk::newDataReaderCreated()
{
   QMutexLocker locker(&this->mutex);
   if (this->file)
      this->file->newDataReaderCreated();
}

void Chunk::dataWriterDeleted()
{
   QMutexLocker locker(&this->mutex);
   if (this->file)
      this->file->dataWriterDeleted();
}

void Chunk::dataReaderDeleted()
{
   QMutexLocker locker(&this->mutex);
   if (this->file)
      this->file->dataReaderDeleted();
}

/**
  * Called by a deleted file just before dying.
  */
void Chunk::fileDeleted()
{
   QMutexLocker locker(&this->mutex);
   this->file = 0;
}

/**
  * Fill the given buffer with read bytes.
  *
  * @exception IOErrorException
  * @exception ChunkDeletedException
  * @exception ChunkNotCompletedException
  * @param buffer The buffer.
  * @param offset The offset relative to the chunk.
  * @return The number of read bytes. If lesser than 'buffer.size' the end of file has been reached
  *         and the buffer will be partially filled.
  */
int Chunk::read(char* buffer, int offset)
{
   static const int BUFFER_SIZE_READING = SETTINGS.get<quint32>("buffer_size_reading");

   QMutexLocker locker(&this->mutex);
   if (!this->file)
      throw ChunkDeletedException();

   if (this->knownBytes == 0)
      throw ChunkNotCompletedException();

   if (offset >= this->knownBytes)
      return 0;

   int bytesRemaining = this->getChunkSize() - offset;
   return this->file->read(buffer, offset + static_cast<qint64>(this->num) * CHUNK_SIZE, bytesRemaining >= BUFFER_SIZE_READING ? BUFFER_SIZE_READING : bytesRemaining);
}

/**
  * Write the given buffer after 'knownBytes'.
  * @exception IOErrorException
  * @exception ChunkDeletedException
  * @exception TryToWriteBeyondTheEndOfChunkException
  * @return 'true' if end of chunk reached.
  */
bool Chunk::write(const char* buffer, int nbBytes)
{
   QMutexLocker locker(&this->mutex);
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

int Chunk::getNum() const
{
   return this->num;
}

int Chunk::getNbTotalChunk() const
{
   QMutexLocker locker(&this->mutex);

   if (this->file)
      return this->file->getNbChunks();

   return 0;
}

QList< QSharedPointer<Chunk> > Chunk::getOtherChunks() const
{
   QMutexLocker locker(&this->mutex);

   if (this->file)
      return this->file->getChunks();
   else
      return QList< QSharedPointer<Chunk> >();
}

bool Chunk::hasHash() const
{
   return !this->hash.isNull();
}

Common::Hash Chunk::getHash() const
{
   QMutexLocker locker(&this->mutex);
   return this->hash;
}

void Chunk::setHash(const Common::Hash& hash)
{
   L_DEBU(QString("Chunk[%1] setHash(..) : %2").arg(this->num).arg(hash.toStr()));

   if (!this->hash.isNull() && this->hash != hash)
      L_WARN(QString("Chunk::setHash : Hash chunk changed from %1 to %2 for the file %3").arg(this->hash.toStr()).arg(hash.toStr()).arg(this->file->getFullPath()));

   this->hash = hash;
}

int Chunk::getKnownBytes() const
{
   QMutexLocker locker(&this->mutex);
   return this->knownBytes;
}

void Chunk::setKnownBytes(int bytes)
{
   this->knownBytes = bytes;
}

int Chunk::getChunkSize() const
{
   if (!this->file)
      return 0;

   if (this->num < this->file->getNbChunks() - 1)
      return this->CHUNK_SIZE;

   const int size = this->file->getSize() % this->CHUNK_SIZE;
   if (!size)
      return this->CHUNK_SIZE;
   else
      return size;
}

bool Chunk::isComplete() const
{
   QMutexLocker locker(&this->mutex);
   return this->file && this->knownBytes >= this->getChunkSize(); // Should be '==' but we are never 100% sure ;).
}

bool Chunk::isOwnedBy(File* file) const
{
   return this->file == file;
}

bool Chunk::matchesEntry(const Protos::Common::Entry& entry) const
{
   QMutexLocker locker(&this->mutex);
   return this->file->matchesEntry(entry);
}
