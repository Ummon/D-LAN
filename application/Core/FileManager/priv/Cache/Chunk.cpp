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

#include <Common/ProtoHelper.h>

#include <IDataReader.h>
#include <IDataWriter.h>
#include <priv/Global.h>
#include <priv/Cache/SharedDirectory.h>
#include <priv/Cache/DataReader.h>
#include <priv/Cache/DataWriter.h>

/**
  * @class FM::Chunk
  *
  * A chunk is a part of a file. It's identified by a hash which can be unknown when a chunk is created and be set later by 'setHash(..)'.
  * A chunk can be read or write, when a chunk is written the 'knownBytes' member is increased.
  * Each chunk of a file has a unique number which begins at 0 and define the order of data, chunk#1 represents the data right after chunk#0 and so on.
  *
  * Concurrent accesses are protected by the 'QSharedPointer', see the 'File' class.
  */

int Chunk::CHUNK_SIZE(0);

Chunk::Chunk(File* file, int num, quint32 knownBytes) :
   file(file), num(num), knownBytes(knownBytes)
{
   L_DEBU(QString("New chunk[%1] : %2. File : %3").arg(num).arg(hash.toStr()).arg(this->file ? this->file->getFullPath() : "<no file defined>"));
}

Chunk::Chunk(File* file, int num, quint32 knownBytes, const Common::Hash& hash) :
   file(file), num(num), knownBytes(knownBytes), hash(hash)
{
   L_DEBU(QString("New chunk[%1] : %2. File : %3").arg(num).arg(hash.toStr()).arg(this->file ? this->file->getFullPath() : "<no file defined>"));
}

Chunk::~Chunk()
{
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
   if (this->file)
      this->file->deleteIfIncomplete();
}

bool Chunk::populateEntry(Protos::Common::Entry* entry) const
{
   if (this->file)
   {
      this->file->populateEntry(entry);
      return true;
   }
   return false;
}

QString Chunk::getFilePath() const
{
   if (this->file)
      return this->file->getFullPath();
   return QString();
}

QSharedPointer<IDataReader> Chunk::getDataReader()
{
   return QSharedPointer<IDataReader>(new DataReader(*this));
}

QSharedPointer<IDataWriter> Chunk::getDataWriter()
{
   return QSharedPointer<IDataWriter>(new DataWriter(*this));
}

void Chunk::newDataWriterCreated()
{
   if (this->file)
      this->file->newDataWriterCreated();
}

void Chunk::newDataReaderCreated()
{
   if (this->file)
      this->file->newDataReaderCreated();
}

void Chunk::dataWriterDeleted()
{
   if (this->file)
      this->file->dataWriterDeleted();
}

void Chunk::dataReaderDeleted()
{
   if (this->file)
      this->file->dataReaderDeleted();
}

/**
  * Called by a deleted file just before dying.
  */
void Chunk::fileDeleted()
{
   this->file = 0;
}


int Chunk::getNum() const
{
   return this->num;
}

int Chunk::getNbTotalChunk() const
{
   if (this->file)
      return this->file->getNbChunks();

   return 0;
}

QVector<QSharedPointer<Chunk>> Chunk::getOtherChunks() const
{
   if (this->file)
      return this->file->getChunks();
   else
      return QVector<QSharedPointer<Chunk>>();
}

bool Chunk::hasHash() const
{
   return !this->hash.isNull();
}

Common::Hash Chunk::getHash() const
{
   return this->hash;
}

void Chunk::setHash(const Common::Hash& hash)
{
   #ifdef DEBUG
      L_DEBU(QString("Chunk[%1] setHash(..) : %2").arg(this->num).arg(hash.toStr()));
      if (!this->hash.isNull() && this->hash != hash)
         L_WARN(QString("Chunk::setHash : Hash chunk changed from %1 to %2 for the file %3").arg(this->hash.toStr()).arg(hash.toStr()).arg(this->file->getFullPath()));
   #endif

   this->hash = hash;
}

int Chunk::getKnownBytes() const
{
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
      return Chunk::CHUNK_SIZE;

   const int size = this->file->getSize() % Chunk::CHUNK_SIZE;
   if (!size)
      return Chunk::CHUNK_SIZE;
   else
      return size;
}

bool Chunk::isComplete() const
{
   return this->file && this->knownBytes >= this->getChunkSize(); // Should be '==' but we are never 100% sure ;).
}

bool Chunk::isOwnedBy(File* file) const
{
   return this->file == file;
}

bool Chunk::matchesEntry(const Protos::Common::Entry& entry) const
{
   return this->file->matchesEntry(entry);
}
