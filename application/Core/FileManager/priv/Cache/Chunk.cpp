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
#include <priv/Cache/SharedEntry.h>
#include <priv/Cache/DataReader.h>
#include <priv/Cache/DataWriter.h>

/**
  * @class FM::Chunk
  *
  * A chunk is a part of a file.
  * It's identified by a hash which can be unknown when a chunk is created and be set later by 'setHash(..)'.
  * A chunk can be read or write, when a chunk is written the 'knownBytes' member is increased.
  * Each chunk of a file has a unique number which begins at 0 and defines the order of data,
  * chunk#1 represents the data right after chunk#0 and so on.
  *
  * File access and byte counts use the owning file's shared mutex. Detachment holds the same lock.
  * Hash snapshots use a separate mutex, released before calling into the file or emitting notifications.
  */

int Chunk::CHUNK_SIZE(0);

Chunk::Chunk(File* file, int num, quint32 knownBytes) :
   fileMutex(file ? file->getChunkMutex() : QSharedPointer<QRecursiveMutex>::create()), file(file), num(num), knownBytes(knownBytes)
{
   L_DEBU(
      QString("New chunk[%1]: %2. File: %3")
         .arg(num)
         .arg(
            hash.toStrShort(),
            this->file ? this->file->getRelativePath().toString() : "<no file defined>"
         )
   );
}

Chunk::Chunk(File* file, int num, quint32 knownBytes, const Common::Hash& hash) :
   fileMutex(file ? file->getChunkMutex() : QSharedPointer<QRecursiveMutex>::create()), file(file), num(num), knownBytes(knownBytes), hash(hash)
{
   L_DEBU(
      QString("New chunk[%1]: %2. File: %3")
         .arg(num)
         .arg(
            hash.toStrShort(),
            this->file ? this->file->getAbsolutePath().toString() : "<no file defined>"
         )
   );
}

Chunk::~Chunk()
{
   // The last reference may be released under an index lock; do not acquire the file mutex here.
   L_DEBU(QString("Chunk Deleted[%1]: %2").arg(this->num).arg(this->getHash().toStrShort()));
}

QString Chunk::toStringLog() const
{
   QMutexLocker locker(this->fileMutex.data());
   return
      QString("num = [%1], hash = %2, knownBytes = %3, size = %4")
         .arg(this->num)
         .arg(this->getHash().toStrShort())
         .arg(this->getKnownBytes())
         .arg(this->getChunkSize());
}

void Chunk::removeItsIncompleteFile()
{
   QMutexLocker locker(this->fileMutex.data());
   if (this->file)
      this->file->deleteIfIncomplete();
}

bool Chunk::populateEntry(Protos::Common::Entry* entry) const
{
   QMutexLocker locker(this->fileMutex.data());
   if (this->file)
   {
      this->file->populateEntry(entry);
      return true;
   }
   return false;
}

Common::Path Chunk::getFilePath() const
{
   QMutexLocker locker(this->fileMutex.data());
   if (this->file)
      return this->file->getAbsolutePath();
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
   QMutexLocker locker(this->fileMutex.data());
   if (this->file)
      this->file->newDataWriterCreated();
}

void Chunk::newDataReaderCreated()
{
   QMutexLocker locker(this->fileMutex.data());
   if (this->file)
      this->file->newDataReaderCreated();
}

void Chunk::dataWriterDeleted()
{
   QMutexLocker locker(this->fileMutex.data());
   if (this->file)
      this->file->dataWriterDeleted();
}

void Chunk::dataReaderDeleted()
{
   QMutexLocker locker(this->fileMutex.data());
   if (this->file)
      this->file->dataReaderDeleted();
}

/**
  * Called when the owning file retires its chunks, including replacement after a disk change or re-download.
  */
void Chunk::fileDeleted()
{
   QMutexLocker locker(this->fileMutex.data());
   this->file = nullptr;
}

int Chunk::getNum() const
{
   return this->num;
}

int Chunk::getNbTotalChunk() const
{
   QMutexLocker locker(this->fileMutex.data());
   if (this->file)
      return this->file->getNbChunks();

   return 0;
}

QVector<QSharedPointer<Chunk>> Chunk::getOtherChunks() const
{
   QMutexLocker locker(this->fileMutex.data());
   if (this->file)
      return this->file->getChunks();
   else
      return QVector<QSharedPointer<Chunk>>();
}

bool Chunk::hasHash() const
{
   QMutexLocker locker(&this->hashMutex);
   return !this->hash.isNull();
}

Common::Hash Chunk::getHash() const
{
   QMutexLocker locker(&this->hashMutex);
   return this->hash;
}

void Chunk::setHash(const Common::Hash& hash)
{
   this->setHash(hash, true);
}

void Chunk::setHash(const Common::Hash& hash, bool saveHashes)
{
   QMutexLocker locker(this->fileMutex.data());
   #ifdef DEBUG
      L_DEBU(QString("Chunk[%1] setHash(..): %2").arg(this->num).arg(hash.toStrShort()));
      if (!this->hash.isNull() && this->hash != hash)
         L_WARN(
            QString("Chunk::setHash: Hash chunk changed from %1 to %2 for the file %3")
               .arg(
                  this->hash.toStr(),
                  hash.toStr(),
                  this->file ? this->file->getAbsolutePath().toString() : QString()
               )
         );
   #endif

   {
      QMutexLocker hashLocker(&this->hashMutex);
      this->hash = hash;
   }

   if (saveHashes && this->file)
      this->file->saveHashes();
}

void Chunk::saveFileHashes()
{
   QMutexLocker locker(this->fileMutex.data());
   if (this->file)
      this->file->saveHashes();
}

int Chunk::getKnownBytes() const
{
   QMutexLocker locker(this->fileMutex.data());
   return this->knownBytes;
}

void Chunk::setKnownBytes(int bytes)
{
   QMutexLocker locker(this->fileMutex.data());
   this->knownBytes = bytes;
}

int Chunk::getChunkSize() const
{
   QMutexLocker locker(this->fileMutex.data());
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
   QMutexLocker locker(this->fileMutex.data());
   return this->file && this->knownBytes >= this->getChunkSize(); // Should be '==' but we are never 100% sure ;).
}

/**
  * @return 'true' if the file owning the chunk is complete (not an unfinished file).
  */
bool Chunk::isFileComplete() const
{
   QMutexLocker locker(this->fileMutex.data());
   return this->file && this->file->isComplete();
}

bool Chunk::isOwnedBy(File* file) const
{
   QMutexLocker locker(this->fileMutex.data());
   return this->file == file;
}

bool Chunk::matchesEntry(const Protos::Common::Entry& entry) const
{
   QMutexLocker locker(this->fileMutex.data());
   if (this->file)
      return this->file->matchesEntry(entry);
   else
      return false;
}
