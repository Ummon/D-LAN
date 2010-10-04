#include <priv/Cache/Chunk.h>
using namespace FM;

#include <IDataReader.h>
#include <IDataWriter.h>
#include <Exceptions.h>
#include <priv/Log.h>
#include <priv/Cache/File.h>
#include <priv/Cache/SharedDirectory.h>
#include <priv/Cache/DataReader.h>
#include <priv/Cache/DataWriter.h>

Chunk::Chunk(File* file, int num, int knownBytes)
   : mutex(QMutex::Recursive), file(file), num(num), knownBytes(knownBytes)
{
   QMutexLocker locker(&this->mutex);
   L_DEBU(QString("New chunk[%1] : %2. File : %3").arg(num).arg(hash.toStr()).arg(this->file->getFullPath()));
}

Chunk::Chunk(File* file, int num, int knownBytes, const Common::Hash& hash)
   : mutex(QMutex::Recursive), file(file), num(num), knownBytes(knownBytes), hash(hash)
{
   QMutexLocker locker(&this->mutex);
   L_DEBU(QString("New chunk[%1] : %2. File : %3").arg(num).arg(hash.toStr()).arg(this->file->getFullPath()));
}

Chunk::~Chunk()
{
   QMutexLocker locker(&this->mutex);
   L_DEBU(QString("Chunk Deleted[%1] : %2. File : %3").arg(num).
      arg(hash.toStr()).
      arg(this->file ? this->file->getFullPath() : "<file deleted>")
   );
}

Chunk* Chunk::restoreFromFileCache(const Protos::FileCache::Hashes_Chunk& chunk)
{
   this->knownBytes = chunk.known_bytes();

   if (chunk.has_hash())
      this->hash = chunk.hash().hash().data();
   return this;
}

void Chunk::populateHashesChunk(Protos::FileCache::Hashes_Chunk& chunk) const
{
   chunk.set_known_bytes(this->knownBytes);
   if (!this->hash.isNull())
      chunk.mutable_hash()->set_hash(this->hash.getData(), Common::Hash::HASH_SIZE);
}

void Chunk::populateEntry(Protos::Common::Entry* entry) const
{
   QMutexLocker locker(&this->mutex);
   if (this->file)
      this->file->populateEntry(entry);
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
      this->file->dataWriterDeleted();
}

void Chunk::fileDeleted()
{
   QMutexLocker locker(&this->mutex);
   this->file = 0;
}

int Chunk::read(QByteArray& buffer, int offset)
{
   QMutexLocker locker(&this->mutex);
   if (!this->file)
      throw ChunkDeletedException();

   if (this->knownBytes == 0)
      throw ChunkNotCompletedException();

   if (offset >= this->knownBytes)
      return 0;

   return this->file->read(buffer, offset + this->num * CHUNK_SIZE);
}

/**
  * Write the given buffer after 'knownBytes'.
  * @exception IOErrorException
  * @exception ChunkDeletedException
  * @exception TryToWriteBeyondTheEndOfChunkException
  */
bool Chunk::write(const QByteArray& buffer)
{
   QMutexLocker locker(&this->mutex);
   if (!this->file)
      throw ChunkDeletedException();

   if (this->knownBytes + buffer.size() > CHUNK_SIZE)
      throw TryToWriteBeyondTheEndOfChunkException();

   this->knownBytes += this->file->write(buffer, this->knownBytes + this->num * CHUNK_SIZE);

   if (this->knownBytes > CHUNK_SIZE) // Should never be true.
   {
      L_ERRO("Chunk::write : this->knownBytes > CHUNK_SIZE");
      this->knownBytes = CHUNK_SIZE;
   }

   return this->knownBytes == CHUNK_SIZE;
}

/*void Chunk::sendContentToSocket(QAbstractSocket& socket)
{
}

void Chunk::getContentFromSocket(QAbstractSocket& socket)
{
}*/

int Chunk::getNum() const
{
   return this->num;
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
   L_DEBU(QString("Chunk[%1] setHash(..) : %2").arg(this->num).arg(hash.toStr()));

   if (!this->hash.isNull())
      L_WARN(QString("Chunk::setHash : Chunk has already an hash! file : %1").arg(this->file->getFullPath()));

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

bool Chunk::isOwnedBy(File* file) const
{
   return this->file == file;
}
