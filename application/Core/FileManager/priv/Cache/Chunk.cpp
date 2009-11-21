#include <priv/Cache/Chunk.h>
using namespace FM;

#include <IDataReader.h>
#include <IDataWriter.h>
#include <Exceptions.h>
#include <priv/Log.h>
#include <priv/Cache/Cache.h>
#include <priv/Cache/File.h>
#include <priv/Cache/SharedDirectory.h>
#include <priv/Cache/DataReader.h>
#include <priv/Cache/DataWriter.h>

Chunk::Chunk(Cache* cache, File* file, int num, int knownBytes)
   : cache(cache), file(file), num(num), knownBytes(knownBytes)
{
   QMutexLocker locker (&this->mutex);
   //LOG_DEBUG(QString("New chunk[%1] : %2. File : %3").arg(num).arg(hash.toStr()).arg(this->file->getFullPath()));
}

Chunk::Chunk(Cache* cache, File* file, int num, int knownBytes, const Common::Hash& hash)
   : cache(cache), file(file), num(num), knownBytes(knownBytes), hash(hash)
{
   QMutexLocker locker (&this->mutex);
   LOG_DEBUG(QString("New chunk[%1] : %2. File : %3").arg(num).arg(hash.toStr()).arg(this->file->getFullPath()));

   this->cache->onChunkHashKnown(this);
}

Chunk::~Chunk()
{
   QMutexLocker locker(&this->mutex);
   LOG_DEBUG(QString("Chunk Deleted [%1] : %2. File : %3").arg(num).
      arg(hash.toStr()).
      arg(this->file ? this->file->getFullPath() : "<file deleted>")
   );
   this->cache->onChunkRemoved(this);
   //this->file->chunkDeleted(this);
}

Chunk* Chunk::restoreFromFileCache(const Protos::FileCache::Hashes_Chunk& chunk)
{
   this->knownBytes = chunk.known_bytes();

   if (chunk.has_hash())
   {
      this->hash = chunk.hash().hash().data();
      this->cache->onChunkHashKnown(this);
   }
   return this;
}

void Chunk::populateHashesChunk(Protos::FileCache::Hashes_Chunk& chunk)
{
   chunk.set_known_bytes(this->knownBytes);
   if (!this->hash.isNull())
      chunk.mutable_hash()->set_hash(this->hash.getData(), Common::Hash::HASH_SIZE);
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
   QMutexLocker locker(&this->mutex);
   if (this->file)
      this->file->newDataReaderCreated();
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

   if (this->knownBytes != CHUNK_SIZE)
      throw ChunkNotCompletedException();

   return this->file->read(buffer, offset + this->num * CHUNK_SIZE);
}

bool Chunk::write(const QByteArray& buffer, int offset)
{
   QMutexLocker locker(&this->mutex);
   if (!this->file)
      throw ChunkDeletedException();

   if (this->knownBytes + buffer.size() > CHUNK_SIZE)
      throw 1; // TODO : create exception
   bool eof = this->file->write(buffer, offset + this->num * CHUNK_SIZE);
   this->knownBytes += buffer.size();
   return eof;
}

/*void Chunk::sendContentToSocket(QAbstractSocket& socket)
{
}

void Chunk::getContentFromSocket(QAbstractSocket& socket)
{
}*/

bool Chunk::hasHash()
{
   return !this->hash.isNull();
}

Common::Hash Chunk::getHash()
{
   return this->hash;
}

void Chunk::setHash(const Common::Hash& hash)
{
   LOG_DEBUG(QString("Chunk::setHash(..) : %1").arg(hash.toStr()));
   this->hash = hash;
}

int Chunk::getKnownBytes()
{
   return this->knownBytes;
}

/*
File& Chunk::getFile()
{
   return this->file;
}*/
