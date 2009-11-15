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

Chunk::Chunk(Cache* cache, File* file, const Common::Hash& hash, int num, int knownBytes)
   : cache(cache), file(file), hash(hash), num(num), knownBytes(knownBytes)
{
   QMutexLocker(&this->mutex);
   LOG_DEBUG(QString("New chunk[%1] : %2. File : %3").arg(num).arg(hash.toStr()).arg(this->file->getFullPath()));

   this->cache->onChunkAdded(this);
}

Chunk::Chunk(Cache* cache, File* file, int num, const Protos::FileCache::Hashes_Chunk& chunk)
   : cache(cache), file(file), hash(chunk.hash().hash().data()), num(num), knownBytes(chunk.known_bytes())
{
   QMutexLocker(&this->mutex);
   LOG_DEBUG(QString("New chunk [%1] : %2. File : %3").arg(num).arg(hash.toStr()).arg(this->file->getFullPath()));

   this->cache->onChunkAdded(this);
}

Chunk::~Chunk()
{
   QMutexLocker(&this->mutex);
   LOG_DEBUG(QString("Chunk Deleted [%1] : %2. File : %3").arg(num).
      arg(hash.toStr()).
      arg(this->file ? this->file->getFullPath() : "<file deleted>")
   );
   this->cache->onChunkRemoved(this);
   //this->file->chunkDeleted(this);
}

void Chunk::populateHashesChunk(Protos::FileCache::Hashes_Chunk& chunk)
{
   chunk.set_known_bytes(this->knownBytes);
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
   QMutexLocker(&this->mutex);
   if (this->file)
      this->file->newDataReaderCreated();
}

void Chunk::newDataReaderCreated()
{
   QMutexLocker(&this->mutex);
   if (this->file)
      this->file->newDataReaderCreated();
}

void Chunk::dataWriterDeleted()
{
   QMutexLocker(&this->mutex);
   if (this->file)
      this->file->dataWriterDeleted();
}

void Chunk::dataReaderDeleted()
{
   QMutexLocker(&this->mutex);
   if (this->file)
      this->file->dataWriterDeleted();
}

void Chunk::fileDeleted()
{
   QMutexLocker(&this->mutex);
   this->file = 0;
}

int Chunk::read(QByteArray& buffer, int offset)
{
   QMutexLocker(&this->mutex);
   if (!this->file)
      throw ChunkDeletedException();

   if (this->knownBytes != CHUNK_SIZE)
      throw ChunkNotCompletedException();

   return this->file->read(buffer, offset + this->num * CHUNK_SIZE);
}

bool Chunk::write(const QByteArray& buffer, int offset)
{
   QMutexLocker(&this->mutex);
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

Common::Hash Chunk::getHash()
{
   return this->hash;
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
