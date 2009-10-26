#include <priv/Cache/Chunk.h>
using namespace FM;

#include <IDataReader.h>
#include <IDataWriter.h>
#include <priv/Log.h>
#include <priv/Cache/Cache.h>
#include <priv/Cache/SharedDirectory.h>
#include <priv/Cache/DataReader.h>
#include <priv/Cache/DataWriter.h>

Chunk::Chunk(File& file, const Common::Hash& hash, int num, int knownBytes)
   : file(file), hash(hash), num(num), knownBytes(knownBytes)
{
   LOG_DEBUG(QString("New chunk[%1] : %2. File : %3").arg(num).arg(hash.toStr()).arg(file.getFullPath()));

   static_cast<SharedDirectory*>(this->file.getRoot())->getCache()->onChunkAdded(this);
}

QSharedPointer<IDataReader> Chunk::getDataReader()
{
   return QSharedPointer<IDataReader>(new DataReader(*this));
}

QSharedPointer<IDataWriter> Chunk::getDataWriter()
{
   return QSharedPointer<IDataWriter>(new DataWriter(*this));
}

int Chunk::read(QByteArray& buffer, int offset)
{
   if (this->knownBytes != File::CHUNK_SIZE)
      throw ChunkNotCompletedException();

   return this->file.read(buffer, offset + this->num * File::CHUNK_SIZE);
}

bool Chunk::write(const QByteArray& buffer, int offset)
{
   if (this->knownBytes + buffer.size() > File::CHUNK_SIZE)
      throw 1; // TODO : create exception
   bool eof = this->file.write(buffer, offset + this->num * File::CHUNK_SIZE);
   this->knownBytes += buffer.size();
   return eof;
}

void Chunk::sendContentToSocket(QAbstractSocket& socket)
{
}

void Chunk::getContentFromSocket(QAbstractSocket& socket)
{
}

Common::Hash Chunk::getHash()
{
   return this->hash;
}

int Chunk::getKnownBytes()
{
   return this->knownBytes;
}

File& Chunk::getFile()
{
   return this->file;
}
