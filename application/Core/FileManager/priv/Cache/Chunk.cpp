#include <priv/Cache/Chunk.h>
using namespace FM;

#include <IDataReader.h>
#include <IDataWriter.h>
#include <priv/FileManager.h>
#include <priv/Cache/DataReader.h>
#include <priv/Cache/DataWriter.h>

Chunk::Chunk(File& file, const Common::Hash& hash, int num)
   : file(file), hash(hash), num(num), complete(false)
{
   LOG_DEBUG(QString("New chunk[%1] : %2. File : %3").arg(num).arg(hash.toStr()).arg(file.getFullPath()));
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
   if (!this->complete)
      throw ChunkNotCompletedException();

   return this->file.read(buffer, offset + this->num * File::CHUNK_SIZE);
}

bool Chunk::write(const QByteArray& buffer, int offset)
{
   bool eof = this->file.write(buffer, offset + this->num * File::CHUNK_SIZE);
   if (offset + buffer.size() >= File::CHUNK_SIZE)
      this->complete = true;
   return eof;
}

Common::Hash Chunk::getHash()
{
   return this->hash;
}

File& Chunk::getFile()
{
   return this->file;
}
