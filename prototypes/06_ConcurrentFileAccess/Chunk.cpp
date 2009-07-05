#include "Chunk.h"

Chunk::Chunk(File& file, int num)
   : file(file), num(num)
{
   this->complete = false;
}

bool Chunk::write(const QByteArray& buffer, uint offset)
{
   bool eof = this->file.write(buffer, (qint64)offset + (qint64)this->num * (qint64)File::chunkSize);
   if (offset + buffer.size() >= File::chunkSize)
      this->complete = true;
   return eof;
}

qint64 Chunk::read(QByteArray& buffer, uint offset) throw(ChunkNotCompletedException)
{
   if (!this->complete)
      throw ChunkNotCompletedException();
   
   return this->file.read(buffer, (qint64)offset + (qint64)this->num * (qint64)File::chunkSize);
}

bool Chunk::isComplete()
{
   return this->complete;
}
