#include "Chunk.h"

/**
  * @class Chunk
  * A part of a file. Can be read or written.
  * 'read' and 'write' are thread safe.
  * A chunk cannot be read is not entirely written.
  */

Chunk::Chunk(File& file, int num)
   : file(file), num(num)
{
   this->complete = false;
}

/**
  * Return 'true' if end of file reached.
  */
bool Chunk::write(const QByteArray& buffer, uint offset)
{
   bool eof = this->file.write(buffer, (qint64)offset + (qint64)this->num * (qint64)File::chunkSize);
   if (offset + buffer.size() >= File::chunkSize)
      this->complete = true;
   return eof;
}

/**
  * Fill the given buffer with read bytes.
  */
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
