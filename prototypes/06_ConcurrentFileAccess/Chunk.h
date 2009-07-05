#ifndef CHUNK_H
#define CHUNK_H

#include <exception>

#include <QByteArray>

#include <File.h>

class ChunkNotCompletedException : public std::exception {};

/**
  * A part of a file. Can be read or written.
  * 'read' and 'write' are thread safe.
  * A chunk cannot be read is not entirely written.
  */
class Chunk
{
public:
   /**
     * @param file The file which owns this chunk.
     * @param num The number of the chunk, 0 is the first. Used to compute the offset.
     */
   Chunk(File& file, int num);
    
   /**
     * @return 'true' if end of file reached.
     */
   bool write(const QByteArray& buffer, uint offset);
   
   /**
     * Fill the given buffer with read bytes.
     *
     * @param buffer The buffer.
     * @param offset The offset relative to the chunk.
     * @return The number of read bytes. If lesser than 'buffer.size' the end of file has been reached
     *         and the buffer will be partially filled.     
     */
   qint64 read(QByteArray& buffer, uint offset) throw(ChunkNotCompletedException);
   
   /**
     * @return 'true' if the chunk has been entirely written.
     */
   bool isComplete();
   
   /**
     * Return the number of the chunk.
     */
   int getNum() { return this->num; }
    
private:
   bool complete;
   File& file;
   int num;
};

#endif // CHUNK_H
