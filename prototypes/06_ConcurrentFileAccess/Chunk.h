#ifndef CHUNK_H
#define CHUNK_H

#include <exception>

#include <QByteArray>

#include <File.h>

class ChunkNotCompletedException : public std::exception {};

class Chunk
{
public:
   Chunk(File& file, int num);

   bool write(const QByteArray& buffer, uint offset);
   qint64 read(QByteArray& buffer, uint offset) throw(ChunkNotCompletedException);
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

#endif
