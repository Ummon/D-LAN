#include <priv/ChunkIndex/Chunks.h>
using namespace FM;

#include <priv/Cache/Chunk.h>
#include <priv/Log.h>

void Chunks::add(QSharedPointer<Chunk> chunk)
{
   this->insert(chunk->getHash(), chunk);
}

void Chunks::rm(QSharedPointer<Chunk> chunk)
{
   this->remove(chunk->getHash());
}
