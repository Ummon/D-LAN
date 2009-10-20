#include <priv/ChunkIndex/Chunks.h>
using namespace FM;

#include <priv/Cache/Chunk.h>

void Chunks::add(Chunk* chunk)
{
   this->insert(chunk->getHash(), chunk);
}
