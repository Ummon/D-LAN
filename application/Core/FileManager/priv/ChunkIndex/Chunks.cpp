#include <priv/ChunkIndex/Chunks.h>
using namespace FileManager;

#include <priv/Cache/Chunk.h>

void Chunks::add(Chunk* chunk)
{
   this->insert(chunk->getHash(), chunk);
}
