#include <priv/ChunkIndex/Chunks.h>
using namespace FM;

#include <priv/Cache/Chunk.h>
#include <priv/Log.h>

void Chunks::add(QSharedPointer<Chunk> chunk)
{
   QMutexLocker lock(&this->mutex);
   this->insert(chunk->getHash(), chunk);
}

void Chunks::rm(QSharedPointer<Chunk> chunk)
{
   QMutexLocker lock(&this->mutex);
   this->remove(chunk->getHash());
}

const QSharedPointer<Chunk> Chunks::value(const Common::Hash& hash) const
{
   QMutexLocker lock(&this->mutex);
   return QHash< Common::Hash, QSharedPointer<Chunk> >::value(hash);
}

bool Chunks::contains(const Common::Hash& hash) const
{
   QMutexLocker lock(&this->mutex);
   return QHash< Common::Hash, QSharedPointer<Chunk> >::contains(hash);
}

/*const QSharedPointer<Chunk> operator[] (const Common::Hash& key) const
{
   return QHash::operator[](key);
}*/
