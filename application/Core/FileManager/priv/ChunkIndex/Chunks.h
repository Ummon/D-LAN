#ifndef FILEMANAGER_CHUNKS_H
#define FILEMANAGER_CHUNKS_H

#include <QHash>

#include <Common/Hash.h>

namespace FM
{
   class Chunk;

   class Chunks : public QHash<Common::Hash, Chunk*>
   {
   public:
      void add(Chunk* chunk);
      void rm(Chunk* chunk);
   };
}
#endif
