#ifndef FILEMANAGER_CHUNKS_H
#define FILEMANAGER_CHUNKS_H

#include <QHash>

#include <Common/Hash.h>

namespace FileManager
{
   class Chunk;

   class Chunks : public QHash<Common::Hash, Chunk*>
   {
   public:
      void add(Chunk* chunk);
   };
}
#endif
