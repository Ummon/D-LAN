#ifndef FILEMANAGER_CHUNK_H
#define FILEMANAGER_CHUNK_H

#include <Common/Hash.h>

#include <IChunk.h>

namespace FileManager
{
   class Chunk : public IChunk
   {
   public:
      virtual ~Chunk();

   private:
      quint32 num;
      Common::Hash hash;
      bool complete;
   };
}
#endif
