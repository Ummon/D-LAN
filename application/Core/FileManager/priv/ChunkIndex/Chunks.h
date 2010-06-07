#ifndef FILEMANAGER_CHUNKS_H
#define FILEMANAGER_CHUNKS_H

#include <QHash>
#include <QSharedPointer>

#include <Common/Hash.h>

namespace FM
{
   class Chunk;

   class Chunks : public QHash< Common::Hash, QSharedPointer<Chunk> >
   {
   public:
      void add(QSharedPointer<Chunk> chunk);
      void rm(QSharedPointer<Chunk> chunk);
   };
}
#endif
