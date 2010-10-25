#ifndef FILEMANAGER_CHUNKS_H
#define FILEMANAGER_CHUNKS_H

#include <QHash>
#include <QSharedPointer>
#include <QMutex>

#include <Common/Hash.h>

namespace FM
{
   class Chunk;

   class Chunks : private QHash< Common::Hash, QSharedPointer<Chunk> >
   {
   public:
      void add(QSharedPointer<Chunk> chunk);
      void rm(QSharedPointer<Chunk> chunk);
      const QSharedPointer<Chunk> value(const Common::Hash& hash) const;
      bool contains(const Common::Hash& hash) const;
      //const QSharedPointer<Chunk> operator[] (const Common::Hash& key) const;

   private:
      mutable QMutex mutex; // From the documentation : "they (containers) are thread-safe in situations where they are used as read-only containers by all threads used to access them.".
   };
}
#endif
