#ifndef FILEMANAGER_SIZE_INDEX_H
#define FILEMANAGER_SIZE_INDEX_H

#include <functional>

#include <QMutex>
#include <QList>

#include <Common/Containers/SortedArray.h>

#include <priv/Cache/Entry.h>

namespace FM
{
   class SizeIndexEntries
   {
   public:
      SizeIndexEntries();

      void addItem(Entry* item);
      void rmItem(Entry* item);

      QList<Entry*> search(qint64 sizeMin, qint64 sizeMax, int limit = std::numeric_limits<int>::max(), std::function<bool(const Entry*)> predicat = nullptr) const;

   private:
      Common::SortedArray<Entry*> index;
      mutable QMutex mutex;
   };
}

#endif
