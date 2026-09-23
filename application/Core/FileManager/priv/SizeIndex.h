#pragma once

#include <functional>

#include <QMutex>
#include <QList>

#include <Common/Containers/SortedArray.h>

namespace FM
{
   class ISizeItem
   {
   public:
      virtual qint64 getSize() const = 0;
      // Orders items of the same size. It must be unique among the indexed items and greater than 0.
      virtual quintptr uniqueKey() const = 0;
   };

   class SizeIndex
   {
   public:
      SizeIndex();

      void addItem(ISizeItem* item);
      void rmItem(ISizeItem* item);

      QList<ISizeItem*> search(
         qint64 sizeMin,
         qint64 sizeMax,
         int limit = std::numeric_limits<int>::max(),
         std::function<bool(const ISizeItem*)> predicat = nullptr
      ) const;

   private:
      class FakeItem : public ISizeItem
      {
      public:
         FakeItem(qint64 size) : size(size) {}
         ~FakeItem() {}

         qint64 getSize() const override { return this->size; }
         quintptr uniqueKey() const override { return 0; } // Before all indexed items of the same size.

      private:
         qint64 size;
      };

      Common::SortedArray<ISizeItem*> index;
      mutable QMutex mutex;
   };
}