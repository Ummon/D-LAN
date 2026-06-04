#include <priv/SizeIndex.h>
using namespace FM;

#include <QMutexLocker>
#include <QDateTime>

#include <priv/Log.h>
#include <priv/Cache/SharedEntry.h>
// #include <priv/Cache/File.h>

SizeIndex::SizeIndex()
{
   this->index.setSortedFunction([](ISizeItem* e1, ISizeItem* e2) {
      if (e1->getSize() == e2->getSize())
         return e1->hash() < e2->hash();

      return e1->getSize() < e2->getSize();
   });
}

void SizeIndex::addItem(ISizeItem* item)
{
   QMutexLocker locker(&this->mutex);
   // L_DEBU(QString("~~~~~~~~~~~~~~~~ addFile: %1 (%2)").arg(dynamic_cast<File*>(item)->getAbsolutePath()).arg(item->getSize()));
   this->index.insert(item);
}

void SizeIndex::rmItem(ISizeItem* item)
{
   QMutexLocker locker(&this->mutex);
   // L_DEBU(QString("~~~~~~~~~~~~~~~~ rmItem: %1 (%2)").arg(dynamic_cast<File*>(item)->getAbsolutePath()).arg(item->getSize()));
   this->index.remove(item);
}

QList<ISizeItem*> SizeIndex::search(
   qint64 sizeMin,
   qint64 sizeMax,
   int limit,
   std::function<bool(const ISizeItem*)> predicat
) const
{
   QMutexLocker locker(&this->mutex);

   QList<ISizeItem*> result;

   if (this->index.isEmpty())
      return result;

   FakeItem searchEntry(sizeMin);
   auto i = this->index.iteratorOfNearest(&searchEntry);

   if ((*i)->getSize() < sizeMin)
      ++i;

   auto end = this->index.end();

   int nb = 0;
   while (i != end && (*i)->getSize() <= sizeMax)
   {
      if (predicat == nullptr || predicat(*i))
      {
         result << *i;
         if (++nb >= limit)
            break;
      }
      ++i;
   }

   return result;
}