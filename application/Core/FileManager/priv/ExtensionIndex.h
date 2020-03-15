#pragma once

#include <functional>
#include <limits>

#include <QHash>
#include <QSet>
#include <QList>
#include <QString>
#include <QMutex>

namespace FM
{
   template<typename T>
   class ExtensionIndex
   {
   public:
      ExtensionIndex();

      void addItem(const QString& extension, const T& item);
      void rmItem(const QString& extension, const T& item);
      void changeItem(const QString& oldExtension, const QString& newExtension, const T& item);

      QList<T> search(const QString& extension, int limit = std::numeric_limits<int>::max(), std::function<bool(const T&)> predicat = nullptr) const;
      QList<T> search(const QList<QString>& extensions, int limit = std::numeric_limits<int>::max(), std::function<bool(const T&)> predicat = nullptr) const;

   private:
      QHash<QString, QSet<T>> index;
      mutable QMutex mutex;
   };
}

template<typename T>
FM::ExtensionIndex<T>::ExtensionIndex() :
   mutex(QMutex::Recursive)
{
}

template<typename T>
void FM::ExtensionIndex<T>::addItem(const QString& extension, const T& item)
{
   if (extension.isEmpty())
      return;

   QMutexLocker locker(&this->mutex);
   QSet<T>& set = this->index[extension.toLower()];
   set.insert(item);
}

template<typename T>
void FM::ExtensionIndex<T>::rmItem(const QString& extension, const T& item)
{
   if (extension.isEmpty())
      return;

   QMutexLocker locker(&this->mutex);

   auto setIterator = this->index.find(extension);
   if (setIterator != this->index.end())
   {
      setIterator->remove(item);
      if (setIterator->empty())
         this->index.erase(setIterator);
   }
}

template<typename T>
void FM::ExtensionIndex<T>::changeItem(const QString& oldExtension, const QString& newExtension, const T& item)
{
   QMutexLocker locker(&this->mutex);

   this->rmItem(oldExtension, item);
   this->addItem(newExtension, item);
}

template<typename T>
QList<T> FM::ExtensionIndex<T>::search(const QString& extension, int limit, std::function<bool(const T&)> predicat) const
{
   return this->search(QList<QString> { extension }, limit, predicat);
}

template<typename T>
QList<T> FM::ExtensionIndex<T>::search(const QList<QString>& extensions, int limit, std::function<bool(const T&)> predicat) const
{
   QMutexLocker locker(&this->mutex);

   QList<T> result;

   for (QListIterator<QString> i(extensions); i.hasNext();)
   {
      const QString& extension = i.next();

      auto setIterator = this->index.find(extension.toLower());
      if (setIterator != this->index.constEnd())
      {
         // Special case to speedup the process.
         if (extensions.count() == 1 && !predicat && setIterator->count() <= limit)
            return setIterator->values();

         for (auto j = setIterator->constBegin(); j != setIterator->constEnd(); ++j)
         {
            if (!predicat || predicat(*j))
               result << *j;

            if (result.size() >= limit)
               goto end;
         }
      }
   }

   end:
   return result;
}
