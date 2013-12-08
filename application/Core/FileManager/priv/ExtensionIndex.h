#ifndef FILEMANAGER_EXTENSION_INDEX_H
#define FILEMANAGER_EXTENSION_INDEX_H

#include <functional>
#include <limits>

#include <QMultiHash>
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
      QMultiHash<QString, T> index;
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
   this->index.insert(extension.toLower(), item);
}

template<typename T>
void FM::ExtensionIndex<T>::rmItem(const QString& extension, const T& item)
{
   if (extension.isEmpty())
      return;

   QMutexLocker locker(&this->mutex);
   this->index.remove(extension.toLower(), item);
}

template<typename T>
void FM::ExtensionIndex<T>::changeItem(const QString& oldExtension, const QString& newExtension, const T& item)
{
   QMutexLocker locker(&this->mutex);

   if (!oldExtension.isEmpty())
      this->index.remove(oldExtension.toLower(), item);

   if (!newExtension.isEmpty())
      this->index.insert(newExtension.toLower(), item);
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
      for (typename QHash<QString, T>::const_iterator j = this->index.find(extension.toLower()); j != this->index.constEnd() && j.key() == extension; ++j)
      {
         if (!predicat || predicat(j.value()))
            result << j.value();

         if (result.size() >= limit)
            goto end;
      }
   }

   end:
   return result;
}

#endif
