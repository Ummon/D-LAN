#ifndef FILEMANAGER_EXTENSIONINDEX_H
#define FILEMANAGER_EXTENSIONINDEX_H

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

      QList<T> search(const QString& extension) const;

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
   QMutexLocker locker(&this->mutex);
   this->index.insert(extension.toLower(), item);
}

template<typename T>
void FM::ExtensionIndex<T>::rmItem(const QString& extension, const T& item)
{
   QMutexLocker locker(&this->mutex);
   this->index.remove(extension.toLower(), item);
}

template<typename T>
void FM::ExtensionIndex<T>::changeItem(const QString& oldExtension, const QString& newExtension, const T& item)
{
   QMutexLocker locker(&this->mutex);
   this->index.remove(oldExtension.toLower(), item);
   this->index.insert(newExtension.toLower(), item);
}

template<typename T>
QList<T> FM::ExtensionIndex<T>::search(const QString& extension) const
{
   QMutexLocker locker(&this->mutex);
   QList<T> result;
   for (typename QHash<QString, T>::const_iterator i = this->index.find(extension.toLower()); i != this->index.constEnd(); i++)
      result << i.key();
   return result;
}

#endif
