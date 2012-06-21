#ifndef COMMON_SORTEDARRAY_H
#define COMMON_SORTEDARRAY_H

#include <QList>

/**
  * @class Common::SortedArray
  *
  */

namespace Common
{
   template<typename T>
   class SortedArray
   {
   public:
      SortedArray(int nbReserved = 0);

      int size() const;
      int insert(T value, bool* exists = nullptr);

      const T& operator[](int index) const;
      T& operator[](int index);
/*
      void remove(int index);

      int indexOf(const K& key);*/

   private:
      int getPosition(const T& value);
//      struct Item
//      {
//         K key;
//      };
      QList<T> items;
   };
}

template<typename T>
Common::SortedArray<T>::SortedArray(int nbReserved)
{
   if (nbReserved != 0)
      this->items.reserve(nbReserved);
}

template<typename T>
int Common::SortedArray<T>::size() const
{
   return this->items.size();
}

/**
  * Returns the position of the inserted element.
  * If 'exists' is given it will be set to 'true' if an existing element has been replaced by the new one.
  */
template<typename T>
int Common::SortedArray<T>::insert(T value, bool* exists)
{
   return 0;
}

template<typename T>
const T& Common::SortedArray<T>::operator[](int index) const
{
   return this->items[index];
}

template<typename T>
T& Common::SortedArray<T>::operator[](int index)
{
   return this->items[index];
}

/**
  * Return the position of the value, if it doesn't exist returns the position where to insert value.
  */
template<typename T>
int Common::SortedArray<T>::getPosition(const T& value)
{
   //if (this->items.size() <= 1)

   int position = this->items.size() / 2;

   return position;
}


#endif
