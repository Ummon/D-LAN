#ifndef COMMON_MAPARRAY_H
#define COMMON_MAPARRAY_H

#include <Common/SortedArray.h>

namespace Common
{
   template <typename K, typename T>
   class MapArray
   {
   public:
      class NotFoundException {};

      inline int size() const;
      inline int insert(const K& key, const T& value, bool* exists = nullptr);

      const T& getValueFromIndex(int index) const;
      T& getValueFromIndex(int index);

      const K& getKeyFromIndex(int index) const;

      T& operator[](const K& key);

      void remove(int index);
      void remove(const K& key);

      int indexOf(const K& key);

   private:
      struct Element
      {
         K key;
         T value;

         bool operator<(const Element& other) const
         {
            return this->key < other.key;
         }
      };

      SortedArray<Element> array;
   };
}

template <typename K, typename T>
inline int Common::MapArray<K, T>::size() const
{
   return this->array.size();
}

template <typename K, typename T>
inline int Common::MapArray<K, T>::insert(const K& key, const T& value, bool* exists)
{
   return this->array.insert(Element {key, value}, exists);
}

/**
  * @exception NotFoundException
  */
template <typename K, typename T>
const T& Common::MapArray<K, T>::getValueFromIndex(int index) const
{
   try
   {
      return this->array[index].value;
   }
   catch (typename SortedArray<Element>::NotFoundException&)
   {
      throw NotFoundException();
   }
}

/**
  * @exception NotFoundException
  */
template <typename K, typename T>
T& Common::MapArray<K, T>::getValueFromIndex(int index)
{
   try
   {
      return this->array[index].value;
   }
   catch (typename SortedArray<Element>::NotFoundException&)
   {
      throw NotFoundException();
   }
}

/**
  * @exception NotFoundException
  */
template <typename K, typename T>
const K& Common::MapArray<K, T>::getKeyFromIndex(int index) const
{
   try
   {
      return this->array[index].key;
   }
   catch (typename SortedArray<Element>::NotFoundException&)
   {
      throw NotFoundException();
   }
}

template <typename K, typename T>
T& Common::MapArray<K, T>::operator[](const K& key)
{
   return this->array[Element {key, T()}].value;
}

template <typename K, typename T>
int Common::MapArray<K, T>::indexOf(const K& key)
{
   return this->array.indexOf(Element {key, T()});
}

#endif
