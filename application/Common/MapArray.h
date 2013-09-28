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

      inline const T& getValueFromIndex(int index) const;
      inline T& getValueFromIndex(int index);
      inline const K& getKeyFromIndex(int index) const;

      inline T& operator[](const K& key);

      inline void removeFromIndex(int index);
      inline bool remove(const K& key);

      inline int indexOf(const K& key);

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
inline const T& Common::MapArray<K, T>::getValueFromIndex(int index) const
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
inline T& Common::MapArray<K, T>::getValueFromIndex(int index)
{
   try
   {
      return this->array.getFromIndex(index).value;
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
inline const K& Common::MapArray<K, T>::getKeyFromIndex(int index) const
{
   try
   {
      return this->array.getFromIndex(index).key;
   }
   catch (typename SortedArray<Element>::NotFoundException&)
   {
      throw NotFoundException();
   }
}

template <typename K, typename T>
inline T& Common::MapArray<K, T>::operator[](const K& key)
{
   return this->array[Element {key, T()}].value;
}

/**
  * @exception NotFoundException
  */
template <typename K, typename T>
inline void Common::MapArray<K, T>::removeFromIndex(int index)
{
   try
   {
      this->array.remove(index);
   }
   catch(typename SortedArray<Element>::NotFoundException&)
   {
      throw NotFoundException();
   }
}

template <typename K, typename T>
inline bool Common::MapArray<K, T>::remove(const K& key)
{
   return this->array.remove(Element {key, T()});
}

template <typename K, typename T>
inline int Common::MapArray<K, T>::indexOf(const K& key)
{
   return this->array.indexOf(Element {key, T()});
}

#endif
