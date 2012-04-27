#ifndef COMMON_SORTEDLIST_H
#define COMMON_SORTEDLIST_H

#include <QLinkedList>

/**
  * @class Common::SortedList
  *
  * A very simple sorted list, not very efficent, implemented as a simple linked list. A more efficient implementation should use a red-black tree or a B-tree.
  * Don't forget to call 'itemChanged(..)' if the data of one of the items has changed and the sorting function ('lesserThan') depends of this data.
  */

namespace Common
{
   template <typename T>
   class SortedList
   {
   public:
      SortedList(std::function<bool(const T&, const T&)> lesserThan = nullptr);

      void insert(const T& item);

      template <typename Container>
      void insert(const Container& items);

      void itemChanged(const T& item);
      void removeOne(const T& item);
      void clear();
      inline const QLinkedList<T>& getList() const { return this->list; }

   private:
      std::function<bool(const T&, const T&)> lesserThan;
      QLinkedList<T> list;
   };
}

using namespace Common;

/**
  * If no function 'lesserThan' is given then the operator < on T is used.
  */
template <typename T>
SortedList<T>::SortedList(std::function<bool(const T&, const T&)> lesserThan) :
   lesserThan(lesserThan)
{
}

template <typename T>
void SortedList<T>::insert(const T& item)
{
   for (QMutableLinkedListIterator<T> i(this->list); i.hasNext(); i.next())
    {
       T e = i.peekNext();
       if (e == item)
          return;
       if (this->lesserThan ? this->lesserThan(item, e) : item < e)
       {
          i.insert(item);
          return;
       }
    }

    this->list << item;
}

/**
  * The given items MUST be sorted.
  */
template <typename T>
template <typename Container>
void SortedList<T>::insert(const Container& items)
{
   typename Container::const_iterator i = items.begin();
   QMutableLinkedListIterator<T> j(list);

   while(i != items.end())
   {
      T ei = *i;

      bool inserted = false;
      while (j.hasNext())
      {
         T ej = j.peekNext();
         if (this->lesserThan ? this->lesserThan(ei, ej) : ei < ej)
         {
            j.insert(ei);
            i++;
            inserted = true;
            break;
         }
         j.next();
      }

      if (!inserted)
         j.insert(ei);
   }
}

template <typename T>
void SortedList<T>::itemChanged(const T& item)
{
   this->list.removeOne(item);
   this->insert(item);
}

template <typename T>
void SortedList<T>::removeOne(const T& item)
{
   this->list.removeOne(item);
}

template <typename T>
void SortedList<T>::clear()
{
   this->list.clear();
}

#endif
