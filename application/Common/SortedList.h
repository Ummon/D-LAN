/**
  * D-LAN - A decentralized LAN file sharing software.
  * Copyright (C) 2010-2012 Greg Burri <greg.burri@gmail.com>
  *
  * This program is free software: you can redistribute it and/or modify
  * it under the terms of the GNU General Public License as published by
  * the Free Software Foundation, either version 3 of the License, or
  * (at your option) any later version.
  *
  * This program is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  *
  * You should have received a copy of the GNU General Public License
  * along with this program.  If not, see <http://www.gnu.org/licenses/>.
  */
  
#ifndef COMMON_SORTEDLIST_H
#define COMMON_SORTEDLIST_H

#include <QLinkedList>

/**
  * @class Common::SortedList
  *
  * A very simple sorted list, not very efficent, implemented as a simple linked list. A more efficient implementation should use a red-black tree or a B-tree.
  * Don't forget to call 'itemChanged(..)' if the data of one of the items has changed and the sorting function ('lesserThan') depends of this data.
  * Do not allow multiple same item.
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
   QMutableLinkedListIterator<T> j(list);

   for (typename Container::const_iterator i = items.begin(); i != items.end(); i++)
   {
      T ei = *i;

      while (j.hasNext())
      {
         T ej = j.peekNext();
         if (this->lesserThan ? this->lesserThan(ei, ej) : ei < ej)
         {
            j.insert(ei);
            goto nextEi;
         }
         j.next();
      }
      j.insert(ei);
      nextEi:;
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
