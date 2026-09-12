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

#pragma once

#include <algorithm>
#include <functional>

#include <QList>
#include <QMutableListIterator>

/**
  * @class Common::SortedList
  *
  * An array-backed sorted list. Single insertion uses logarithmic comparisons,
  * plus a scan of equivalent items for duplicates; moving elements is linear.
  * Don't forget to call 'itemChanged(..)' if the data of one of the items has
  * changed and the sorting function ('lesserThan') depends of this data.
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

      inline const QList<T>& getList() const { return this->list; }

   private:
      std::function<bool(const T&, const T&)> lesserThan;
      QList<T> list;
   };
}

/**
  * If no function 'lesserThan' is given then the operator < on T is used.
  */
template <typename T>
Common::SortedList<T>::SortedList(std::function<bool(const T&, const T&)> lesserThan) :
   lesserThan(lesserThan)
{
}

template <typename T>
void Common::SortedList<T>::insert(const T& item)
{
   const auto less = [this](const T& a, const T& b)
   {
      return this->lesserThan ? this->lesserThan(a, b) : a < b;
   };

   // Directory scans commonly supply names in order. Avoid searching or
   // detaching the list just to discover that the new item belongs at the end.
   if (this->list.isEmpty() || less(this->list.constLast(), item))
   {
      this->list.append(item);
      return;
   }

   auto position = std::lower_bound(this->list.cbegin(), this->list.cend(), item, less);
   // Equivalent sort keys need not identify the same item (e.g. distinct files
   // with case-insensitively equal names). Keep their insertion order and reject
   // only operator== duplicates, as before.
   while (position != this->list.cend() && !less(item, *position))
   {
      if (*position == item)
         return;
      ++position;
   }

   const auto index = position - this->list.cbegin();
   this->list.insert(index, item);
}

/**
  * The given items MUST be sorted.
  */
template <typename T>
template <typename Container>
void Common::SortedList<T>::insert(const Container& items)
{   
   QMutableListIterator<T> j(this->list);

   for (typename Container::const_iterator i = items.begin(); i != items.end(); i++)
   {
      const T& ei = *i;
      bool alreadyExists = false;

      while (j.hasNext())
      {
         const T& ej = j.peekNext();

         if (ej == ei) // Do not allow multiple same item, like the single item 'insert(..)'.
         {
            alreadyExists = true;
            break;
         }

         if (this->lesserThan ? this->lesserThan(ei, ej) : ei < ej)
            break;

         j.next();
      }

      if (!alreadyExists)
         j.insert(ei);
   }
}

template <typename T>
void Common::SortedList<T>::itemChanged(const T& item)
{
   this->list.removeOne(item);
   this->insert(item);
}

template <typename T>
void Common::SortedList<T>::removeOne(const T& item)
{
   this->list.removeOne(item);
}

template <typename T>
void Common::SortedList<T>::clear()
{
   this->list.clear();
}
