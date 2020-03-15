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
  
#ifndef COMMON_SORTARRAY_H
#define COMMON_SORTARRAY_H

#include <functional>

#include <QSharedDataPointer>
#include <QList>

/**
  * @class Common::SortedArray
  *
  * The goal of the class is to be able to use an ordered list as an array, here are listed some properties:
  *  - Access to element by integer index like an array: 0, 1, 2, . . .
  *  - Found the index of a given value.
  *  - The elements are kept ordered when a new one is inserted.
  * The type T must have the operator '<' defined. Otherwise a "lesser than" function can be given with the method 'setSortedFunction'.
  *
  * 'SortedArray' is implemented as a B-Tree, see here for more information: http://en.wikipedia.org/wiki/B-tree
  * M is the order according Knuth's definition. This is the maximum number of children a node can have.
  *
  * The height of the tree can be computed with this formula:
  *  h = log_M(n+1) - 1
  * where
  *  h: the height
  *  n: the number of distinct elements
  *  M: the order
  * For example a 5 order tree with a height of 4 can contain a maximum of 3124 elements.
  */

namespace Common
{
   template<typename T, int M = 7>
   class SortedArray
   {
      struct Node;
      struct Position
      {
         bool operator==(const Position& other) const { return other.node == this->node && other.p == this->p; }
         bool operator!=(const Position& other) const { return !(*this == other); }
         Node* node; // Should never be 'nullptr'.
         int p;
      };

   public:      
      class NotFoundException {};
      class InvalidMException {};

      class iterator
      {
         iterator(const SortedArray& array, const Position& position);

      public:
         iterator(const iterator& other);

         bool operator==(const iterator& other) const;
         bool operator!=(const iterator& other) const;
         const T& operator*() const;
         T* operator->() const;
         iterator& operator++();

         friend class SortedArray;

      private:
         const SortedArray& array;
         Position currentPosition;
      };

      SortedArray();
      SortedArray(const SortedArray& other);

      int size() const;
      int insert(const T& value, bool* exists = nullptr);
      void removeFromIndex(int index);
      bool remove(const T& value);
      void clear();

      inline bool isEmpty() const;

      bool contains(const T& value) const;

      int indexOf(const T& value) const;
      int indexOfNearest(const T& value) const;

      iterator begin() const;
      iterator end() const;
      iterator iteratorOf(const T& value) const;
      iterator iteratorOfNearest(const T& value) const;

      const T& getFromIndex(int index) const;
      T& getFromIndex(int index);

      const T& getFromValue(const T& value) const;
      T& getFromValue(const T& value);

      T& operator[](const T& value);

      QList<T> toList() const;

      int getM() const;

      //void sort();
      void setSortedFunction(const std::function<bool(const T&, const T&)>& lesserThan);

   private:
      struct Node
      {
         Node(Node* parent = nullptr) : nbItems(0), parent(parent), children{}, size(0) {}

         int nbItems;
         T items[M-1];

         Node* parent; // The root has no parent (nullptr).
         Node* children[M];

         int size; // The size of the subtree (children's sizes + this->nbItems).
      };

      T& get(int index) const;
      T& getValue(const T& value);

      static Node* getLeftmostNode(Node* node);
      static Node* getRightmostNode(Node* node);

      static Position getNextPosition(const Position&);
      static Position getPreviousPosition(const Position&);
      static Node* getFromIndex(Node* node, int index, int nbItemsBefore, int& position);

      static int indexOf(Node* node, const T& value, int nbItemsBefore, const std::function<bool(const T&, const T&)>& lesserThan);
      static int indexOfNearest(Node* node, const T& value, int nbItemsBefore, const std::function<bool(const T&, const T&)>& lesserThan);
      static Position positionOf(Node* node, const T& value, const std::function<bool(const T&, const T&)>& lesserThan);
      static Position positionOfNearest(Node* node, const T& value, const std::function<bool(const T&, const T&)>& lesserThan);
      static Node* getNode(Node* node, const T& value, int& position, const std::function<bool(const T&, const T&)>& lesserThan);
      inline static int getPosition(Node* node, const T& value, bool& exists, const std::function<bool(const T&, const T&)>& lesserThan);

      static void incrementSize(Node* node);
      static void decrementSize(Node* node);
      static void moveChild(Node* node1, int pos1, Node* node2, int pos2);
      static void insertChild(Node* child, Node* parent, int pos);

      static void remove(Node* node, int position);
      static void rebalance(Node* node);
      static Node* getRightNeighbor(Node* node, int& medianPosition);
      static Node* getLeftNeighbor(Node* node, int& medianPosition);
      static void merge(Node* leftNode, int medianPosition, Node* rightNode);
      static void shiftLeft(Node* node, const T& value = T(), int startPositionItem = 0, int startPositionChild = 0);
      static void shiftRight(Node* node, const T& value = T(), int startPositionItem = -1, int startPositionChild = -1);

      static Node* duplicateNode(Node* node);
      static void deleteNode(Node* node);

      static Node* add(Node* node, const T& value, const std::function<bool(const T&, const T&)>& lesserThan, Node* child = nullptr);
      static Node* split(Node* node, const T& value, const std::function<bool(const T&, const T&)>& lesserThan, Node* child = nullptr);

      //static void quickSort(const Position& first, const Position& last);
      static Position partition(const Position& first, const Position& last, const Position& pivot);

      struct SortedArrayData : public QSharedData
      {
         SortedArrayData(const std::function<bool(const T&, const T&)>& lesserThan) :
            root(new Node()), lesserThanFun(lesserThan)
         {}
         SortedArrayData(const SortedArrayData& other) :
            root(duplicateNode(other.root)),
            lesserThanFun(other.lesserThanFun)
         {}
         ~SortedArrayData() { deleteNode(this->root); }

         Node* root;
         std::function<bool(const T&, const T&)> lesserThanFun;
      };

      QSharedDataPointer<SortedArrayData> d;
   };
}

/////

template <typename T, int M>
Common::SortedArray<T, M>::iterator::iterator(const SortedArray& array, const Position& position) :
   array(array), currentPosition(position)
{
}

template <typename T, int M>
Common::SortedArray<T, M>::iterator::iterator(const iterator& other) :
   array(other.array), currentPosition(other.currentPosition)
{
}

template <typename T, int M>
bool Common::SortedArray<T, M>::iterator::operator==(const iterator& other) const
{
   return other.currentPosition == this->currentPosition;
}

template <typename T, int M>
bool Common::SortedArray<T, M>::iterator::operator!=(const iterator& other) const
{
   return other.currentPosition != this->currentPosition;
}

template <typename T, int M>
const T& Common::SortedArray<T, M>::iterator::operator*() const
{
   return this->currentPosition.node->items[this->currentPosition.p];
}

template <typename T, int M>
T* Common::SortedArray<T, M>::iterator::operator->() const
{
   return &this->currentPosition.node->items[this->currentPosition.p];
}

template <typename T, int M>
typename Common::SortedArray<T, M>::iterator& Common::SortedArray<T, M>::iterator::operator++()
{
   this->currentPosition = getNextPosition(this->currentPosition);
   return *this;
}

/////

/**
  * @exception InvalidMException
  */
template <typename T, int M>
Common::SortedArray<T, M>::SortedArray() :
   d(new SortedArrayData([](const T& e1, const T& e2) { return e1 < e2; }))
{
   // M must be an odd number.
   if (M % 2 == 0)
      throw InvalidMException();
}

template <typename T, int M>
Common::SortedArray<T, M>::SortedArray(const SortedArray& other) :
   d(other.d)
{
}

template <typename T, int M>
int Common::SortedArray<T, M>::size() const
{
   return this->d->root->size;
}

/**
  * Insert or update the given value. The update is done with assignement operator of T.
  * @param exists Optional, set to 'true' if the value already exists.
  * @return The position 'value'.
  */
template <typename T, int M>
int Common::SortedArray<T, M>::insert(const T& value, bool* exists)
{
   int position;
   Node* node = getNode(this->d->root, value, position, this->d->lesserThanFun);
   if (position == -1)
   {
      if (Node* newRoot = add(node, value, this->d->lesserThanFun))
         this->d->root = newRoot;
   }
   else
      node->items[position] = value;

   // If the argument 'exists' is given (is not a null pointer).
   if (exists)
      *exists = position != -1;

   return 0;
}

/**
  * @exception NotFoundException
  */
template <typename T, int M>
void Common::SortedArray<T, M>::removeFromIndex(int index)
{
   int position;
   Node* node = getFromIndex(this->d->root, index, 0, position);

   remove(node, position);
}

/**
  * Return 'false' if the value doesn't exist.
  */
template <typename T, int M>
bool Common::SortedArray<T, M>::remove(const T& value)
{
   int position;
   Node* node = getNode(this->d->root, value, position, this->d->lesserThanFun);
   if (position == -1)
      return false;
   remove(node, position);
   return true;
}

template <typename T, int M>
void Common::SortedArray<T, M>::clear()
{
    deleteNode(this->d->root);
    this->d->root = new Node();
}

template <typename T, int M>
inline bool Common::SortedArray<T, M>::isEmpty() const
{
   return this->size() == 0;
}

template <typename T, int M>
bool Common::SortedArray<T, M>::contains(const T& value) const
{
   int position;
   getNode(this->d->root, value, position, this->d->lesserThanFun);
   return position != -1;
}

/**
  * returns -1 if not found.
  */
template <typename T, int M>
int Common::SortedArray<T, M>::indexOf(const T& value) const
{
   return indexOf(this->d->root, value, 0, this->d->lesserThanFun);
}

/**
  * If an item matches the given value returns its index. Otherwise the index of the previous item is returned.
  * If the value is located before the first item then 0 is returned.
  * If the value is located after the last item then the last item index is returned.
  * Returns -1 if the array is empty.
  */
template <typename T, int M>
int Common::SortedArray<T, M>::indexOfNearest(const T& value) const
{
   return indexOfNearest(this->d->root, value, 0, this->d->lesserThanFun);
}

template <typename T, int M>
typename Common::SortedArray<T, M>::iterator Common::SortedArray<T, M>::begin() const
{
   Node* firstNode = getLeftmostNode(this->d->root);
   return iterator(*this, { firstNode, 0 });
}

/**
  * The end is always after the last item.
  * Warning: the complexity is O(h) where h = log_M(n+1) - 1
  * It's a good idea to compute it only once during a loop.
  */
template <typename T, int M>
typename Common::SortedArray<T, M>::iterator Common::SortedArray<T, M>::end() const
{
   Node* lastNode = getRightmostNode(this->d->root);
   return iterator(*this, { lastNode, lastNode->nbItems });
}

template <typename T, int M>
typename Common::SortedArray<T, M>::iterator Common::SortedArray<T, M>::iteratorOf(const T& value) const
{
   return iterator(*this, positionOf(this->d->root, value, this->d->lesserThanFun));
}

template <typename T, int M>
typename Common::SortedArray<T, M>::iterator Common::SortedArray<T, M>::iteratorOfNearest(const T& value) const
{
   return iterator(*this, positionOfNearest(this->d->root, value, this->d->lesserThanFun));
}

/**
  * @exception NotFoundException
  */
template <typename T, int M>
const T& Common::SortedArray<T, M>::getFromIndex(int index) const
{
   return this->get(index);
}

/**
  * @exception NotFoundException
  */
template <typename T, int M>
T& Common::SortedArray<T, M>::getFromIndex(int index)
{
   return this->get(index);
}

/**
  * @exception NotFoundException
  */
template <typename T, int M>
const T& Common::SortedArray<T, M>::getFromValue(const T& value) const
{
   int position;
   Node* node = getNode(this->d->root, value, position, this->d->lesserThanFun);
   if (position == -1)
      throw NotFoundException();
   return node->items[position];
}

/**
  * @exception NotFoundException
  */
template <typename T, int M>
T& Common::SortedArray<T, M>::getFromValue(const T& value)
{
   int position;
   Node* node = getNode(this->d->root, value, position, this->d->lesserThanFun);
   if (position == -1)
      throw NotFoundException();
   return node->items[position];
}

/**
  * Try to find a value. If can't then the value is inserted.
  */
template <typename T, int M>
T& Common::SortedArray<T, M>::operator[](const T& value)
{
   return this->getValue(value);
}

template <typename T, int M>
QList<T> Common::SortedArray<T, M>::toList() const
{
   QList<T> l;
   for (typename Common::SortedArray<T>::iterator i(*this); i.hasNext();)
      l << i.next();
   return l;
}

template <typename T, int M>
int Common::SortedArray<T, M>::getM() const
{
   return M;
}

/**
  * Must be call after data used to compare an element is modified.
  * Not implemented.
  */
/*template <typename T, int M>
void Common::SortedArray<T, M>::sort()
{
   if (this->size() < 2)
      return;
   Position start { getLeftmostNode(this->d->root), 0 };
   Position last { getRightmostNode(this->d->root), 0 };
   last.p = last.node->nbItems - 1;

   quickSort(start, last);
}*/

/**
  * Defines custom function used to define the order of the values.
  * The array is automatically reordered after calling this method.
  */
template <typename T, int M>
void Common::SortedArray<T, M>::setSortedFunction(const std::function<bool(const T&, const T&)>& lesserThan)
{
   this->d->lesserThanFun = lesserThan;

   // For the moment we recreate an entire new tree and inserting all the elements in it.
   // A better approach will be to re-sort the tree in place.
   QSharedDataPointer<SortedArrayData> newD(new SortedArrayData(lesserThan));
   for (iterator i = this->begin(); i != this->end(); ++i)
   {
      int position;
      const T& value = *i;
      Node* node = getNode(newD->root, value, position, newD->lesserThanFun);

      Q_ASSERT(position == -1); // The value should never be found.

      if (Node* newRoot = add(node, value, newD->lesserThanFun))
         newD->root = newRoot;
   }
   this->d = newD;
}

template <typename T, int M>
T& Common::SortedArray<T, M>::get(int index) const
{
   if (index >= this->d->root->size)
      throw NotFoundException();

   int position;
   Node* node = getFromIndex(this->d->root, index, 0, position);
   return node->items[position];
}

template <typename T, int M>
T& Common::SortedArray<T, M>::getValue(const T& value)
{
   int position;
   Node* node = getNode(this->d->root, value, position, this->d->lesserThanFun);

   if (position == -1)
   {
      if (Node* newRoot = add(node, value, this->d->lesserThanFun))
         this->d->root = newRoot;

      node = getNode(node->parent ? node->parent : node, value, position, this->d->lesserThanFun);
   }

   return node->items[position];
}

/**
  * Return the leftmost node.
  */
template <typename T, int M>
typename Common::SortedArray<T, M>::Node* Common::SortedArray<T, M>::getLeftmostNode(Node* node)
{
   if (Node* child = node->children[0])
      return getLeftmostNode(child);
   return node;
}

template <typename T, int M>
typename Common::SortedArray<T, M>::Node* Common::SortedArray<T, M>::getRightmostNode(Node* node)
{
   if (Node* child = node->children[node->nbItems])
      return getRightmostNode(child);
   return node;
}

/**
  * Get the next position to the right.
  */
template <typename T, int M>
typename Common::SortedArray<T, M>::Position Common::SortedArray<T, M>::getNextPosition(const Position& currentPosition)
{
   Position nextPosition = currentPosition;
   nextPosition.p++;

   if (Node* child = nextPosition.node->children[nextPosition.p])
   {
      nextPosition.node = getLeftmostNode(child);
      nextPosition.p = 0;
      return nextPosition;
   }

   if (nextPosition.p < nextPosition.node->nbItems)
      return nextPosition;

   while (nextPosition.node->parent)
   {
      for (int i = 0; i < nextPosition.node->parent->nbItems; i++)
         if (nextPosition.node->parent->children[i] == nextPosition.node)
         {
            nextPosition.node = nextPosition.node->parent;
            nextPosition.p = i;
            return nextPosition;
         }
      nextPosition.node = nextPosition.node->parent;
   }

   return { currentPosition.node, currentPosition.p + 1 };
}

/**
  * If there is no previous element then p == -1.
  */
template <typename T, int M>
typename Common::SortedArray<T, M>::Position Common::SortedArray<T, M>::getPreviousPosition(const Position& currentPosition)
{
   Position previousPosition = currentPosition;

   if (Node* child = previousPosition.node->children[previousPosition.p])
   {
      previousPosition.node = getRightmostNode(child);
      previousPosition.p = previousPosition.node->nbItems - 1;
      return previousPosition;
   }

   previousPosition.p--;

   if (previousPosition.p >= 0)
      return previousPosition;

   while (previousPosition.node->parent)
   {
      for (int i = 1; i <= previousPosition.node->parent->nbItems; i++)
         if (previousPosition.node->parent->children[i] == previousPosition.node)
         {
            previousPosition.node = previousPosition.node->parent;
            previousPosition.p = i - 1;
            return previousPosition;
         }
      previousPosition.node = previousPosition.node->parent;
   }

   return { currentPosition.node, currentPosition.p - 1 };
}

template <typename T, int M>
typename Common::SortedArray<T, M>::Node* Common::SortedArray<T, M>::getFromIndex(Node* node, int index, int nbItemsBefore, int& position)
{
   for (int i = 0; i <= node->nbItems; i++)
   {
      if (node->children[i])
      {
         if (index < nbItemsBefore + node->children[i]->size)
            return getFromIndex(node->children[i], index, nbItemsBefore, position);
         else
            nbItemsBefore += node->children[i]->size;
      }

      if (nbItemsBefore++ == index && i < node->nbItems)
      {
         position = i;
         return node;
      }
   }

   throw NotFoundException();
}

template <typename T, int M>
int Common::SortedArray<T, M>::indexOf(Node* node, const T& value, int nbItemsBefore, const std::function<bool(const T&, const T&)>& lesserThan)
{
   bool exists;
   int position = getPosition(node, value, exists, lesserThan);

   nbItemsBefore += position;
   for (int i = 0; i < position; i++)
      if (node->children[i])
         nbItemsBefore += node->children[i]->size;

   if (!exists)
   {
      if (node->children[position])
         return indexOf(node->children[position], value, nbItemsBefore, lesserThan);
      return -1;
   }

   return nbItemsBefore;
}

template <typename T, int M>
int Common::SortedArray<T, M>::indexOfNearest(Node* node, const T& value, int nbItemsBefore, const std::function<bool(const T&, const T&)>& lesserThan)
{
   bool exists;
   int position = getPosition(node, value, exists, lesserThan);

   nbItemsBefore += position;
   for (int i = 0; i < position; i++)
      if (node->children[i])
         nbItemsBefore += node->children[i]->size;

   if (!exists)
   {
      if (node->children[position])
         return indexOfNearest(node->children[position], value, nbItemsBefore, lesserThan);

      if (nbItemsBefore == 0)
         return 0;
      else
         return nbItemsBefore - 1;
   }

   return nbItemsBefore;
}

/**
  * @exception NotFoundException
  */
template <typename T, int M>
typename Common::SortedArray<T, M>::Position Common::SortedArray<T, M>::positionOf(Node* node, const T& value, const std::function<bool(const T&, const T&)>& lesserThan)
{
   bool exists;
   int position = getPosition(node, value, exists, lesserThan);

   if (!exists)
   {
      if (node->children[position])
         return node->iteratorOf(node->children[position], value, lesserThan);
      throw NotFoundException();
   }

   return { node, position };
}

template <typename T, int M>
typename Common::SortedArray<T, M>::Position Common::SortedArray<T, M>::positionOfNearest(Node* node, const T& value, const std::function<bool(const T&, const T&)>& lesserThan)
{
   bool exists;
   int position = getPosition(node, value, exists, lesserThan);

   if (!exists)
   {
      if (node->children[position])
         return positionOfNearest(node->children[position], value, lesserThan);

      if (position > 0)
         return { node, position - 1};
      else
      {
         Position previous = getPreviousPosition({ node, position });
         if (previous.p != -1)
            return previous;
         return { node, position };
      }
   }

   return { node, position };
}

/**
  * Returns a node corresponding to the given value, this node may or may not contain the value.
  * If the node doesn't contain the value then position is set to -1.
  */
template <typename T, int M>
typename Common::SortedArray<T, M>::Node* Common::SortedArray<T, M>::getNode(Node* node, const T& value, int& position, const std::function<bool(const T&, const T&)>& lesserThan)
{
   bool exists;
   position = getPosition(node, value, exists, lesserThan);
   if (!exists)
   {
      if (node->children[position])
         return getNode(node->children[position], value, position, lesserThan);
      position = -1;
   }

   return node;
}

/**
  * Return the position of the value, if it doesn't exist returns the position where to insert the value.
  * It may return a position after the last value if the new one have to be inserted in the last position.
  */
template <typename T, int M>
inline int Common::SortedArray<T, M>::getPosition(Node* node, const T& value, bool& exists, const std::function<bool(const T&, const T&)>& lesserThan)
{
   exists = false;
   if (node->nbItems == 0 || lesserThan(value, node->items[0]))
      return 0;

   if (lesserThan(node->items[node->nbItems-1], value))
      return node->nbItems;

   // Value is equal to the first item.
   if (!lesserThan(node->items[0], value))
   {
      exists = true;
      return 0;
   }

   // Values at 'i1' and 'i2' are never equal to 'value'.
   int i1 = 0;
   int i2 = node->nbItems;

   forever
   {
      int i3 = (i2 + i1) >> 1;

      if (lesserThan(value, node->items[i3]))
      {
         if (i1 + 1 == i3)
            return i3;
         i2 = i3;
      }
      else if (lesserThan(node->items[i3], value))
      {
         if (i3 + 1 == i2)
            return i2;
         i1 = i3;
      }
      else
      {
         exists = true;
         return i3;
      }
   }
}

template <typename T, int M>
void Common::SortedArray<T, M>::incrementSize(Node* node)
{
   node->size++;
   if (node->parent)
      incrementSize(node->parent);
}

template <typename T, int M>
void Common::SortedArray<T, M>::decrementSize(Node* node)
{
   node->size--;
   if (node->parent)
      decrementSize(node->parent);
}

/**
  * Move the pos1'th child of node1 to the pos2'th position of node2.
  * The child to move may be null.
  */
template <typename T, int M>
void Common::SortedArray<T, M>::moveChild(Node* node1, int pos1, Node* node2, int pos2)
{
   Q_ASSERT(node2->children[pos2] == nullptr);
   Q_ASSERT(node1 != node2);

   node2->children[pos2] = node1->children[pos1];

   if (node2->children[pos2])
   {
      node2->size += node2->children[pos2]->size;
      node2->children[pos2]->parent = node2;

      node1->children[pos1] = nullptr;
      node1->size -= node2->children[pos2]->size;
   }
}

/**
  * 'child' may be null.
  */
template <typename T, int M>
void Common::SortedArray<T, M>::insertChild(Node* child, Node* parent, int pos)
{
   Q_ASSERT(parent->children[pos] == nullptr);

   parent->children[pos] = child;
   if (child)
   {
      child->parent = parent;
      parent->size += child->size;
   }
}

template <typename T, int M>
void Common::SortedArray<T, M>::remove(Node* node, int position)
{
   if (node->nbItems == node->size) // It's a leaf
   {
      node->items[position] = T(); // Delete the element.
      node->nbItems--;
      decrementSize(node);

      for (int i = position; i < node->nbItems; i++)
         qSwap(node->items[i], node->items[i+1]);

      rebalance(node);
   }
   else // It's an internal node.
   {
      Node* leftChild = getRightmostNode(node->children[position]);
      Node* rightChild = getLeftmostNode(node->children[position+1]);

      if (leftChild->nbItems > rightChild->nbItems)
      {
         qSwap(leftChild->items[leftChild->nbItems - 1], node->items[position]);
         remove(leftChild, leftChild->nbItems - 1);
      }
      else
      {
         qSwap(rightChild->items[0], node->items[position]);
         remove(rightChild, 0);
      }
   }
}

template <typename T, int M>
void Common::SortedArray<T, M>::rebalance(Node* node)
{
   if (!node->parent || node->nbItems >= M / 2 )
      return;

   // 1) We look on the left and right neighbors if we can take one of their children.
   int medianPositionRight;
   Node* rightNeighbor = getRightNeighbor(node, medianPositionRight);
   if (rightNeighbor && rightNeighbor->nbItems > M / 2)
   {
      node->items[node->nbItems] = node->parent->items[medianPositionRight];

      // Move the associated child if it exists.
      moveChild(rightNeighbor, 0, node, node->nbItems + 1);

      node->nbItems++;

      // We don't need to change the size of parent because they share the same parent.
      node->size++;
      rightNeighbor->size--;

      node->parent->items[medianPositionRight] = rightNeighbor->items[0];

      shiftLeft(rightNeighbor);
   }
   else
   {
      int medianPositionLeft = 0;
      Node* leftNeighbor = getLeftNeighbor(node, medianPositionLeft);
      if (leftNeighbor && leftNeighbor->nbItems > M / 2)
      {
         shiftRight(node, node->parent->items[medianPositionLeft]);

         // Move the associated child if it exists.
         moveChild(leftNeighbor, leftNeighbor->nbItems, node, 0);

         leftNeighbor->nbItems--;

         // We don't need to change the size of parent because they share the same parent.
         node->size++;
         leftNeighbor->size--;

         node->parent->items[medianPositionLeft] = leftNeighbor->items[leftNeighbor->nbItems];

         leftNeighbor->items[leftNeighbor->nbItems] = T();
      }
      else // If we can't take an element from the right neighbor or from the left neighbor, we have to merge it.
      {
         if (rightNeighbor)
            merge(node, medianPositionRight, rightNeighbor);
         else
            merge(leftNeighbor, medianPositionLeft, node);
      }
   }
}

/**
  * Merge two neighbors together. 'leftNode->nbItems' + 'rightNode->nbItems' + 1 must be equal to M - 1.
  */
template <typename T, int M>
void Common::SortedArray<T, M>::merge(Common::SortedArray<T, M>::Node* leftNode, int medianPosition, Node* rightNode)
{
   Node* parent = leftNode->parent;

   // Special case when the parent is the root and both given nodes can be merge into it.
   if (!parent->parent && parent->nbItems == 1)
   {
      // Median value.
      parent->items[leftNode->nbItems] = parent->items[0];

      for (int i = 0; i < leftNode->nbItems; i++)
      {
         parent->items[i] = leftNode->items[i];
         if (parent->children[i] = leftNode->children[i])
            parent->children[i]->parent = parent;
      }
      if (parent->children[leftNode->nbItems] = leftNode->children[leftNode->nbItems])
         parent->children[leftNode->nbItems]->parent = parent;

      for (int i = 0; i < rightNode->nbItems; i++)
      {
         parent->items[leftNode->nbItems + 1 + i] = rightNode->items[i];
         if (parent->children[leftNode->nbItems + 1 + i] = rightNode->children[i])
            parent->children[leftNode->nbItems + 1 + i]->parent = parent;
      }
      if (parent->children[leftNode->nbItems + rightNode->nbItems + 1] = rightNode->children[rightNode->nbItems])
         parent->children[leftNode->nbItems + rightNode->nbItems + 1]->parent = parent;

      parent->nbItems = M - 1;

      delete leftNode;
      delete rightNode;
   }
   else // Else we merge the median of the parent and the elements of the right nodes into the left node.
   {
      leftNode->items[leftNode->nbItems] = parent->items[medianPosition];
      leftNode->nbItems++;
      leftNode->size++;

      for (int i = 0; i < rightNode->nbItems; i++)
      {
         leftNode->items[leftNode->nbItems+i] = rightNode->items[i];
         if (leftNode->children[leftNode->nbItems+i] = rightNode->children[i])
            leftNode->children[leftNode->nbItems+i]->parent = leftNode;
      }
      leftNode->nbItems += rightNode->nbItems;
      leftNode->size += rightNode->size;
      if (leftNode->children[leftNode->nbItems] = rightNode->children[rightNode->nbItems])
         leftNode->children[leftNode->nbItems]->parent = leftNode;

      delete rightNode;

      shiftLeft(parent, T(), medianPosition, medianPosition+1);

      rebalance(parent);
   }
}

/**
  * @remarks Will decrement 'node->nbItems'.
  */
template <typename T, int M>
void Common::SortedArray<T, M>::shiftLeft(Node* node, const T& value, int startPositionItem, int startPositionChild)
{
   node->nbItems--;

   for (int i = startPositionItem; i < node->nbItems; i++)
      node->items[i] = node->items[i+1];
   node->items[node->nbItems] = value;

   for (int i = startPositionChild; i <= node->nbItems; i++)
      node->children[i] = node->children[i+1];
   node->children[node->nbItems+1] = nullptr;
}

/**
  * @remarks Will increment 'node->nbItems'.
  */
template <typename T, int M>
void Common::SortedArray<T, M>::shiftRight(Node* node, const T& value, int startPositionItem, int startPositionChild)
{
   startPositionItem = startPositionItem == -1 ? node->nbItems : startPositionItem;
   startPositionChild = startPositionChild == -1 ? node->nbItems+1 : startPositionChild;

   for (int i = startPositionItem; i > 0; i--)
      node->items[i] = node->items[i-1];
   node->items[0] = value;

   for (int i = startPositionChild; i > 0; i--)
      node->children[i] = node->children[i-1];
   node->children[0] = nullptr;

   node->nbItems++;
}

template <typename T, int M>
typename Common::SortedArray<T, M>::Node* Common::SortedArray<T, M>::getRightNeighbor(Node* node, int& medianPosition)
{
   if (node->parent)
      for (int i = 0; i < node->parent->nbItems; i++)
         if (node->parent->children[i] == node)
         {
            medianPosition = i;
            return node->parent->children[i+1];
         }

   return nullptr;
}

template <typename T, int M>
typename Common::SortedArray<T, M>::Node* Common::SortedArray<T, M>::getLeftNeighbor(Node* node, int& medianPosition)
{
   if (node->parent)
      for (int i = 1; i <= node->parent->nbItems; i++)
         if (node->parent->children[i] == node)
         {
            medianPosition = i-1;
            return node->parent->children[i-1];
         }

   return nullptr;
}

template <typename T, int M>
typename Common::SortedArray<T, M>::Node* Common::SortedArray<T, M>::duplicateNode(Node* node)
{
   Node* newNode = new Node();

   memcpy(newNode, node, sizeof(Node));

   for (int i = 0; i <= newNode->nbItems; i++)
      if (newNode->children[i])
      {
         newNode->children[i] = duplicateNode(newNode->children[i]);
         newNode->children[i]->parent = newNode;
      }

   return newNode;
}

template <typename T, int M>
void Common::SortedArray<T, M>::deleteNode(Node* node)
{
   for (int i = 0; i <= node->nbItems; i++)
   {
      if (node->children[i])
         deleteNode(node->children[i]);
   }
   delete node;
}

/**
  * Add the element 'e' to 'node', may attach an optional child after the position of 'e'.
  * @return the new root if a new root has been created, else returns 'nullptr'.
  */
template <typename T, int M>
typename Common::SortedArray<T, M>::Node* Common::SortedArray<T, M>::add(Node* node, const T& value, const std::function<bool(const T&, const T&)>& lesserThan, Node* child)
{
   if (child)
      child->parent = node;

   // The node isn't full.
   if (node->nbItems < M-1)
   {
      for (int i = node->nbItems - 1; i >= -1; i--)
      {
         // If 'e' must be put after the ith element.
         if (i == -1 || lesserThan(node->items[i], value))
         {
            node->items[i+1] = value;
            node->children[i+2] = child; // If 'child' is null then "node->children[i+2]" must be null too.
            break;
         }
         else
         {
            node->items[i+1] = node->items[i];
            node->children[i+2] = node->children[i+1];
         }
      }
      node->nbItems++;
      incrementSize(node);
   }
   // The node is full, we have to split it and give the median value to its parent.
   // If the node doesn't have a parent we create one.
   else
   {
      Node* rightNode = split(node, value, lesserThan, child);

      Node* newRoot;

      if (node->parent)
      {
         newRoot = add(node->parent, node->items[M / 2], lesserThan, rightNode);
      }
      else
      {
         newRoot = new Node();
         newRoot->nbItems = 1;
         newRoot->size = 1 + node->size + rightNode->size;
         newRoot->items[0] = node->items[M / 2]; // Copy the median value.
         newRoot->children[0] = node;
         newRoot->children[1] = rightNode;
         node->parent = newRoot;
         rightNode->parent = newRoot;
      }
      node->items[M / 2] = T(); // Remove the median value.

      return newRoot;
   }

   return nullptr;
}

/**
  * Split the given node by creating a new node.
  * The median value is put in the position "M / 2" in 'node'.
  */
template <typename T, int M>
typename Common::SortedArray<T, M>::Node* Common::SortedArray<T, M>::split(Node* node, const T& value, const std::function<bool(const T&, const T&)>& lesserThan, Node* child)
{
   Node* rightNode = new Node();
   rightNode->nbItems = (M-1) / 2;
   rightNode->size = rightNode->nbItems;
   node->nbItems = rightNode->nbItems;
   node->size -= rightNode->nbItems;

   int i = M - 2;
   bool eInsertedInRightNode = false;

   // First we copy the values in the new node.
   for (int j = rightNode->nbItems - 1; j >= 0; j--, i--)
   {
      if (!eInsertedInRightNode && lesserThan(node->items[i], value))
      {
         rightNode->items[j] = value;

         if (child)
         {
            insertChild(child, rightNode, j+1);
            node->size -= child->size;
         }
         eInsertedInRightNode = true;
         i++;
      }
      else
      {
         rightNode->items[j] = node->items[i];
         node->items[i] = T();
         moveChild(node, i+1, rightNode, j+1);
      }
   }

   // Defines the median value in "node->items[i+1]".
   if (!eInsertedInRightNode && lesserThan(node->items[i], value))
   {
      node->items[i+1] = value;
      if (child)
      {
         insertChild(child, rightNode, 0);
         node->size -= child->size;
      }
      eInsertedInRightNode = true;
   }
   else
   {
      if (!eInsertedInRightNode)
         node->items[i+1] = node->items[i];
      moveChild(node, i+1, rightNode, 0);
   }

   // The left part of the split node.
   if (!eInsertedInRightNode)
      while (--i >= -1)
      {
         if (!eInsertedInRightNode && (i == -1 || lesserThan(node->items[i], value)))
         {
            node->items[i+1] = value;
            if (child)
               node->children[i+2] = child;
            break;
         }
         else
         {
            node->items[i+1] = node->items[i];
            node->children[i+2] = node->children[i+1];
         }
      }

   return rightNode;
}

/**
  * Not implemented
  */
/*
template <typename T, int M>
void Common::SortedArray<T, M>::quickSort(const Position& first, const Position& last)
{
   if (getNextPosition(first) == last)
      return;

   Position pivot;
}
template <typename T, int M>
typename Common::SortedArray<T, M>::Position Common::SortedArray<T, M>::partition(const Position& first, const Position& last, const Position& pivot)
{

}*/

#endif
