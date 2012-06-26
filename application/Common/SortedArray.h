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

#include <QSharedDataPointer>

/**
  * @class Common::SortedArray
  *
  * The goal of the class is to be able to use an ordered list as an array, here are listed some properties:
  *  - Access to element by integer index like an array: 0, 1, 2, ...
  *  - Found the index of a given value.
  *  - The elements are kept ordered when a new one is inserted.
  * The type T must have the operators '<' and '==' defined.
  *
  * 'SortedArray' is implemented as a B-Tree, see here for more information: http://en.wikipedia.org/wiki/B-tree
  * M is the order according Knuth's definition. This is the maximum number of children a node can have.
  */

namespace Common
{
   template<typename T, int M = 7>
   class SortedArray
   {
      struct Node;
   public:      
      class NotFoundException {};
      class InvalidMException {};

      SortedArray();
      SortedArray(const SortedArray& other);

      int size() const;
      int insert(const T& value, bool* exists = nullptr);
      void remove(int index);
      bool remove(const T& value);

      bool contains(const T& value) const;

      int indexOf(const T& value) const;

      const T& operator[](int index) const;
      T& operator[](int index);

      int getM() const;

      /**
        * A very basic iterator to iterator on the sorted elements.
        */
      class Iterator
      {
      public:
         Iterator(const SortedArray&);
         bool hasNext() const;
         const T& next();

      private:
         const SortedArray& array;
         Node* currentNode;
         int currentPosition;
      };

   private:
      struct Node
      {
         Node(Node* parent = nullptr) : nbItems(0), parent(parent), children({}), size(0) {}

         int nbItems;
         T items[M-1];

         Node* parent; // The root has no parent (nullptr).
         Node* children[M];

         int size; // The size of the subtree (children's sizes + this->nbItems).
      };

      static Node* getLeftmostNode(Node* node);
      static Node* getRightmostNode(Node* node);

      static int getNextPosition(Node*& node, int currentPosition);
      static Node* getFromIndex(Node* node, int index, int nbItemsBefore, int& position);
      static int indexOf(Node* node, const T& value, int nbItemsBefore);
      static Node* getNode(Node* node, const T& value, int& position);
      static int getPosition(Node* node, const T& value, bool& exists);
      static void incrementSize(Node* node);
      static void decrementSize(Node* node);
      static void moveChild(Node* node1, int pos1, Node* node2, int pos2);
      static void insertChild(Node* child, Node* parent, int pos);

      static void remove(Node* node, int position);
      static void rebalance(Node* node);
      static Node* getRightNeighbor(Node* node, int& medianPosition);
      static Node* getLeftNeighbor(Node* node, int& medianPosition);

      static Node* duplicateNode(Node* node);
      static void deleteNode(Node* node);

      void add(Node* node, const T& value, Node* child = nullptr);
      Node* split(Node* node, const T& value, Node* child = nullptr);

      struct SortedArrayData : public QSharedData
      {
         SortedArrayData() : root(new Node()) {}
         SortedArrayData(const SortedArrayData& other) : root(duplicateNode(other.root)) {}
         ~SortedArrayData() { deleteNode(this->root); }

         Node* root;
      };

      QSharedDataPointer<SortedArrayData> d;
   };
}

/**
  * @exception InvalidMException
  */
template <typename T, int M>
Common::SortedArray<T, M>::SortedArray() :
   d(new SortedArrayData())
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
   Node* node = getNode(this->d->root, value, position);
   if (position == -1)
      this->add(node, value);
   else
      node->items[position] = value;

   if (exists)
      *exists = position != -1;

   return 0;
}

/**
  * @exception NotFoundException
  */
template <typename T, int M>
void Common::SortedArray<T, M>::remove(int index)
{
   int position;
   Node* node = getFromIndex(this->d->root, index, 0, position);

   remove(node, position);
}

/**
  * Return false if the value doesn't exist.
  */
template <typename T, int M>
bool Common::SortedArray<T, M>::remove(const T& value)
{
   int position;
   Node* node = getNode(this->d->root, value, position);
   if (!node)
      return false;
   remove(node, position);
   return true;
}

template <typename T, int M>
bool Common::SortedArray<T, M>::contains(const T& value) const
{
   int position;
   getNode(this->d->root, value, position);
   return position != -1;
}

/**
  * @exception NotFoundException
  */
template <typename T, int M>
int Common::SortedArray<T, M>::indexOf(const T& value) const
{
   return indexOf(this->d->root, value, 0);
}

/**
  * @exception NotFoundException
  */
template <typename T, int M>
const T& Common::SortedArray<T, M>::operator[](int index) const
{
   return (*this)[index];
}

/**
  * @exception NotFoundException
  */
template <typename T, int M>
T& Common::SortedArray<T, M>::operator[](int index)
{
   if (index >= this->d->root->size)
      throw NotFoundException();

   int position;
   Node* node = getFromIndex(this->d->root, index, 0, position);
   return node->items[position];
}

template <typename T, int M>
int Common::SortedArray<T, M>::getM() const
{
   return M;
}

template <typename T, int M>
Common::SortedArray<T, M>::Iterator::Iterator(const SortedArray& array) :
   array(array), currentNode(nullptr), currentPosition(0)
{
   if (array.size() > 0)
      this->currentNode = getLeftmostNode(array.d->root);
}

template <typename T, int M>
bool Common::SortedArray<T, M>::Iterator::hasNext() const
{
   return this->currentNode != nullptr;
}

/**
  * @exception NotFoundException
  */
template <typename T, int M>
const T& Common::SortedArray<T, M>::Iterator::next()
{
   if (this->currentNode == nullptr)
      throw NotFoundException();

   const T& value = this->currentNode->items[this->currentPosition];
   this->currentPosition = getNextPosition(this->currentNode, this->currentPosition);
   return value;
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
  * 'node' is set to 'nullptr' if there is no next position.
  */
template <typename T, int M>
int Common::SortedArray<T, M>::getNextPosition(Node*& node, int currentPosition)
{
   currentPosition++;

   if (Node* child = node->children[currentPosition])
   {
      node = getLeftmostNode(child);
      return 0;
   }

   if (currentPosition < node->nbItems)
      return currentPosition;

   while (node->parent)
   {
      for (int i = 0; i < node->parent->nbItems; i++)
         if (node->parent->children[i] == node)
         {
            node = node->parent;
            return i;
         }
      node = node->parent;
   }

   node = nullptr;
   return 0;
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
int Common::SortedArray<T, M>::indexOf(Node* node, const T& value, int nbItemsBefore)
{
   bool exists;
   int position = getPosition(node, value, exists);

   nbItemsBefore += position;
   for (int i = 0; i < position; i++)
      if (node->children[i])
         nbItemsBefore += node->children[i]->size;

   if (!exists)
   {
      if (node->children[position])
         return indexOf(node->children[position], value, nbItemsBefore);
      return -1;
   }

   return nbItemsBefore;
}

/**
  * Returns a node corresponding to the given value, this node may or may not contain the value.
  * If the node doesn't contain the value it's a leaf.
  */
template <typename T, int M>
typename Common::SortedArray<T, M>::Node* Common::SortedArray<T, M>::getNode(Common::SortedArray<T, M>::Node* node, const T& value, int& position)
{
   bool exists;
   position = getPosition(node, value, exists);
   if (!exists)
   {
      if (node->children[position])
         return getNode(node->children[position], value, position);
      position = -1;
   }

   return node;
}

/**
  * Return the position of the value, if it doesn't exist returns the position where to insert the value.
  * It may return a position after the last value if the new one have to be inserted in the last position.
  */
template <typename T, int M>
int Common::SortedArray<T, M>::getPosition(Node* node, const T& value, bool& exists)
{
   exists = false;
   if (node->nbItems == 0 || value < node->items[0])
      return 0;

   if (node->items[node->nbItems-1] < value)
      return node->nbItems;

   int i1 = 0;
   int i2 = node->nbItems;

   forever
   {
      int i3 = (i2 + i1) / 2;
      if (node->items[i3] == value)
      {
         exists = true;
         return i3;
      }

      if (i1 == i3)
      {
         if (node->items[i2] == value)
            exists = true;
         return i2;
      }

      if (node->items[i3] < value)
         i1 = i3;
      else
         i2 = i3;
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
   if (node->nbItems >= M / 2 )
      return;

   // 1) We look on the left and right neighbors if we can take one of their children.
   int medianPosition;
   Node* neighbor = getRightNeighbor(node, medianPosition);
   if (neighbor && neighbor->nbItems > M / 2)
   {
      node->items[node->nbItems] = node->parent->items[medianPosition];
      node->children[node->nbItems + 1] = neighbor->children[0];

      node->nbItems++;
      neighbor->nbItems--;
      // We don't need to change the size of parent because they share the same parent.
      node->size++;
      neighbor->size--;

      node->parent->items[medianPosition] = neighbor->items[0];

      for (int i = 0; i < neighbor->nbItems; i++)
      {
         neighbor->items[i] = neighbor->items[i+1];
         neighbor->children[i] = neighbor->children[i+1];
      }
      neighbor->children[neighbor->nbItems] = neighbor->children[neighbor->nbItems+1];

      neighbor->items[neighbor->nbItems] = T();
   }
   else if ((neighbor = getLeftNeighbor(node, medianPosition)) && neighbor->nbItems > M / 2)
   {
      for (int i = node->nbItems; i > 0; i--)
      {
         node->items[i] = node->items[i-1];
         node->children[i+1] = node->children[i];
      }
      node->children[1] = node->children[0];

      node->items[0] = node->parent->items[medianPosition];
      node->children[0] = neighbor->children[neighbor->nbItems];

      node->nbItems++;
      neighbor->nbItems--;
      // We don't need to change the size of parent because they share the same parent.
      node->size++;
      neighbor->size--;

      node->parent->items[medianPosition] = neighbor->items[neighbor->nbItems];

      neighbor->items[neighbor->nbItems] = T();
   }
   else
   {
      ; // TODO
   }
}

template <typename T, int M>
typename Common::SortedArray<T, M>::Node* Common::SortedArray<T, M>::getRightNeighbor(Common::SortedArray<T, M>::Node* node, int& medianPosition)
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
typename Common::SortedArray<T, M>::Node* Common::SortedArray<T, M>::getLeftNeighbor(Common::SortedArray<T, M>::Node* node, int& medianPosition)
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

   memcpy(newNode, node, sizeof(node));

   for (int i = 0; i <= newNode->nbItems; i++)
      if (newNode->children[i])
         newNode->children[i] = duplicateNode(newNode->children[i]);

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
  */
template <typename T, int M>
void Common::SortedArray<T, M>::add(Node* node, const T& value, Node* child)
{
   if (child)
      child->parent = node;

   // The node isn't full.
   if (node->nbItems < M-1)
   {
      for (int i = node->nbItems - 1; i >= -1; i--)
      {
         // If 'e' must be put after the ith element.
         if (i == -1 || node->items[i] < value)
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
      Node* rightNode = split(node, value, child);

      if (node->parent)
      {
         add(node->parent, node->items[M / 2], rightNode);
      }
      else
      {
         this->d->root = new Node();
         this->d->root->nbItems = 1;
         this->d->root->size = 1 + node->size + rightNode->size;
         this->d->root->items[0] = node->items[M / 2]; // Copy the median value.
         this->d->root->children[0] = node;
         this->d->root->children[1] = rightNode;
         node->parent = this->d->root;
         rightNode->parent = this->d->root;
      }
      node->items[M / 2] = T(); // Remove the median value.
   }
}

/**
  * Split the given node by creating a new node.
  * The median value is put in the position "M / 2" in 'node'.
  */
template <typename T, int M>
typename Common::SortedArray<T, M>::Node* Common::SortedArray<T, M>::split(Node* node, const T& value, Node* child)
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
      if (!eInsertedInRightNode && node->items[i] < value)
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
   if (!eInsertedInRightNode && node->items[i] < value)
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
         if (!eInsertedInRightNode && (i == -1 || node->items[i] < value))
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

#endif
