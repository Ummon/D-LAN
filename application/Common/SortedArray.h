#ifndef COMMON_SORTARRAY_H
#define COMMON_SORTARRAY_H

#include <QList>

/**
  * @class Common::SortedArray
  *
  */

namespace Common
{
   template<typename T, int M = 3>
   class SortedArray
   {
   public:
      SortedArray();

      int size() const;
      int insert(const T& value, bool* exists = nullptr);
      void remove(int index);

      int indexOf(const T& value);

      const T& operator[](int index) const;
      T& operator[](int index);

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

      int getPosition(const T& value, bool& exists);

      static Node* getLeaf(Node* node, const T& value);
      static void incrementSize(Node* node);
      static void moveChild(Node* node1, int pos1, Node* node2, int pos2);
      static void insertChild(Node* child, Node* parent, int pos);

      void add(Node* node, const T& e, Node* child = nullptr);
      Node* split(Node* node, const T& e, Node* child = nullptr);

      Node* root;
   };
}

template <typename T, int M>
Common::SortedArray<T, M>::SortedArray() :
   root(new Node())
{
}

template <typename T, int M>
int Common::SortedArray<T, M>::size() const
{
   return this->root->size;
}

/**
  * Insert or update the given value. The update is done with assignement operator of T.
  * @param exists Optional, set to 'true' if the value already exists.
  * @return The position 'value'.
  */
template <typename T, int M>
int Common::SortedArray<T, M>::insert(const T& value, bool* exists)
{
   Node* leaf = getLeaf(this->root, value);
   this->add(leaf, value);
   return 0;
}

/**
  * Returns a leaf corresponding to the given value, this leaf may or may not contain the value.
  */
template <typename T, int M>
typename Common::SortedArray<T, M>::Node* Common::SortedArray<T, M>::getLeaf(Common::SortedArray<T, M>::Node* node, const T& value)
{
   if (node->nbItems == node->size)
      return node;

   for (int i = 0; i < node->nbItems; i++)
      if (value < node->items[i])
      {
         if (node->children[i])
            return getLeaf(node->children[i], value);
         return node;
      }

   if (node->children[node->nbItems])
      return getLeaf(node->children[node->nbItems], value);
   return node;
}

template <typename T, int M>
void Common::SortedArray<T, M>::incrementSize(Node* node)
{
   node->size++;
   if (node->parent)
      incrementSize(node->parent);
}

/**
  * Move the pos1'th child of node1 to the pos2'th position of node2.
  * The child to move may be null.
  */
template <typename T, int M>
void Common::SortedArray<T, M>::moveChild(Node* node1, int pos1, Node* node2, int pos2)
{
   Q_ASSERT(node2->children[pos2] == nullptr);

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

/**
  * Add the element 'e' to 'node', may attach an optional child after the position of 'e'.
  */
template <typename T, int M>
void Common::SortedArray<T, M>::add(Node* node, const T& e, Node* child)
{
   if (child)
      child->parent = node;

   // The node isn't full.
   if (node->nbItems < M-1)
   {
      for (int i = node->nbItems - 1; i >= -1; i--)
      {
         // If 'e' must be put after the ith element.
         if (i == -1 || e > node->items[i])
         {
            node->items[i+1] = e;
            node->children[i+2] = child; // If child is null then "node->children[i+2]" must be null too.
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
      Node* rightNode = split(node, e, child);

      if (node->parent)
      {
         add(node->parent, node->items[M / 2], rightNode);
      }
      else
      {
         this->root = new Node();
         this->root->nbItems = 1;
         this->root->size = 1 + node->size + rightNode->size;
         this->root->items[0] = node->items[M / 2]; // Copy the median value.
         this->root->children[0] = node;
         this->root->children[1] = rightNode;
         node->parent = this->root;
         rightNode->parent = this->root;
      }
      node->items[M / 2] = T(); // Remove the median value.
   }
}

/**
  * Split the given node by creating a new node.
  * The median value is put in the position "M / 2" in 'node'.
  */
template <typename T, int M>
typename Common::SortedArray<T, M>::Node* Common::SortedArray<T, M>::split(Node* node, const T& e, Node* child)
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
      if (!eInsertedInRightNode && e > node->items[i])
      {
         rightNode->items[j] = e;
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
         moveChild(node, i+1, rightNode, j+1);
      }
   }

   // Median.
   if (!eInsertedInRightNode && e > node->items[i])
   {
      node->items[i+1] = e;
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
         if (!eInsertedInRightNode && (i == -1 || e > node->items[i]))
         {
            node->items[i+1] = e;
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
