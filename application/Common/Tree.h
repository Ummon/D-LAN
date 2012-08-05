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
  
#ifndef COMMON_TREE_H
#define COMMON_TREE_H

#include <QList>

#include <Common/Uncopyable.h>

namespace Common
{
   template <typename TreeType>
   class TreeBreadthFirstIterator;

   template <typename TreeType>
   class TreeDepthFirstIterator;

   template <typename TreeType>
   class TreeReverseDepthFirstIterator;

   class OutOfRangeException {};

   /**
     * @class Tree
     * A tree data structure, can store data called 'item' of type 'ItemType'.
     * To use this classe you have to inherit it and give your child class type as the second template parameter.
     * For example:
     * MyTree : public Tree<int, MyTree> { .. };
     *
     * Some remarks:
     *  - To remove an element just delete it.
     *  - You can reimplement 'newTree(..)' to dynamically create new type of children.
     *  - This class comes with a breadth-first and a depth-first iterators.
     *
     * If you don't want to inherit from Tree you can use the 'SimpleTree' class.
     *
     * @remarks No copy constructor neither no operator assignment are defined for the moment.
     */

   template<typename ItemType, typename TreeType>
   class Tree : Uncopyable
   {
   public:
      Tree();
      Tree(const ItemType&, TreeType* parent);
      virtual ~Tree();

      /**
        * Applies 'fun' to all subtrees.
        * Scan the subtrees in a breadth first traversal way.
        * Stop the scan if 'fun' returns 'false'.
        * @return 'false' if the last call of 'fun' returns 'false'.
        */
      bool mapBreadthFirst(std::function<bool(TreeType*)> fun, bool iterateOnRoot = false);

      /**
        * Applies 'fun' to all subtrees.
        * Scan the subtrees in a depth first traversal way.
        * Stop the scan if 'fun' returns 'false'.
        * @return 'false' if the last call of 'fun' returns 'false'.
        */
      bool mapDepthFirst(std::function<bool(TreeType*)> fun, bool iterateOnRoot = false);

      bool mapReverseDepthFirst(std::function<bool(TreeType*)> fun, bool iterateOnRoot = false);

      virtual TreeType* getParent();
      virtual const TreeType* getParent() const;
      virtual int getNbChildren() const;
      virtual TreeType* getChild(int pos) const;
      virtual void moveChild(int from, int to);
      virtual TreeType* insertChild(const ItemType& item);
      virtual TreeType* insertChild(const ItemType& item, int pos);
      virtual void deleteAllChildren();

      virtual int getOwnPosition() const;
      virtual const ItemType& getItem() const;
      virtual ItemType& getItem();
      virtual void setItem(const ItemType& item);

      TreeType& operator[](int pos);
      const TreeType& operator[](int pos) const;

   protected:
      virtual TreeType* newTree(const ItemType& item);

      friend class TreeBreadthFirstIterator<TreeType>;
      friend class TreeDepthFirstIterator<TreeType>;
      friend class TreeReverseDepthFirstIterator<TreeType>;

      ItemType item;
      TreeType* parent;
      QList<TreeType*> children;
   };

   /////

   template <typename T>
   class SimpleTree : public Tree<T, SimpleTree<T>>
   {
   public:
      SimpleTree() {}
      SimpleTree(const T& item, SimpleTree<T>* parent) : Tree<T, SimpleTree<T>>(item, parent) {}
   };

   /////

   template <typename TreeType>
   class TreeBreadthFirstIterator
   {
   public:
      TreeBreadthFirstIterator(TreeType* tree, bool iterateOnRoot = false);
      bool hasNext() const;
      TreeType* next();

   private:
      void readChildren(TreeType* parentTree);
      QList<TreeType*> nextTrees;
   };

   /////

   template <typename TreeType>
   class TreeDepthFirstIterator
   {
   public:
      TreeDepthFirstIterator(TreeType* tree, bool iterateOnRoot = false);
      bool hasNext() const;
      TreeType* next();

   private:
      void readChildren(TreeType* parentTree);
      QList<TreeType*> nextTrees;
   };

   /////

   template <typename TreeType>
   class TreeReverseDepthFirstIterator
   {
   public:
      TreeReverseDepthFirstIterator(TreeType* tree, bool iterateOnRoot = false);
      bool hasNext() const;
      TreeType* next();

   private:
      TreeType* getDeepestTree(TreeType* tree, int subTreePosition = -1);
      static int parentPosition(TreeType* tree);

      bool iterateOnRoot;
      TreeType* const root;
      TreeType* nextTree;
   };
}

using namespace Common;

template <typename ItemType, typename TreeType>
Tree<ItemType, TreeType>::Tree() :
   parent(0)
{
}

template <typename ItemType, typename TreeType>
Tree<ItemType, TreeType>::Tree(const ItemType& item, TreeType* parent) :
   item(item), parent(parent)
{
}

template <typename ItemType, typename TreeType>
Tree<ItemType, TreeType>::~Tree()
{
   for (QListIterator<TreeType*> i(this->children); i.hasNext();)
      delete i.next();

   if (this->parent)
      this->parent->children.removeOne(static_cast<TreeType*>(this));
}

template <typename ItemType, typename TreeType>
bool Tree<ItemType, TreeType>::mapBreadthFirst(std::function<bool(TreeType*)> fun, bool iterateOnRoot)
{
   TreeBreadthFirstIterator<TreeType> i(static_cast<TreeType*>(this), iterateOnRoot);
   TreeType* currentTree;
   while (currentTree = i.next())
      if (!fun(currentTree))
         return false;
   return true;
}

template <typename ItemType, typename TreeType>
bool Tree<ItemType, TreeType>::mapDepthFirst(std::function<bool(TreeType*)> fun, bool iterateOnRoot)
{
   TreeDepthFirstIterator<TreeType> i(static_cast<TreeType*>(this), iterateOnRoot);
   TreeType* currentTree;
   while (currentTree = i.next())
      if (!fun(currentTree))
         return false;
   return true;
}

template <typename ItemType, typename TreeType>
bool Tree<ItemType, TreeType>::mapReverseDepthFirst(std::function<bool(TreeType*)> fun, bool iterateOnRoot)
{
   TreeReverseDepthFirstIterator<TreeType> i(static_cast<TreeType*>(this), iterateOnRoot);
   TreeType* currentTree;
   while (currentTree = i.next())
      if (!fun(currentTree))
         return false;
   return true;
}

template <typename ItemType, typename TreeType>
TreeType* Tree<ItemType, TreeType>::getParent()
{
   return this->parent;
}

template <typename ItemType, typename TreeType>
const TreeType* Tree<ItemType, TreeType>::getParent() const
{
   return this->parent;
}

template <typename ItemType, typename TreeType>
int Tree<ItemType, TreeType>::getNbChildren() const
{
   return this->children.size();
}

template <typename ItemType, typename TreeType>
TreeType* Tree<ItemType, TreeType>::getChild(int pos) const
{
   if (pos >= this->children.size())
      return 0;
   return this->children[pos];
}

template <typename ItemType, typename TreeType>
void Tree<ItemType, TreeType>::moveChild(int from, int to)
{
   if (from >= this->children.size() || to >= this->children.size())
      return;
   this->children.move(from, to);
}

template <typename ItemType, typename TreeType>
TreeType* Tree<ItemType, TreeType>::insertChild(const ItemType& item)
{
   this->children << this->newTree(item);
   return this->children.last();
}

/**
  * Insert an item into the tree at the position 'pos', if the position exceed
  * the children size the new item will be put at the end.
  * @return The new created subtree.
  */
template <typename ItemType, typename TreeType>
TreeType* Tree<ItemType, TreeType>::insertChild(const ItemType& item, int pos)
{
   if (pos > this->children.size())
      pos = this->children.size();

   TreeType* tree = this->newTree(item);
   this->children.insert(pos, tree);
   return tree;
}

template <typename ItemType, typename TreeType>
void Tree<ItemType, TreeType>::deleteAllChildren()
{
   for (QListIterator<TreeType*> i(this->children); i.hasNext();)
      delete i.next();
   this->children.clear();
}

/**
  * O(n).
  */
template <typename ItemType, typename TreeType>
int Tree<ItemType, TreeType>::getOwnPosition() const
{
   if (this->parent)
      return this->parent->children.indexOf(const_cast<TreeType*>(static_cast<const TreeType*>(this)));

   return 0;
}

template <typename ItemType, typename TreeType>
const ItemType& Tree<ItemType, TreeType>::getItem() const
{
   return this->item;
}

template <typename ItemType, typename TreeType>
ItemType& Tree<ItemType, TreeType>::getItem()
{
   return this->item;
}

template <typename ItemType, typename TreeType>
void Tree<ItemType, TreeType>::setItem(const ItemType& item)
{
   this->item = item;
}

/**
  * @exception OutOfRangeException
  */
template <typename ItemType, typename TreeType>
TreeType& Tree<ItemType, TreeType>::operator[](int pos)
{
   if (pos >= this->children.size())
      throw OutOfRangeException();
   return *this->children[pos];
}

/**
  * @exception OutOfRangeException
  */
template <typename ItemType, typename TreeType>
const TreeType& Tree<ItemType, TreeType>::operator[](int pos) const
{
   if (pos >= this->children.size())
      throw OutOfRangeException();
   return *this->children[pos];
}

template <typename ItemType, typename TreeType>
TreeType* Tree<ItemType, TreeType>::newTree(const ItemType& item)
{
   return new TreeType(item, static_cast<TreeType*>(this));
}

/////

template <typename TreeType>
TreeBreadthFirstIterator<TreeType>::TreeBreadthFirstIterator(TreeType* tree, bool iterateOnRoot)
{
   if (iterateOnRoot)
      this->nextTrees << tree;
   else
      this->readChildren(tree);
}

template <typename TreeType>
bool TreeBreadthFirstIterator<TreeType>::hasNext() const
{
   return !this->nextTrees.isEmpty();
}

template <typename TreeType>
TreeType* TreeBreadthFirstIterator<TreeType>::TreeBreadthFirstIterator::next()
{
   if (this->nextTrees.isEmpty())
      return 0;

   TreeType* tree = this->nextTrees.takeFirst();
   this->readChildren(tree);
   return tree;
}

template <typename TreeType>
void TreeBreadthFirstIterator<TreeType>::readChildren(TreeType* parentTree)
{
   this->nextTrees.append(parentTree->children);
}

/////

template <typename TreeType>
TreeDepthFirstIterator<TreeType>::TreeDepthFirstIterator(TreeType* tree, bool iterateOnRoot)
{
   if (iterateOnRoot)
      this->nextTrees << tree;
   else
      this->readChildren(tree);
}

template <typename TreeType>
bool TreeDepthFirstIterator<TreeType>::hasNext() const
{
   return !this->nextTrees.isEmpty();
}

template <typename TreeType>
TreeType* TreeDepthFirstIterator<TreeType>::TreeDepthFirstIterator::next()
{
   if (this->nextTrees.isEmpty())
      return 0;

   TreeType* tree = this->nextTrees.takeFirst();
   this->readChildren(tree);
   return tree;
}

template <typename TreeType>
void TreeDepthFirstIterator<TreeType>::readChildren(TreeType* parentTree)
{
   QListIterator<TreeType*> i(parentTree->children);
   i.toBack();
   while (i.hasPrevious())
      this->nextTrees.prepend(i.previous());
}

/////

template <typename TreeType>
TreeReverseDepthFirstIterator<TreeType>::TreeReverseDepthFirstIterator(TreeType* tree, bool iterateOnRoot) :
   iterateOnRoot(iterateOnRoot),
   root(tree),
   nextTree(nullptr)
{
   this->nextTree = this->getDeepestTree(tree);
}

template <typename TreeType>
bool TreeReverseDepthFirstIterator<TreeType>::hasNext() const
{
   return this->nextTree != nullptr;
}

template <typename TreeType>
TreeType* TreeReverseDepthFirstIterator<TreeType>::next()
{
   TreeType* nextTreeCopy = this->nextTree;

   this->nextTree = this->nextTree && this->nextTree != this->root ? this->getDeepestTree(this->nextTree->parent, parentPosition(this->nextTree)) : nullptr;

   return nextTreeCopy;
}

template <typename TreeType>
TreeType* TreeReverseDepthFirstIterator<TreeType>::getDeepestTree(TreeType* tree, int subTreePosition)
{
   if (tree->children.size() > ++subTreePosition)
      return getDeepestTree(tree->children[subTreePosition]);

   if (tree == this->root && !this->iterateOnRoot)
      return nullptr;

   return tree;
}

template <typename TreeType>
int TreeReverseDepthFirstIterator<TreeType>::parentPosition(TreeType* tree)
{
   if (!tree->parent)
      return -1;

   return tree->parent->children.indexOf(tree);
}

#endif
