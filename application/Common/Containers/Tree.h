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

#include <functional>
#include <algorithm>
#include <memory>

#include <QList>

#include <Common/Uncopyable.h>

namespace Common
{
   template <typename TreeType>
   class TreeBreadthFirstIterator;

   template <typename TreeType>
   class TreeDepthFirstIterator;

   template <typename TreeType>
   class TreePostOrderIterator;

   class OutOfRangeException {};

   /**
     * @class Tree
     * A tree data structure, can store data called 'item' of type 'ItemType'.
     * To use this class you have to inherit it and give your child class type as the second template parameter.
     * For example:
     * MyTree : public Tree<int, MyTree> { .. };
     *
     * Some remarks:
     *  - To remove an element just delete it.
     *  - You can re-implement 'newTree(..)' to dynamically create new type of children.
     *  - This class comes with a breadth-first and a depth-first iterators.
     *
     * If you don't want to inherit from Tree you can use the 'SimpleTree' class.
     *
     * Traversals do not own the nodes or detect invalidation. The traversal root
     * must remain alive until traversal finishes. Item values may be changed
     * during any traversal. Structural changes invalidate traversal unless
     * explicitly permitted by the iterator's documentation below.
     * These rules also apply to callbacks passed to the map functions.
     * Map callbacks are invoked by reference and are not copied; move-only
     * callables are supported. Return false to stop traversal immediately.
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
        * 'fun' may change items, but must not insert, delete, move or sort nodes
        * in the traversed tree if traversal will continue. Children are queued
        * before 'fun' is called; deleting them leaves dangling pointers.
        * @return 'false' if the last call of 'fun' returns 'false'.
        */
      template <typename Fun>
      bool mapBreadthFirst(Fun&& fun, bool iterateOnRoot = false);

      /**
        * Applies 'fun' to all subtrees.
        * Scan the subtrees in a depth first traversal way.
        * Stop the scan if 'fun' returns 'false'.
        * The same mutation restrictions as mapBreadthFirst() apply.
        * @return 'false' if the last call of 'fun' returns 'false'.
        */
      template <typename Fun>
      bool mapDepthFirst(Fun&& fun, bool iterateOnRoot = false);

      /**
        * Applies 'fun' in post-order (children before their parent).
        * Stop the scan if 'fun' returns 'false'.
        * 'fun' may delete the current node (except the traversal root), or
        * modify its descendants, which have already been visited. Newly inserted
        * descendants are not visited. Other structural changes invalidate traversal.
        * @return 'false' if a callback stopped the traversal, otherwise 'true'.
        */
      template <typename Fun>
      bool mapPostOrder(Fun&& fun, bool iterateOnRoot = false);

      virtual TreeType* getParent();
      virtual const TreeType* getParent() const;
      virtual int getNbChildren() const;
      virtual TreeType* getChild(int pos);
      virtual const TreeType* getChild(int pos) const;
      virtual void moveChild(int from, int to);
      virtual TreeType* insertChild(const ItemType& item);
      virtual TreeType* insertChild(const ItemType& item, int pos);
      /**
        * Deletes descendants from the leaves upward without recursive traversal.
        * Each descendant's derived destructor runs with no children, while it is
        * still linked to its parent. Destructors must not change other nodes'
        * structure. The same descendant deletion order applies when deleting a tree;
        * the explicitly deleted tree's derived destructor runs before this cleanup.
        */
      virtual void deleteAllChildren();

      virtual int getOwnPosition() const;
      virtual const ItemType& getItem() const;
      virtual ItemType& getItem();
      virtual void setItem(const ItemType& item);

      TreeType& operator[](int pos);
      const TreeType& operator[](int pos) const;

      template <typename T>
      void sort(T comparator);

   protected:
      virtual TreeType* newTree(const ItemType& item);

      friend class TreeBreadthFirstIterator<TreeType>;
      friend class TreeDepthFirstIterator<TreeType>;
      friend class TreePostOrderIterator<TreeType>;

      ItemType item{};
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

   /**
     * Breadth-first traversal. The root must remain alive. Items may be changed,
     * but structural changes invalidate the iterator: do not call next() again
     * after inserting, deleting, moving or sorting nodes in the traversed tree.
     * next() queues children before returning their parent, so deleting the
     * returned node or clearing its children can leave queued dangling pointers.
     */
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

   /**
     * Pre-order traversal. The same lifetime and mutation restrictions as
     * TreeBreadthFirstIterator apply; next() also queues children before returning.
     */
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

   /**
     * Post-order traversal. The root must remain alive. Items may be changed.
     * After next(), the returned node may be deleted (unless it is the root),
     * or its descendants modified. Those descendants have already been visited;
     * newly inserted descendants will not be visited. The successor is computed
     * before next() returns. Other structural changes invalidate the iterator.
     * Traversal takes O(n) time and O(h) extra space for n nodes of height h,
     * excluding work performed by the caller.
     */
   template <typename TreeType>
   class TreePostOrderIterator
   {
   public:
      TreePostOrderIterator(TreeType* tree, bool iterateOnRoot = false);
      bool hasNext() const;
      TreeType* next();

   private:
      struct Frame
      {
         TreeType* tree;
         qsizetype remainingChildren;
      };

      TreeType* advance();

      bool iterateOnRoot;
      TreeType* const root;
      TreeType* nextTree;
      QList<Frame> stack;
   };
}

template <typename ItemType, typename TreeType>
Common::Tree<ItemType, TreeType>::Tree() :
   parent(nullptr)
{
}

template <typename ItemType, typename TreeType>
Common::Tree<ItemType, TreeType>::Tree(const ItemType& item, TreeType* parent) :
   item(item), parent(parent)
{
}

template <typename ItemType, typename TreeType>
Common::Tree<ItemType, TreeType>::~Tree()
{
   Tree<ItemType, TreeType>::deleteAllChildren();

   if (this->parent)
      this->parent->children.removeOne(static_cast<TreeType*>(this));
}

template <typename ItemType, typename TreeType>
template <typename Fun>
bool Common::Tree<ItemType, TreeType>::mapBreadthFirst(Fun&& fun, bool iterateOnRoot)
{
   TreeBreadthFirstIterator<TreeType> i(static_cast<TreeType*>(this), iterateOnRoot);
   while (TreeType* currentTree = i.next())
      if (!std::invoke(fun, currentTree))
         return false;
   return true;
}

template <typename ItemType, typename TreeType>
template <typename Fun>
bool Common::Tree<ItemType, TreeType>::mapDepthFirst(Fun&& fun, bool iterateOnRoot)
{
   TreeDepthFirstIterator<TreeType> i(static_cast<TreeType*>(this), iterateOnRoot);
   while (TreeType* currentTree = i.next())
      if (!std::invoke(fun, currentTree))
         return false;
   return true;
}

template <typename ItemType, typename TreeType>
template <typename Fun>
bool Common::Tree<ItemType, TreeType>::mapPostOrder(Fun&& fun, bool iterateOnRoot)
{
   TreePostOrderIterator<TreeType> i(static_cast<TreeType*>(this), iterateOnRoot);
   while (TreeType* currentTree = i.next())
      if (!std::invoke(fun, currentTree))
         return false;
   return true;
}

template <typename ItemType, typename TreeType>
TreeType* Common::Tree<ItemType, TreeType>::getParent()
{
   return this->parent;
}

template <typename ItemType, typename TreeType>
const TreeType* Common::Tree<ItemType, TreeType>::getParent() const
{
   return this->parent;
}

template <typename ItemType, typename TreeType>
int Common::Tree<ItemType, TreeType>::getNbChildren() const
{
   return this->children.size();
}

template <typename ItemType, typename TreeType>
TreeType* Common::Tree<ItemType, TreeType>::getChild(int pos)
{
   if (pos < 0 || pos >= this->children.size())
      return nullptr;
   return this->children[pos];
}

template <typename ItemType, typename TreeType>
const TreeType* Common::Tree<ItemType, TreeType>::getChild(int pos) const
{
   if (pos < 0 || pos >= this->children.size())
      return nullptr;
   return this->children[pos];
}

template <typename ItemType, typename TreeType>
void Common::Tree<ItemType, TreeType>::moveChild(int from, int to)
{
   if (from < 0 || to < 0 || from >= this->children.size() || to >= this->children.size())
      return;
   this->children.move(from, to);
}

template <typename ItemType, typename TreeType>
TreeType* Common::Tree<ItemType, TreeType>::insertChild(const ItemType& item)
{
   std::unique_ptr<TreeType> tree(this->newTree(item));
   this->children.append(tree.get());
   return tree.release();
}

/**
  * Insert an item into the tree at the position 'pos'. Negative positions are
  * clamped to the beginning; positions exceeding the children size to the end.
  * @return The new created subtree.
  */
template <typename ItemType, typename TreeType>
TreeType* Common::Tree<ItemType, TreeType>::insertChild(const ItemType& item, int pos)
{
   if (pos < 0)
      pos = 0;
   else if (pos > this->children.size())
      pos = this->children.size();

   std::unique_ptr<TreeType> tree(this->newTree(item));
   this->children.insert(pos, tree.get());
   return tree.release();
}

template <typename ItemType, typename TreeType>
void Common::Tree<ItemType, TreeType>::deleteAllChildren()
{
   // Walk down to leaves and back through parent links. Deleting only leaves
   // keeps nested base-destructor calls bounded, regardless of tree depth.
   Tree<ItemType, TreeType>* current = this;
   while (true)
   {
      if (!current->children.isEmpty())
      {
         current = current->children.first();
      }
      else if (current == this)
      {
         return;
      }
      else
      {
         Tree<ItemType, TreeType>* parent = current->parent;
         // The destructor removes the leaf from its parent after the derived
         // destructor has had a chance to inspect the parent relationship.
         delete static_cast<TreeType*>(current);
         current = parent;
      }
   }
}

/**
  * O(n).
  */
template <typename ItemType, typename TreeType>
int Common::Tree<ItemType, TreeType>::getOwnPosition() const
{
   if (this->parent)
      return this->parent->children.indexOf(const_cast<TreeType*>(static_cast<const TreeType*>(this)));

   return 0;
}

template <typename ItemType, typename TreeType>
const ItemType& Common::Tree<ItemType, TreeType>::getItem() const
{
   return this->item;
}

template <typename ItemType, typename TreeType>
ItemType& Common::Tree<ItemType, TreeType>::getItem()
{
   return this->item;
}

template <typename ItemType, typename TreeType>
void Common::Tree<ItemType, TreeType>::setItem(const ItemType& item)
{
   this->item = item;
}

/**
  * @exception OutOfRangeException
  */
template <typename ItemType, typename TreeType>
TreeType& Common::Tree<ItemType, TreeType>::operator[](int pos)
{
   if (pos < 0 || pos >= this->children.size())
      throw OutOfRangeException();
   return *this->children[pos];
}

/**
  * @exception OutOfRangeException
  */
template <typename ItemType, typename TreeType>
const TreeType& Common::Tree<ItemType, TreeType>::operator[](int pos) const
{
   if (pos < 0 || pos >= this->children.size())
      throw OutOfRangeException();
   return *this->children[pos];
}

template <typename ItemType, typename TreeType>
TreeType* Common::Tree<ItemType, TreeType>::newTree(const ItemType& item)
{
   return new TreeType(item, static_cast<TreeType*>(this));
}

template <typename ItemType, typename TreeType>
template <typename T>
void Common::Tree<ItemType, TreeType>::sort(T comparator)
{
   std::sort(this->children.begin(), this->children.end(), comparator);
}

/////

template <typename TreeType>
Common::TreeBreadthFirstIterator<TreeType>::TreeBreadthFirstIterator(TreeType* tree, bool iterateOnRoot)
{
   if (iterateOnRoot)
      this->nextTrees << tree;
   else
      this->readChildren(tree);
}

template <typename TreeType>
bool Common::TreeBreadthFirstIterator<TreeType>::hasNext() const
{
   return !this->nextTrees.isEmpty();
}

template <typename TreeType>
TreeType* Common::TreeBreadthFirstIterator<TreeType>::TreeBreadthFirstIterator::next()
{
   if (this->nextTrees.isEmpty())
      return 0;

   TreeType* tree = this->nextTrees.takeFirst();
   this->readChildren(tree);
   return tree;
}

template <typename TreeType>
void Common::TreeBreadthFirstIterator<TreeType>::readChildren(TreeType* parentTree)
{
   this->nextTrees.append(parentTree->children);
}

/////

template <typename TreeType>
Common::TreeDepthFirstIterator<TreeType>::TreeDepthFirstIterator(TreeType* tree, bool iterateOnRoot)
{
   if (iterateOnRoot)
      this->nextTrees << tree;
   else
      this->readChildren(tree);
}

template <typename TreeType>
bool Common::TreeDepthFirstIterator<TreeType>::hasNext() const
{
   return !this->nextTrees.isEmpty();
}

template <typename TreeType>
TreeType* Common::TreeDepthFirstIterator<TreeType>::TreeDepthFirstIterator::next()
{
   if (this->nextTrees.isEmpty())
      return 0;

   TreeType* tree = this->nextTrees.takeFirst();
   this->readChildren(tree);
   return tree;
}

template <typename TreeType>
void Common::TreeDepthFirstIterator<TreeType>::readChildren(TreeType* parentTree)
{
   QListIterator<TreeType*> i(parentTree->children);
   i.toBack();
   while (i.hasPrevious())
      this->nextTrees.prepend(i.previous());
}

/////

template <typename TreeType>
Common::TreePostOrderIterator<TreeType>::TreePostOrderIterator(TreeType* tree, bool iterateOnRoot) :
   iterateOnRoot(iterateOnRoot),
   root(tree),
   nextTree(nullptr)
{
   this->stack.append(Frame { tree, tree->children.size() });
   this->nextTree = this->advance();
}

template <typename TreeType>
bool Common::TreePostOrderIterator<TreeType>::hasNext() const
{
   return this->nextTree != nullptr;
}

template <typename TreeType>
TreeType* Common::TreePostOrderIterator<TreeType>::next()
{
   TreeType* nextTreeCopy = this->nextTree;

   // Find the successor before returning: callers may delete the returned node
   // or change its already-visited descendants.
   this->nextTree = this->advance();

   return nextTreeCopy;
}

template <typename TreeType>
TreeType* Common::TreePostOrderIterator<TreeType>::advance()
{
   while (!this->stack.isEmpty())
   {
      Frame& frame = this->stack.last();
      if (frame.remainingChildren > 0)
      {
         // Unvisited children form a suffix. Counting from the end keeps the
         // next position valid when callers delete an already-visited sibling.
         TreeType* child = frame.tree->children.at(frame.tree->children.size() - frame.remainingChildren);
         --frame.remainingChildren;
         this->stack.append(Frame { child, child->children.size() });
      }
      else
      {
         TreeType* tree = frame.tree;
         this->stack.removeLast();
         if (tree != this->root || this->iterateOnRoot)
            return tree;
      }
   }

   return nullptr;
}
