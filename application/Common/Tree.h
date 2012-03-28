#ifndef COMMON_TREE_H
#define COMMON_TREE_H

#include <QList>

namespace Common
{
   template <typename TreeType>
   class TreeBreadthIterator;

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
     *  - You can reimplement 'newwTree(..)' to dynamically create new type of children.
     *  - This class comes with a breadth-first iterator.
     *
     * If you don't want to inherit from Tree you can use the SimpleTree class.
     */

   template<typename ItemType, typename TreeType>
   class Tree
   {
   public:
      Tree();
      Tree(const ItemType&, TreeType* parent);
      virtual ~Tree();

      virtual TreeType* getParent();
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

      friend class TreeBreadthIterator<TreeType>;

      ItemType item;
      TreeType* parent;
      QList<TreeType*> children;
   };

   template <typename T>
   class SimpleTree : public Tree< T, SimpleTree<T> >
   {
   public:
      SimpleTree() {}
      SimpleTree(const T& item, SimpleTree<T>* parent) : Tree< T, SimpleTree<T> >(item, parent) {}
   };

   template <typename TreeType>
   class TreeBreadthIterator
   {
   public:
      TreeBreadthIterator(TreeType* tree, bool iterateOnRoot = false);
      bool hasNext() const;
      TreeType* next();

   private:
      void readChildren(TreeType* parentTree);
      QList<TreeType*> nextTrees;
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
TreeType* Tree<ItemType, TreeType>::getParent()
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
TreeBreadthIterator<TreeType>::TreeBreadthIterator(TreeType* tree, bool iterateOnRoot)
{
   if (iterateOnRoot)
      this->nextTrees << tree;
   else
      this->readChildren(tree);
}

template <typename TreeType>
bool TreeBreadthIterator<TreeType>::hasNext() const
{
   return !this->nextTrees.isEmpty();
}

template <typename TreeType>
TreeType* TreeBreadthIterator<TreeType>::TreeBreadthIterator::next()
{
   if (this->nextTrees.isEmpty())
      return 0;

   TreeType* tree = this->nextTrees.takeFirst();
   this->readChildren(tree);
   return tree;
}

template <typename TreeType>
void TreeBreadthIterator<TreeType>::readChildren(TreeType* parentTree)
{
   this->nextTrees.append(parentTree->children);
}

#endif
