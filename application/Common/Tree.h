#ifndef COMMON_TREE_H
#define COMMON_TREE_H

#include <QList>

namespace Common
{
   template <typename T>
   class TreeBreadthIterator;

   template<typename T>
   class Tree
   {
   public:
      Tree();
      Tree(const T&, Tree* parent);
      virtual ~Tree();

      virtual Tree<T>* getParent();
      virtual int getNbChildren() const;
      virtual Tree<T>* getChild(int pos) const;
      virtual void moveChild(int from, int to);
      virtual Tree<T>* insertChild(const T& item);
      virtual Tree<T>* insertChild(const T& item, int pos);

      virtual int getOwnPosition() const;
      virtual const T& getItem() const;
      virtual T& getItem();
      virtual void setItem(const T& item);

   protected:
      virtual Tree<T>* newTree(const T& item);

      friend class TreeBreadthIterator<T>;

      T item;
      Tree<T>* parent;
      QList<Tree<T>*> children;
   };

   template <typename T>
   class TreeBreadthIterator
   {
   public:
      TreeBreadthIterator(Tree<T>* tree, bool iterateOnRoot = false);
      bool hasNext() const;
      Tree<T>* next();

   private:
      void readChildren(Tree<T>* parentTree);
      QList<Tree<T>*> nextTrees;
   };
}

using namespace Common;

template <typename T>
Tree<T>::Tree() :
   parent(0)
{
}

template <typename T>
Tree<T>::Tree(const T& item, Tree* parent) :
   item(item), parent(parent)
{
}

template <typename T>
Tree<T>::~Tree()
{
   for (QListIterator<Tree*> i(this->children); i.hasNext();)
      delete i.next();

   if (this->parent)
      this->parent->children.removeOne(this);
}

template <typename T>
Tree<T>* Tree<T>::getParent()
{
   return this->parent;
}

template <typename T>
int Tree<T>::getNbChildren() const
{
   return this->children.size();
}

template <typename T>
Tree<T>* Tree<T>::getChild(int pos) const
{
   if (pos >= this->children.size())
      return 0;
   return this->children[pos];
}

template <typename T>
void Tree<T>::moveChild(int from, int to)
{
   if (from >= this->children.size() || to >= this->children.size())
      return;
   this->children.move(from, to);
}

template <typename T>
Tree<T>* Tree<T>::insertChild(const T& item)
{
   this->children << this->newTree(item);
   return this->children.last();
}

template <typename T>
Tree<T>* Tree<T>::insertChild(const T& item, int pos)
{
   if (pos > this->children.size())
      pos = this->children.size();

   Tree<T>* tree = this->newTree(item);
   this->children.insert(pos, tree);
   return tree;
}

/**
  * O(n).
  */
template <typename T>
int Tree<T>::getOwnPosition() const
{
   if (this->parent)
      return this->parent->children.indexOf(const_cast<Tree<T>*>(this));

   return 0;
}

template <typename T>
const T& Tree<T>::getItem() const
{
   return this->item;
}

template <typename T>
T& Tree<T>::getItem()
{
   return this->item;
}

template <typename T>
void Tree<T>::setItem(const T& item)
{
   this->item = item;
}

template <typename T>
Tree<T>* Tree<T>::newTree(const T& item)
{
   return new Tree(item, this);
}

template <typename T>
TreeBreadthIterator<T>::TreeBreadthIterator(Tree<T>* tree, bool iterateOnRoot)
{
   if (iterateOnRoot)
      this->nextTrees << tree;
   this->readChildren(tree);
}

template <typename T>
bool TreeBreadthIterator<T>::hasNext() const
{
   return !this->nextTrees.isEmpty();
}

template <typename T>
Tree<T>* TreeBreadthIterator<T>::TreeBreadthIterator::next()
{
   if (this->nextTrees.isEmpty())
      return 0;

   Tree<T>* tree = this->nextTrees.takeFirst();
   this->readChildren(tree);
   return tree;
}

template <typename T>
void TreeBreadthIterator<T>::readChildren(Tree<T>* parentTree)
{
   this->nextTrees.append(parentTree->children);
}

#endif
