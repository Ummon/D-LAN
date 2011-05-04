/**
  * D-LAN - A decentralized LAN file sharing software.
  * Copyright (C) 2010-2011 Greg Burri <greg.burri@gmail.com>
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
  
#ifndef FILEMANAGER_NODE_H
#define FILEMANAGER_NODE_H

#include <QList>
#include <QSet>
#include <QChar>

#include <Common/Uncopyable.h>

namespace FM
{
   template<typename T>
   struct NodeResult
   {
      NodeResult() : level(0) {}
      NodeResult(T* v, bool level = 0) : value(v), level(level) {}
      static void intersect(QSet< NodeResult<T> >& s1, const QSet< NodeResult<T> >& s2, int matchValue);

      T* value; // Should be const but QList must be able to change a NodeResult in place...
      int level;
   };

   /**
     * s1 <- s1 & s2.
     * For all common items the 'level' fields are summed.
     */
   template <typename T>
   void NodeResult<T>::intersect(QSet< NodeResult<T> >& s1, const QSet< NodeResult<T> >& s2, int matchValue)
   {
      for (QMutableSetIterator< NodeResult<T> > i(s1); i.hasNext();)
      {
         const NodeResult<T>& node = i.next();
         typename QSet< NodeResult<T> >::const_iterator j = s2.find(node);
         if (j == s2.constEnd())
            i.remove();
         else
            const_cast<NodeResult<T>&>(node).level += j->level ? matchValue : 0;
      }
   }

   /**
     * To sort from the best level (the lowest value) to the worse (the hightest value).
     */
   template <typename T>
   inline bool operator<(const NodeResult<T>& nr1, const NodeResult<T>& nr2)
   {
      return nr1.level == nr2.level ? *nr1.value > *nr2.value : nr1.level < nr2.level;
   }

   /////

   template<typename T>
   class Node : Common::Uncopyable
   {
   public:
      /**
        * Create a root node.
        */
      Node();

      ~Node();

      /**
        * Add a child node and return it.
        * If the node already exists it will returned.
        */
      Node<T>& addNode(QChar letter);

      /**
        * Remove a node for their children.
        * If the node doesn't exist nothing happen.
        */
      void rmNode(Node<T>* const node);

      /**
        * Get a children node.
        * /!\ If no one exists 0 is returned.
        */
      Node<T>* getNode(QChar letter) const;

      /**
        * Does the node have some children?
        */
      bool haveChildren() const;

      /**
        * Add an item to the node.
        * If the item already exists (using operator==) nothing is added.
        */
      void addItem(T* item);

      /**
        * Remove the item from the node.
        * If the item doesn'exist nothing happen.
        */
      void rmItem(T* item);

      /**
        * Return all items from the current node and its sub nodes (recursively) if 'alsoFromSubNodes' is true.
        */
      QSet< NodeResult<T> > getItems(bool alsoFromSubNodes = false) const;

      /**
        * Does the node own some items?
        */
      bool haveItems() const;

   private:
      Node(const QChar& letter);

      QChar letter; ///< The letter from an indexed word.

      QList<Node<T>*> children; ///< The children nodes.

      QSet< NodeResult<T> > items; ///< The indexed items.
   };

   template <typename T>
   inline bool operator==(const NodeResult<T>& r1, const NodeResult<T>& r2)
   {
      return r1.value == r2.value;
   }

   template <typename T>
   inline uint qHash(const NodeResult<T>& r)
   {
      return reinterpret_cast<uint>(r.value);
   }
}

/***** Definition *****/
using namespace FM;

template <typename T>
Node<T>::Node() :
   letter('\0')
{
}

template <typename T>
Node<T>::~Node()
{
   for (QListIterator<Node<T>*>i(this->children); i.hasNext();)
      delete i.next();
}

template <typename T>
Node<T>& Node<T>::addNode(QChar letter)
{
   // Search if the letter already exists.
   for (int i = 0; i < this->children.size(); i++)
   {
      if (this->children[i]->letter == letter)
         return *this->children[i];
   }
   Node<T>* n = new Node(letter);
   this->children.append(n);
   return *n;
}

template <typename T>
void Node<T>::rmNode(Node<T>* const node)
{
   this->children.removeOne(node);
}

template <typename T>
Node<T>* Node<T>::getNode(QChar letter) const
{
   for (QListIterator<Node<T>*>i(this->children); i.hasNext();)
   {
      Node<T>* node = i.next();
      if (node->letter == letter)
         return node;
   }
   return 0;
}

template <typename T>
bool Node<T>::haveChildren() const
{
   return !this->children.empty();
}

template <typename T>
void Node<T>::addItem(T* item)
{
   // Do not add an existing item.
   if (this->items.contains(NodeResult<T>(item)))
      return;
   this->items << item;
}

template <typename T>
void Node<T>::rmItem(T* item)
{
   this->items.remove(NodeResult<T>(item));
}

template <typename T>
QSet< NodeResult<T> > Node<T>::getItems(bool alsoFromSubNodes) const
{
   if (!alsoFromSubNodes)
      return this->items;

   QSet< NodeResult<T> > result;
   QList<Node<T>*> nodesToVisit;

   nodesToVisit.append(const_cast<Node<T>*>(this));

   while (!nodesToVisit.empty())
   {
      const Node<T>* current = nodesToVisit.takeFirst();
      QSet< NodeResult<T> > currentItems = current->items;

      for (QSetIterator< NodeResult<T> > i(currentItems); i.hasNext();)
         // 'level' == 0 means the item matches exactly, it's a bit tricky..
         const_cast< NodeResult<T>& >(i.next()).level = (current == this ? 0 : 1); // Const cast is valid because 'level' is not used by QSet. See 'uint qHash(const NodeResult<T>& r)'.

      result += currentItems;
      nodesToVisit.append(current->children);
   }

   return result;
}

template <typename T>
bool Node<T>::haveItems() const
{
   return !this->items.empty();
}

template <typename T>
Node<T>::Node(const QChar& letter) :
   letter(letter)
{}

#endif
