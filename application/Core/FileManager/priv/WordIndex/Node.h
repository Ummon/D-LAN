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
  
#ifndef FILEMANAGER_NODE_H
#define FILEMANAGER_NODE_H

#include <QList>
#include <QSet>
#include <QString>
#include <QPair>

#include <Common/Uncopyable.h>
#include <Common/Global.h>

namespace FM
{
   template<typename T>
   struct NodeResult
   {
      NodeResult() : level(0) {}
      NodeResult(T* v, bool level = 0) : value(v), level(level) {}
      static void intersect(QSet<NodeResult<T>>& s1, const QSet<NodeResult<T>>& s2, int matchValue);

      T* value; // Should be const but QList must be able to change a NodeResult in place...
      int level;
   };

   /**
     * s1 <- s1 & s2.
     * For all common items the 'level' fields are summed.
     */
   template <typename T>
   void NodeResult<T>::intersect(QSet<NodeResult<T>>& s1, const QSet<NodeResult<T>>& s2, int matchValue)
   {
      for (QMutableSetIterator<NodeResult<T>> i(s1); i.hasNext();)
      {
         const NodeResult<T>& node = i.next();
         typename QSet<NodeResult<T>>::const_iterator j = s2.find(node);
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
      return nr1.level < nr2.level;
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
        * Add an item to the node.
        * If the item already exists (using operator==) nothing is added.
        */
      void addItem(const QString& word, T* item);

      /**
        * Remove the item from the node.
        * If the item doesn'exist nothing happen.
        */
      void rmItem(const QString& word, T* item);

      QList<NodeResult<T>> search(const QString& word, bool alsoFromSubNodes = false, int maxNbResult = -1) const;

   private:
      Node(const QString& part);
      Node(const QString& part, T* item);

      QPair<Node<T>*, int> getNode(const QString& word, bool exactMatch = false) const;

      /**
        * Return all items from the current node and its sub nodes (recursively) if 'alsoFromSubNodes' is true.
        * For all direct sub nodes NodeResult::level is set to 0, for other sub nodes level is set to 1.
        */
      QList<NodeResult<T>> getItems(bool alsoFromSubNodes = false, int maxNbResult = -1) const;

      void remove(int i);

      QString part;
      QList<Node<T>*> children; ///< The children nodes.
      QList<T*> items; ///< The indexed items.
   };

   template <typename T>
   inline bool operator==(const NodeResult<T>& r1, const NodeResult<T>& r2)
   {
      return r1.value == r2.value;
   }

   template <typename T>
   inline uint qHash(const NodeResult<T>& r)
   {
      uint h = 0;
      static const int n = sizeof(T*) > sizeof(uint) ? sizeof(T*) / sizeof(uint) : 1;
      for (int i = 0; i < n; ++i)
         h ^= intptr_t(r.value) >> (i * sizeof(uint));
      return h;
   }
}

/***** Definition *****/
using namespace FM;

template <typename T>
Node<T>::Node()
{
}


template <typename T>
Node<T>::~Node()
{
   for (QListIterator<Node<T>*>i(this->children); i.hasNext();)
      delete i.next();
}

template <typename T>
void Node<T>::addItem(const QString& word, T* item)
{
   if (this->children.isEmpty())
   {
      this->children << new Node(word, item);
   }
   else
   {
      for (int i = 0; i < this->children.size(); ++i)
      {
         const int p = Common::Global::commonPrefix(word, this->children[i]->part);
         if (p != 0)
         {
            if (p == word.size())
            {
                // The word and the sub-part are equal.
               if (p == this->children[i]->part.size())
                  this->children[i]->items << item;
               else // The word is the begining of the the sub-part.
               {
                  Node<T>* newNode = new Node<T>(word, item);
                  this->children[i]->part.remove(0, p);
                  this->children << newNode;
                  newNode->children << this->children[i];
                  this->children.removeAt(i);
               }
            }
            else if (p == this->children[i]->part.size()) // The sub part is the begining of the word.
            {
               this->children[i]->addItem(word.right(word.size() - p), item);
            }
            else
            {
               // The word and the sub part share at least one character from the begining.
               Node<T>* newNodeSplit = new Node<T>(word.left(p));
               this->children[i]->part.remove(0, p);
               this->children << newNodeSplit;
               newNodeSplit->children << this->children[i];
               this->children.removeAt(i);

               Node<T>* newNode = new Node<T>(word.right(word.size() - p), item);
               newNodeSplit->children << newNode;
            }
            return;
         }
      }
      this->children << new Node<T>(word, item);
   }
}

template <typename T>
void Node<T>::rmItem(const QString& word, T* item)
{
   QPair<Node<T>*, int> nodes = this->getNode(word, true);
   if (!nodes.first)
      return;

   Node<T>* node = nodes.first->children[nodes.second];

   if (node->items.size() == 1 && node->items[0] == item)
   {
      node->items.clear();
      nodes.first->remove(nodes.second);
   }
   else
   {
      node->items.removeOne(item);
   }
}

template <typename T>
QList<NodeResult<T>> Node<T>::search(const QString& word, bool alsoFromSubNodes, int maxNbResult) const
{
   QPair<Node<T>*, int> nodes = this->getNode(word, !alsoFromSubNodes);
   if (!nodes.first)
      return QList<NodeResult<T>>();

   return nodes.first->children[nodes.second]->getItems(alsoFromSubNodes, maxNbResult);
}

template <typename T>
Node<T>::Node(const QString& part) :
   part(part)
{
}

template <typename T>
Node<T>::Node(const QString& part, T* item) :
   part(part)
{
   this->items << item;
}

/**
  * Returns the node matching the given word as the 'QPair::second'th child of its parent 'QPair::first'.
  */
template <typename T>
QPair<Node<T>*, int> Node<T>::getNode(const QString& word, bool exactMatch) const
{
   QString part = word;
   Node<T>* currentParent = const_cast<Node<T>*>(this);
   for (int i = 0; i < currentParent->children.size(); ++i)
   {
      int p = Common::Global::commonPrefix(part, currentParent->children[i]->part);
      if (p == part.size() && p == currentParent->children[i]->part.size())
         return qMakePair(currentParent, i);
      if (p == currentParent->children[i]->part.size())
      {
         currentParent = currentParent->children[i];
         part.remove(0, p);
         i = -1;
      }
      else if (p == part.size())
      {
         if (exactMatch)
            break;
         else
            return qMakePair(currentParent, i);
      }
      else if (p != 0)
         break;
   }
   return QPair<Node<T>*, int>(nullptr, 0);
}

template <typename T>
QList<NodeResult<T>> Node<T>::getItems(bool alsoFromSubNodes, int maxNbResult) const
{
   QList<NodeResult<T>> result;
   QList<Node<T>*> nodesToVisit;

   nodesToVisit.append(const_cast<Node<T>*>(this));

   while (!nodesToVisit.empty())
   {
      const Node<T>* current = nodesToVisit.takeFirst();

      for (QListIterator<T*> i(current->items); i.hasNext();)
      {
         // 'level' == 0 means the item matches exactly, it's a bit tricky..
         result << NodeResult<T>(i.next(), current == this ? 0 : 1);
         if (result.size() == maxNbResult)
            return result;
      }

      if (!alsoFromSubNodes)
         break;

      nodesToVisit.append(current->children);
   }

   return result;
}

/**
  * Try to remove the i'th child.
  */
template <typename T>
void Node<T>::remove(int i)
{
   if (i >= this->children.size())
      return;

   Node<T>* nodeToDelete = this->children[i];

   // If the node to delete has nothing to merge we just remove it.
   if (nodeToDelete->children.isEmpty() && nodeToDelete->items.isEmpty())
   {
      this->children.removeAt(i);
      delete nodeToDelete;

      // If we have only one child maybe we can delete it.
      if (this->children.size() == 1)
         this->remove(0);
   }
   // If the parent has no item and only one child (nodeToDelete) then we merge its child and delete it.
   else if (this->items.isEmpty() && this->children.size() == 1)
   {
      this->items << nodeToDelete->items;
      this->children << nodeToDelete->children;
      this->part.append(nodeToDelete->part);

      this->children.removeAt(i);
      nodeToDelete->children.clear();
      delete nodeToDelete;

      // If we have only one child maybe we can delete it.
      if (this->children.size() == 1)
         this->remove(0);
   }
   // If the node to delete has one child and no item we can merge its child.
   else if (nodeToDelete->children.size() == 1 && nodeToDelete->items.isEmpty())
   {
      nodeToDelete->remove(0);
   }
}

#endif
