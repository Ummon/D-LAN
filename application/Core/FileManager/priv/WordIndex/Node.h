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

#include <QList>
#include <QString>
#include <QPair>
#include <QTextBoundaryFinder>
#include <QVarLengthArray>

#include <Common/Uncopyable.h>
#include <Common/StringUtils.h>

/**
  * @class FM::Node
  *
  * Indexed item by string, see the class 'WordIndex' for more explanations.
  */

namespace FM
{
   template<typename T>
   struct NodeResult
   {
      NodeResult() : level(0) {}
      NodeResult(T v, int level = 0) : value(v), level(level) {}

      T value;
      int level;
   };

   /**
     * To sort from the best level (the lowest value) to the worse (the hightest value).
     */
   template <typename T>
   inline bool operator<(const NodeResult<T>& nr1, const NodeResult<T>& nr2)
   {
      return nr1.level < nr2.level;
   }

   template <typename T>
   inline bool operator==(const NodeResult<T>& nr1, const NodeResult<T>& nr2)
   {
      return nr1.value == nr2.value;
   }

   template <typename T>
   inline uint qHash(const NodeResult<T>& nr)
   {
      return qHash(nr.value);
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
      void addItem(const QStringView& word, const T& item);

      /**
        * Remove the item from the node.
        * If the item doesn't exist nothing happen and 'false' is returned else the item is removed and 'true' is returned.
        */
      bool rmItem(const QString& word, const T& item);

      QList<NodeResult<T>> search(
         const QString& word,
         bool alsoFromSubNodes = false,
         int maxNbResult = -1,
         std::function<bool(const T&)> predicat = nullptr
      ) const;

      /**
        * Call 'visitor(item, level)' for each item indexed by 'word' (level 0) then, if 'alsoFromSubNodes' is true,
        * for each item indexed by a word beginning with 'word' (level 1).
        * An item indexed by several of these words is given once per word.
        * The visit stops when 'visitor' returns 'false'.
        */
      template <typename Visitor>
      void visit(const QString& word, bool alsoFromSubNodes, Visitor visitor) const;

      QString toStringDebug() const;

   private:
      Node(const QString& part);
      Node(const QString& part, const T& item);

      QPair<Node<T>*, int> getNode(const QString& word, bool exactMatch = false, bool* fullyMatched = nullptr, int* matchedLength = nullptr) const;

      /**
        * Give to 'visitor' all items from the current node and its sub nodes (recursively) if 'alsoFromSubNodes' is true.
        * Items in this node have level 0 only if the query fully matched it; all other items have level 1.
        * Prefix matches must end at a grapheme boundary in the indexed word.
        */
      template <typename Visitor>
      void visitItems(
         const QString& word,
         int matchedLength,
         bool fullyMatched,
         bool alsoFromSubNodes,
         Visitor& visitor
      ) const;

      static bool isGraphemeBoundary(const QString& word, QStringView continuation);

      void remove(int i);

      QString part;
      QList<Node<T>*> children; ///< The children nodes.
      QList<T> items; ///< The indexed items.
   };
}

template <typename T>
FM::Node<T>::Node()
{
}


template <typename T>
FM::Node<T>::~Node()
{
   for (Node<T>* child : std::as_const(this->children))
      delete child;
}

template <typename T>
void FM::Node<T>::addItem(const QStringView& word, const T& item)
{
   if (this->children.isEmpty())
   {
      this->children << new Node(word.toString(), item);
   }
   else
   {
      for (int i = 0; i < this->children.size(); ++i)
      {
         Node<T>* child = this->children[i];
         const int p = Common::StringUtils::commonPrefix(word, QStringView(child->part));
         if (p != 0)
         {
            if (p == word.size())
            {
                // The word and the sub-part are equal.
               if (p == child->part.size())
               {
                  child->items << item;
               }
               else // The word is the beginning of the the sub-part.
               {
                  Node<T>* newNode = new Node<T>(word.toString(), item);
                  child->part.remove(0, p);
                  this->children.replace(i, newNode);
                  newNode->children << child;
               }
            }
            else if (p == child->part.size()) // The sub part is the beginning of the word.
            {
               child->addItem(word.sliced(p), item);
            }
            else
            {
               // The word and the sub part share at least one character from the beginning.
               Node<T>* newNodeSplit = new Node<T>(word.sliced(0, p).toString());
               child->part.remove(0, p);
               this->children.replace(i, newNodeSplit);
               newNodeSplit->children << child;

               Node<T>* newNode = new Node<T>(word.sliced(p).toString(), item);
               newNodeSplit->children << newNode;
            }
            return;
         }
      }
      this->children << new Node<T>(word.toString(), item);
   }
}

template <typename T>
bool FM::Node<T>::rmItem(const QString& word, const T& item)
{
   QPair<Node<T>*, int> nodes = this->getNode(word, true);
   if (!nodes.first)
      return false;

   Node<T>* node = nodes.first->children[nodes.second];

   if (node->items.size() == 1 && node->items[0] == item)
   {
      node->items.clear();
      nodes.first->remove(nodes.second);
      return true;
   }
   else
   {
      return node->items.removeOne(item);
   }
}

template <typename T>
QList<FM::NodeResult<T>> FM::Node<T>::search(
   const QString& word,
   bool alsoFromSubNodes,
   int maxNbResult,
   std::function<bool(const T&)> predicat
) const
{
   QList<NodeResult<T>> result;
   this->visit(word, alsoFromSubNodes, [&](const T& item, int level) {
      if (predicat && !predicat(item))
         return true;

      result << NodeResult<T>(item, level);
      return result.size() != maxNbResult;
   });
   return result;
}

template <typename T>
template <typename Visitor>
void FM::Node<T>::visit(const QString& word, bool alsoFromSubNodes, Visitor visitor) const
{
   bool fullyMatched = false;
   int matchedLength = 0;
   QPair<Node<T>*, int> nodes = this->getNode(word, !alsoFromSubNodes, &fullyMatched, &matchedLength);
   if (nodes.first)
      nodes.first->children[nodes.second]->visitItems(word, matchedLength, fullyMatched, alsoFromSubNodes, visitor);
}

template <typename T>
QString FM::Node<T>::toStringDebug() const
{
   static const int INDENTATION = 3;
   struct SubNode
   {
      int level;
      const Node<T>* node;
   };

   QString result;
   QList<SubNode> nodesToProcess { SubNode { 0, this } };

   while (!nodesToProcess.isEmpty())
   {
      SubNode current = nodesToProcess.takeFirst();
      result.append(QString().fill(' ', INDENTATION * current.level));
      result.append(current.node->part).append(current.node->items.isEmpty() ? "" : QString(" N = %1").arg(current.node->items.size())).append('\n');

      for (auto child = current.node->children.crbegin(); child != current.node->children.crend(); ++child)
         nodesToProcess.prepend(SubNode { current.level + 1, *child });
   }

   return result;
}

template <typename T>
FM::Node<T>::Node(const QString& part) :
   part(part)
{
}

template <typename T>
FM::Node<T>::Node(const QString& part, const T& item) :
   part(part)
{
   this->items << item;
}

/**
  * Returns the node matching the given word as the 'QPair::second'th child of its parent 'QPair::first'.
  */
template <typename T>
QPair<FM::Node<T>*, int> FM::Node<T>::getNode(const QString& word, bool exactMatch, bool* fullyMatched, int* matchedLength) const
{
   if (fullyMatched)
      *fullyMatched = false;
   QString part = word;
   Node<T>* currentParent = const_cast<Node<T>*>(this);
   for (int i = 0; i < currentParent->children.size(); ++i)
   {
      Node<T>* child = currentParent->children[i];
      int p = Common::StringUtils::commonPrefix(part, child->part);

      if (p != 0)
      {
         if (p == child->part.size())
         {
            if (p == part.size())
            {
               if (fullyMatched)
                  *fullyMatched = true;
               if (matchedLength)
                  *matchedLength = p;
               return qMakePair(currentParent, i);
            }

            currentParent = child;
            part.remove(0, p);
            i = -1;
            continue;
         }
         else if (p == part.size())
         {
            if (exactMatch)
               break;
            else
            {
               if (matchedLength)
                  *matchedLength = p;
               return qMakePair(currentParent, i);
            }
         }
         break;
      }
   }
   return QPair<Node<T>*, int>(nullptr, 0);
}

template <typename T>
template <typename Visitor>
void FM::Node<T>::visitItems(
   const QString& word,
   int matchedLength,
   bool fullyMatched,
   bool alsoFromSubNodes,
   Visitor& visitor
) const
{
   struct Candidate
   {
      const Node<T>* node;
      bool boundaryKnown; ///< 'true' if 'word' ends at a grapheme boundary of the word of the node.
      char16_t highSurrogate; ///< If not null: it ends the word of the parent node, right after 'word'.
   };

   // Depth-first: only the siblings of the visited nodes are kept, not a whole level of the tree.
   QVarLengthArray<Candidate, 64> nodesToVisit { Candidate { this, false, 0 } };

   while (!nodesToVisit.isEmpty())
   {
      Candidate candidate = nodesToVisit.last();
      nodesToVisit.removeLast();
      const Node<T>* current = candidate.node;
      const bool entirelyMatched = fullyMatched && current == this;
      bool matches = true;
      if (!candidate.boundaryKnown && !entirelyMatched)
      {
         // Trie nodes may split combining sequences or surrogate pairs. Keep the full
         // query context until the first following code point is available.
         QString completedPart;
         QStringView continuation = current == this ? QStringView(this->part).sliced(matchedLength) : QStringView(current->part);
         if (candidate.highSurrogate != 0)
         {
            completedPart = QChar(candidate.highSurrogate) + current->part;
            continuation = completedPart;
         }

         matches = isGraphemeBoundary(word, continuation);
         const bool incompleteCodePoint = continuation.size() == 1 && continuation.front().isHighSurrogate();
         if (incompleteCodePoint)
            candidate.highSurrogate = continuation.front().unicode();
         else
         {
            if (!matches)
               continue;
            candidate.boundaryKnown = true;
         }
      }

      if (matches)
         for (const T& item : current->items)
            if (!visitor(item, !entirelyMatched)) // The only call to the visitor: the compiler can inline it.
               return;

      if (!alsoFromSubNodes)
         break;

      // In reverse order: the first child is the next visited node.
      for (auto child = current->children.crbegin(); child != current->children.crend(); ++child)
         nodesToVisit.append(Candidate { *child, candidate.boundaryKnown, candidate.highSurrogate });
   }
}

/**
  * Whether a word made of 'word' followed by 'continuation' can be cut between them.
  */
template <typename T>
bool FM::Node<T>::isGraphemeBoundary(const QString& word, QStringView continuation)
{
   // The characters below U+0300 don't combine with each other, except a line feed following a carriage return.
   if (word.back().unicode() < 0x0300 && continuation.front().unicode() < 0x0300 && continuation.front() != u'\n')
      return true;

   QString text = word;
   text += continuation;
   QTextBoundaryFinder boundaries(QTextBoundaryFinder::Grapheme, text);
   boundaries.setPosition(word.size());
   return boundaries.isAtBoundary();
}

/**
  * Try to remove the i'th child.
  */
template <typename T>
void FM::Node<T>::remove(int i)
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
   // Merge an empty parent with its only child, except for the root: its empty
   // part is the lookup starting point and must never absorb indexed words.
   else if (!this->part.isEmpty() && this->items.isEmpty() && this->children.size() == 1)
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
