#ifndef FILEMANAGER_NODE_H
#define FILEMANAGER_NODE_H

#include <QtCore/QList>
#include <QtCore/QSet>
#include <QtCore/QChar>

namespace FM
{
   template<typename T>
   class Node
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
      Node<T>* getNode(QChar letter);

      /**
        * Does the node have some children?
        */
      bool haveChildren();

      /**
        * Add an item to the node.
        * If the item already exists (using operator==) nothing is added.
        */
      void addItem(T item);

      /**
        * Remove the item from the node.
        * If the item doesn'exist nothing happen.
        */
      void rmItem(T item);

      /**
        * Return all items from the current node and its sub nodes (recursively).
        */
      QSet<T> getItems();

      /**
        * Does the node own some items?
        */
      bool haveItems();

   private:
      Node(const QChar& letter);

      QChar letter; ///< The letter from an indexed word.

      QList<Node<T>*> children; ///< The children nodes.

      QSet<T> itemList; ///< The indexed items.
   };
}

/***** Definition *****/
using namespace FM;

template <typename T>
Node<T>::Node()
   : letter('\0')
{
}

template <typename T>
Node<T>::~Node()
{
   foreach (Node* n, this->children)
      delete n;
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
Node<T>* Node<T>::getNode(QChar letter)
{
   foreach (Node* n, this->children)
   {
      if (n->letter == letter)
         return n;
   }
   return 0;
}

template <typename T>
bool Node<T>::haveChildren()
{
   return !this->children.empty();
}

template <typename T>
void Node<T>::addItem(T item)
{
   // Do not add an existing item.
   if (this->itemList.contains(item))
      return;
   this->itemList << item;
}

template <typename T>
void Node<T>::rmItem(T item)
{
   this->itemList.remove(item);
}

template <typename T>
QSet<T> Node<T>::getItems()
{
   QSet<T> result;
   QList<Node<T>*> nodesToVisit;

   nodesToVisit.append(this);
   while (!nodesToVisit.empty())
   {
      Node<T>* current = nodesToVisit.takeFirst();
      result += current->itemList;
      nodesToVisit.append(current->children);
   }

   return result;
}

template <typename T>
bool Node<T>::haveItems()
{
   return !this->itemList.empty();
}

template <typename T>
Node<T>::Node(const QChar& letter)
   : letter(letter)
{}

#endif
