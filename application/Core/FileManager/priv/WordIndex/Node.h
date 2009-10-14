#ifndef NODE_H
#define NODE_H

#include <QtCore/QDebug>
#include <QtCore/QList>
#include <QtCore/QChar>

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
   QList<T> getItems();
   
   /**
     * Does the node own some items?
     */
   bool haveItems();
   
private:
   Node(const QChar& letter);
   
   QChar letter; ///< The letter from an indexed word.
   
   QList<Node<T>*> children; ///< The children nodes.
   
   QList<T> itemList; ///< The indexed items.
};

/***** Definition *****/

template <typename T>
Node<T>::Node()
   : letter('\0')
{
   qDebug() << "New root node";
}

template <typename T>
Node<T>::~Node()
{
   qDebug() << "Node deleted : " << this->letter;
   foreach (Node* n, this->children)
      delete n;
}

template <typename T>
Node<T>& Node<T>::addNode(QChar letter)
{
   // Search if the letter already exists.
   foreach (Node* n, this->children)
   {      
      if (n->letter == letter)
         return *n;
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
   foreach(T i, this->itemList)
      if (i == item)
         return;
   
   this->itemList.append(item);
}

template <typename T>
void Node<T>::rmItem(T item)
{
   this->itemList.removeOne(item);
}
   
template <typename T>
QList<T> Node<T>::getItems()
{
   QList<T> result;
   QList<Node<T>*> nodesToVisit;
   
   nodesToVisit.append(this);
   while (!nodesToVisit.empty())
   {
      Node<T>* current = nodesToVisit.takeFirst();
      result.append(current->itemList);
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
{
   qDebug() << "New node : " << letter;
}

#endif // NODE_H
