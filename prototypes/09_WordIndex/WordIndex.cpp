#include "WordIndex.h"

#include <QtCore/QDebug>

template<typename T>
WordIndex<T>::WordIndex()
   : node('\0')
{}

template<typename T>
void WordIndex<T>::addItem(const QList<QString>& words, T item)
{
   foreach (QString word, words)
   {
      Node<T>* currentNode = &this->node;
      foreach (QChar letter, word)
      {
         currentNode = &currentNode->addNode(letter);
      }
      currentNode->addItem(item);
   }
}

template<typename T>
void WordIndex<T>::rmItem(const QList<QString>& words, T item)
{
   foreach (QString word, words)
   {
      QList<Node<T>*> nodes;
      Node<T>* currentNode = &this->node;
      nodes.prepend(currentNode);
      foreach (QChar letter, word)
      {
         if (!(currentNode = currentNode->getNode(letter)))
            goto nextWord;
         nodes.prepend(currentNode);
      }
      
      currentNode->rmItem(item);
      
      if (!currentNode->haveChildren())
      {   
         Node<T>* nodeToRemove = 0;
         foreach (Node<T>* n, nodes)
         {
            if (nodeToRemove)
            {
               n->rmNode(nodeToRemove);
               delete nodeToRemove;
            }
            
            if (n->haveItems() || n->haveChildren())
               break;
            else
               nodeToRemove = n;
         }
      }
      
      nextWord :;      
   }
}

template<typename T>
QList<T> WordIndex<T>::search(QList<QString> words)
{   
   QList<T> result;
   foreach (QString word, words)
   {
      Node<T>* currentNode = &this->node;
      foreach (QChar letter, word)
      {
         if (!(currentNode = currentNode->getNode(letter)))
            goto nextWord;
      }
      result.append(currentNode->getItems());
      nextWord :;
   }
   return result;
}

template<typename T>
Node<T>::Node(const QChar& letter)
   : letter(letter)
{
   qDebug() << "New node : " << letter;
}

template<typename T>
Node<T>::~Node()
{
   qDebug() << "Node deleted : " << this->letter;
   foreach (Node* n, this->children)
      delete n;
}

template<typename T>
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

template<typename T>
void Node<T>::rmNode(Node<T>* const node)
{
   this->children.removeOne(node);
}

template<typename T>
Node<T>* Node<T>::getNode(QChar letter)
{
   foreach (Node* n, this->children)
   {
      if (n->letter == letter)
         return n;
   }
   return 0;
}

template<typename T>
bool Node<T>::haveChildren()
{
   return !this->children.empty();
}

template<typename T>
void Node<T>::addItem(T item)
{
   // Do not add an existing item.
   foreach(T i, this->itemList)
      if (i == item)
         return;
   
   this->itemList.append(item);
}

template<typename T>
void Node<T>::rmItem(T item)
{
   this->itemList.removeOne(item);
}
   
template<typename T>
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

template<typename T>
bool Node<T>::haveItems()
{
   return !this->itemList.empty();      
}
