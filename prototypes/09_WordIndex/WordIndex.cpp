#include "WordIndex.h"

#include <QtCore/QDebug>

template<typename T>
WordIndex<T>::WordIndex()
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
   // TODO
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
Node<T>::Node()
{}

template<typename T>
Node<T>::Node(QChar letter)
   : letter(letter)
{
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
void Node<T>::addItem(T item)
{
   foreach(T i, this->itemList)
      if (i == item)
         return;
   
   this->itemList.append(item);
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
