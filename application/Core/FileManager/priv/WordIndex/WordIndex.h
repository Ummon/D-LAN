#ifndef FILEMANAGER_WORDINDEX_H
#define FILEMANAGER_WORDINDEX_H

#include <QDebug>
#include <QList>
#include <QString>
#include <QChar>

#include <priv/WordIndex/Node.h>

namespace FileManager
{
   /**
     * An collection of T indexed by word.
     */
   template<typename T>
   class WordIndex
   {
   public:
      WordIndex();

      void addItem(const QList<QString>& words, T item);
      void rmItem(const QList<QString>& words, T item);
      QList<T> search(QList<QString> words);

   private:
      Node<T> node;
   };
}

/***** Definition *****/
using namespace FileManager;

template<typename T>
WordIndex<T>::WordIndex()
{}

template<typename T>
void WordIndex<T>::addItem(const QList<QString>& words, T item)
{
   QListIterator<QString> i(words);
   while (i.hasNext())
   {
      Node<T>* currentNode = &this->node;
      foreach (QChar letter, i.next())
      {
         currentNode = &currentNode->addNode(letter);
      }
      currentNode->addItem(item);
   }
}

template<typename T>
void WordIndex<T>::rmItem(const QList<QString>& words, T item)
{
   QListIterator<QString> i(words);
   while (i.hasNext())
   {
      QList<Node<T>*> nodes;
      Node<T>* currentNode = &this->node;
      nodes.prepend(currentNode);
      foreach (QChar letter, i.next())
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

#endif // WORDINDEX_H
