#ifndef FILEMANAGER_WORDINDEX_H
#define FILEMANAGER_WORDINDEX_H

#include <QList>
#include <QSet>
#include <QString>
#include <QChar>
#include <QMutex>

#include <priv/WordIndex/Node.h>

namespace FM
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
      QSet<T> search(QList<QString> words);
      QSet<T> search(const QString& word);

   private:
      Node<T> node;
      QMutex mutex;
   };
}

/***** Definitions *****/
using namespace FM;

template<typename T>
WordIndex<T>::WordIndex()
{}

template<typename T>
void WordIndex<T>::addItem(const QList<QString>& words, T item)
{
   QMutexLocker lock(&mutex);

   for (QListIterator<QString> i(words); i.hasNext();)
   {
      const QString& word = i.next();
      Node<T>* currentNode = &this->node;
      for (int j = 0; j < word.size(); j++)
         currentNode = &currentNode->addNode(word[j]);
      currentNode->addItem(item);
   }
}

template<typename T>
void WordIndex<T>::rmItem(const QList<QString>& words, T item)
{
   QMutexLocker lock(&mutex);

   for (QListIterator<QString> i(words); i.hasNext();)
   {
      const QString& word = i.next();
      QList<Node<T>*> nodes;
      Node<T>* currentNode = &this->node;
      nodes.prepend(currentNode);
      for (int j = 0; j < word.size(); j++)
      {
         if (!(currentNode = currentNode->getNode(word[j])))
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
QSet<T> WordIndex<T>::search(QList<QString> words)
{
   QMutexLocker lock(&mutex);

   QSet<T> result;
   foreach (QString word, words)
   {
      Node<T>* currentNode = &this->node;
      foreach (QChar letter, word)
      {
         if (!(currentNode = currentNode->getNode(letter)))
            goto nextWord;
      }
      result += currentNode->getItems();
      nextWord :;
   }
   return result;
}

template<typename T>
QSet<T> WordIndex<T>::search(const QString& word)
{
   QMutexLocker lock(&mutex);

   Node<T>* currentNode = &this->node;

   for (int i = 0; i < word.size(); i++)
      if (!(currentNode = currentNode->getNode(word[i])))
         return QSet<T>();

   return currentNode->getItems();
}

#endif
