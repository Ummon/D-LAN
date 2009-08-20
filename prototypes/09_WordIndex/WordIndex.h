#ifndef WORDINDEX_H
#define WORDINDEX_H

#include <QtCore/QList>
#include <QtCore/QString>
#include <QtCore/QChar>

template<typename T>
class Node;

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

template<typename T>
class Node
{
public:
   Node();
   Node(const QChar& letter);
   ~Node();
         
   /**
     * Add a child node and return it.
     * If the node already exists it will returned.
     */
   Node<T>& addNode(QChar letter);
   
   void rmNode(Node<T>* const node);
   
   /**
     * Get a children node.
     * /!\ If no one exists 0 is returned.
     */
   Node<T>* getNode(QChar letter);
   
   bool haveChildren();
   
   void addItem(T item);
   
   void rmItem(T item);
   
   /**
     * Return all items from the node and its sub nodes (recursively).
     */
   QList<T> getItems();
   
   bool haveItems();
   
private:
   QChar letter;
   QList<Node<T>*> children;
   QList<T> itemList;
};

#include "WordIndex.cpp"

#endif // WORDINDEX_H
