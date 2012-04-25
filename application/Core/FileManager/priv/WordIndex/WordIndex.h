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
  
#ifndef FILEMANAGER_WORDINDEX_H
#define FILEMANAGER_WORDINDEX_H

#include <QList>
#include <QString>
#include <QChar>
#include <QMutex>

#include <Common/Uncopyable.h>

#include <priv/WordIndex/Node.h>

namespace FM
{
   /**
     * A thread safe collection of T indexed by word.
     */
   template<typename T>
   class WordIndex : Common::Uncopyable
   {
      static const int MIN_WORD_SIZE_PARTIAL_MATCH; ///< During a search, the words which have a size below this value must match entirely, for exemple 'of' match "conspiracy of one" and not "offspring".
   public:
      WordIndex();

      void addItem(const QStringList& words, T* item);
      void addItem(const QString& word, T* item);
      void rmItem(const QStringList& words, T* item);
      void rmItem(const QString& word, T* item);
      QList<NodeResult<T>> search(const QStringList& words, int maxNbResultPerWord = -1) const;
      QList<NodeResult<T>> search(const QString& word, int maxNbResult = -1) const;

      static QList<T*> resultToList(const QList<NodeResult<int>>& result);

   private:
      Node<T> root;
      mutable QMutex mutex;
   };
}

/***** Definitions *****/
using namespace FM;

template<typename T>
const int WordIndex<T>::MIN_WORD_SIZE_PARTIAL_MATCH(3);

template<typename T>
WordIndex<T>::WordIndex()
{}

template<typename T>
void WordIndex<T>::addItem(const QStringList& words, T* item)
{
   QMutexLocker locker(&mutex);
   for (QListIterator<QString> i(words); i.hasNext();)
      this->root.addItem(i.next(), item);
}

template<typename T>
void WordIndex<T>::addItem(const QString& word, T* item)
{
   QMutexLocker locker(&mutex);
   this->root.addItem(word, item);
}

template<typename T>
void WordIndex<T>::rmItem(const QStringList& words, T* item)
{
   QMutexLocker locker(&mutex);
   for (QListIterator<QString> i(words); i.hasNext();)
      this->root.rmItem(i.next(), item);
}

template<typename T>
void WordIndex<T>::rmItem(const QString& word, T* item)
{
   QMutexLocker locker(&mutex);
   this->root.rmItem(word, item);
}

template<typename T>
QList<NodeResult<T>> WordIndex<T>::search(const QStringList& words, int maxNbResultPerWord) const
{
   QMutexLocker locker(&mutex);

   QList<NodeResult<T>> result;
   for (QListIterator<QString> i(words); i.hasNext();)
   {
      const QString& word = i.next();
      result << this->root.search(word, word.size() >= MIN_WORD_SIZE_PARTIAL_MATCH, maxNbResultPerWord);
   }
   return result;
}

template<typename T>
QList<NodeResult<T>> WordIndex<T>::search(const QString& word, int maxNbResult) const
{
   QMutexLocker locker(&mutex);
   return this->root.search(word, word.size() >= MIN_WORD_SIZE_PARTIAL_MATCH, maxNbResult);
}

template<typename T>
QList<T*> WordIndex<T>::resultToList(const QList<NodeResult<int>>& result)
{
   QList<T*> l;
   for (auto i = result.begin(); i != result.end(); ++i)
      l << i->value;
   return l;
}

#endif
