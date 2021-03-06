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
#include <algorithm>

#include <QList>
#include <QString>
#include <QChar>
#include <QMutex>

#include <Common/Uncopyable.h>
#include <Common/Global.h>
#include <Common/LogManager/ILoggable.h>

#include <priv/WordIndex/Node.h>

/**
  * @class FM::WordIndex
  *
  * The purpose of the class 'WordIndex' is to index a set of item of type 'T' by string.
  *
  * This class is thread safe.
  */

namespace FM
{
   template<typename T>
   class WordIndex : public LM::ILoggable, Common::Uncopyable
   {
   public:
      static const int MIN_WORD_SIZE_PARTIAL_MATCH; ///< During a search, the words which have a size below this value must match entirely, for example 'of' match "conspiracy of one" and not "offspring".
      static const int MIN_WORD_SIZE_PARTIAL_MATCH_KOREAN;

      WordIndex();

      void addItem(const QString& word, const T& item);
      void addItem(const QStringList& words, const T& item);
      bool rmItem(const QString& word, const T& item);
      bool rmItem(const QStringList& words, const T& item);
      void renameItem(const QString& oldWord, const QString& newWord, const T& item);
      void renameItem(const QStringList& oldWords, const QStringList& newWords, const T& item);

      QList<NodeResult<T>> search(const QString& word, int maxNbResult = -1, std::function<bool(const T&)> predicat = nullptr) const;
      QList<NodeResult<T>> search(const QStringList& words, int maxNbResult = -1, std::function<bool(const T&)> predicat = nullptr) const;

      QString toStringLog() const;

      static QList<T> resultToList(const QList<NodeResult<T>>& result);

   private:
      Node<T> root;
      mutable QMutex mutex;
   };
}

template<typename T>
const int FM::WordIndex<T>::MIN_WORD_SIZE_PARTIAL_MATCH(3);

template<typename T>
const int FM::WordIndex<T>::MIN_WORD_SIZE_PARTIAL_MATCH_KOREAN(1);

template<typename T>
   FM::WordIndex<T>::WordIndex() :
      mutex(QMutex::Recursive)
{}

template<typename T>
void FM::WordIndex<T>::addItem(const QString& word, const T& item)
{
   QMutexLocker locker(&this->mutex);
   this->root.addItem(&word, item);
}

template<typename T>
void FM::WordIndex<T>::addItem(const QStringList& words, const T& item)
{
   QMutexLocker locker(&this->mutex);
   for (QStringListIterator i(words); i.hasNext();)
      this->root.addItem(&i.next(), item);
}

template<typename T>
bool FM::WordIndex<T>::rmItem(const QString& word, const T& item)
{
   QMutexLocker locker(&this->mutex);
   return this->root.rmItem(word, item);
}

/**
  * @return 'true' if at least one item is removed.
  */
template<typename T>
bool FM::WordIndex<T>::rmItem(const QStringList& words, const T& item)
{
   QMutexLocker locker(&this->mutex);
   bool itemRemoved = false;
   for (QStringListIterator i(words); i.hasNext();)
      itemRemoved |= this->root.rmItem(i.next(), item);
   return itemRemoved;
}

template<typename T>
void FM::WordIndex<T>::renameItem(const QString& oldWord, const QString& newWord, const T& item)
{
   QMutexLocker locker(&this->mutex);
   this->root.rmItem(oldWord, item);
   this->root.addItem(&newWord, item);
}

template<typename T>
void FM::WordIndex<T>::renameItem(const QStringList& oldWords, const QStringList& newWords, const T& item)
{
   QMutexLocker locker(&this->mutex);
   for (QStringListIterator i(oldWords); i.hasNext();)
      this->root.rmItem(i.next(), item);
   for (QStringListIterator i(newWords); i.hasNext();)
      this->root.addItem(&i.next(), item);
}

/**
  * Return a an unordered list of 'NodeResult' matching the given word. If 'NodeResult::level' is 0 then the item matches entirely the given word otherwise (level is 1) the word match the beginning of the indexed string.
  * There is a particular case when the word length is below 'MIN_WORD_SIZE_PARTIAL_MATCH', see the comment associated to this constant for more information.
  */
template<typename T>
QList<FM::NodeResult<T>> FM::WordIndex<T>::search(const QString& word, int maxNbResult, std::function<bool(const T&)> predicat) const
{
   QMutexLocker locker(&this->mutex);
   return this->root.search(word, word.size() >= (Common::StringUtils::isKorean(word) ? MIN_WORD_SIZE_PARTIAL_MATCH_KOREAN : MIN_WORD_SIZE_PARTIAL_MATCH), maxNbResult, predicat);
}

/**
  * @see http://dev.euphorik.ch/wiki/pmp/Algorithms#Word-indexing for more information.
  */
template<typename T>
QList<FM::NodeResult<T>> FM::WordIndex<T>::search(const QStringList& words, int maxNbResult, std::function<bool(const T&)> predicat) const
{
   QMutexLocker locker(&this->mutex);

   const int N = words.size();

   // Launch a search for each term.
   QVector<QSet<NodeResult<T>>> results(N);
   for (int i = 0; i < N; i++)
   {
      // We can only limit the number of result for one term. When there is more than one term and thus some results set, say [a, b, c] for example, some good result may be contained in intersect, for example a & b or a & c.
      auto result = this->search(words[i], N == 1 ? maxNbResult : -1, predicat);
      results[i] += QSet(result.begin(), result.end());
   }

   QList<NodeResult<T>> finalResult;

   int level = 0;

   // For each group of intersection number.
   // For example, [a, b, c] :
   //  * a & b & c
   //  * (a & b) \ c
   //    (a & c) \ b
   //    (b & c) \ a
   //  * a \ b \ c
   for (int i = 0; i < N && finalResult.size() < maxNbResult; i++)
   {
      const int NB_INTERSECTS = N - i; // Number of set intersected.
      int intersect[NB_INTERSECTS]; // A array of the results wich will be intersected.
      for (int j = 0; j < NB_INTERSECTS; j++)
         intersect[j] = j;

      // For each combination of the current intersection group.
      // For 2 intersections (NB_INTERSECTS == 2) among 3 elements [a, b, c]:
      //  * (a, b)
      //  * (a, c)
      //  * (b, c)
      QList<NodeResult<T>> nodesToSort;
      const int NB_COMBINATIONS = Common::Global::nCombinations(N, NB_INTERSECTS);
      for (int j = 0; j < NB_COMBINATIONS && nodesToSort.size() + finalResult.size() < maxNbResult; j++)
      {
         // Apply intersects.
         QSet<NodeResult<T>> currentLevelSet = results[intersect[0]];
         for (QSetIterator<NodeResult<T>> k(currentLevelSet); k.hasNext();)
         {
            NodeResult<T>& node = const_cast<NodeResult<T>&>(k.next());
            node.level = node.level ? NB_COMBINATIONS : 0;
         }

         for (int k = 1; k < NB_INTERSECTS; k++)
            NodeResult<T>::intersect(currentLevelSet, results[intersect[k]], NB_COMBINATIONS);

         // Apply substracts.
         for (int k = -1; k < NB_INTERSECTS; k++)
            for (int l = (k == -1 ? 0 : intersect[k] + 1); l < (k == NB_INTERSECTS - 1 ? N : intersect[k+1]); l++)
               currentLevelSet -= results[l];

         for (QSetIterator<NodeResult<T>> k(currentLevelSet); k.hasNext();)
            const_cast<NodeResult<T>&>(k.next()).level += level;

         // Sort by level.
         nodesToSort << currentLevelSet.values();

         // Define positions of each intersect term.
         for (int k = NB_INTERSECTS - 1; k >= 0; k--)
            if  (intersect[k] < N - NB_INTERSECTS + k)
            {
               intersect[k] += 1;
               for (int l = k + 1; l < NB_INTERSECTS; l++)
                  intersect[l] = intersect[k] + (l - k);
               break;
            }

         level += 1;
      }

      std::sort(nodesToSort.begin(), nodesToSort.end()); // Sort by level

      finalResult << nodesToSort;

      level += NB_COMBINATIONS * NB_INTERSECTS;
   }

   if (finalResult.size() > maxNbResult)
      finalResult.erase(finalResult.end() - (finalResult.size() - maxNbResult), finalResult.end());

   return finalResult;
}

template<typename T>
QString FM::WordIndex<T>::toStringLog() const
{
   QMutexLocker locker(&mutex);
   return this->root.toStringDebug();
}

template<typename T>
QList<T> FM::WordIndex<T>::resultToList(const QList<NodeResult<T>>& result)
{
   QList<T> l;
   for (auto i = result.begin(); i != result.end(); ++i)
      l << i->value;
   return l;
}
