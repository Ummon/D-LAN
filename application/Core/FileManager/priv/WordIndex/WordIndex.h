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
#include <QHash>
#include <QVector>
#include <QString>
#include <QChar>
#include <QRecursiveMutex>

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
      static constexpr int MAX_SEARCH_TERMS = 24; ///< Larger queries return no results; keeps work and relevance scores bounded.
      static const int MIN_WORD_SIZE_PARTIAL_MATCH; ///< During a search, the words which have a size below this value must match entirely, for example 'of' match "conspiracy of one" and not "offspring".
      static const int MIN_WORD_SIZE_PARTIAL_MATCH_KOREAN;

      WordIndex();

      void addItem(const QString& word, const T& item);
      void addItem(const QStringList& words, const T& item);
      bool rmItem(const QString& word, const T& item);
      bool rmItem(const QStringList& words, const T& item);
      void renameItem(const QString& oldWord, const QString& newWord, const T& item);
      void renameItem(const QStringList& oldWords, const QStringList& newWords, const T& item);

      QList<NodeResult<T>> search(
         const QString& word,
         int maxNbResult = -1,
         std::function<bool(const T&)> predicat = nullptr
      ) const;

      QList<NodeResult<T>> search(
         const QStringList& words,
         int maxNbResult = -1,
         std::function<bool(const T&)> predicat = nullptr
      ) const;

      QString toStringLog() const;

      static QList<T> resultToList(const QList<NodeResult<T>>& result);

   private:
      Node<T> root;
      mutable QRecursiveMutex mutex;
   };
}

template<typename T>
const int FM::WordIndex<T>::MIN_WORD_SIZE_PARTIAL_MATCH(3);

template<typename T>
const int FM::WordIndex<T>::MIN_WORD_SIZE_PARTIAL_MATCH_KOREAN(1);

template<typename T>
   FM::WordIndex<T>::WordIndex()
{}

template<typename T>
void FM::WordIndex<T>::addItem(const QString& word, const T& item)
{
   QMutexLocker locker(&this->mutex);
   this->root.addItem(QStringView(word), item);
}

template<typename T>
void FM::WordIndex<T>::addItem(const QStringList& words, const T& item)
{
   QMutexLocker locker(&this->mutex);
   for (QStringListIterator i(words); i.hasNext();)
      this->root.addItem(QStringView(i.next()), item);
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
      this->root.addItem(QStringView(i.next()), item);
}

/**
  * Return a an unordered list of 'NodeResult' matching the given word.
  * If 'NodeResult::level' is 0 then the item matches entirely the given word otherwise (level is 1)
  * the word match the beginning of the indexed string.
  * There is a particular case when the word length is below 'MIN_WORD_SIZE_PARTIAL_MATCH',
  * see the comment associated to this constant for more information.
  */
template<typename T>
QList<FM::NodeResult<T>> FM::WordIndex<T>::search(
   const QString& word,
   int maxNbResult,
   std::function<bool(const T&)> predicat
) const
{
   QMutexLocker locker(&this->mutex);
   return
      this->root.search(
         word,
         word.size() >=
            (
               Common::StringUtils::isKorean(word) ?
                    MIN_WORD_SIZE_PARTIAL_MATCH_KOREAN
                  : MIN_WORD_SIZE_PARTIAL_MATCH
            ),
         maxNbResult,
         predicat
      );
}

/**
  * Aggregate actual matches rather than enumerating every subset of query terms.
  * A negative result limit means unlimited; oversized queries return no results.
  */
template<typename T>
QList<FM::NodeResult<T>> FM::WordIndex<T>::search(
   const QStringList& words,
   int maxNbResult,
   std::function<bool(const T&)> predicat
) const
{
   QMutexLocker locker(&this->mutex);

   const int N = words.size();
   if (N == 0 || N > MAX_SEARCH_TERMS || maxNbResult == 0)
      return {};

   struct Match
   {
      QList<int> terms;
      int partialMatches = 0;
   };
   QHash<T, Match> matches;
   for (int term = 0; term < N; ++term)
   {
      // An item can be indexed by several words matching the same prefix.
      // Count it once per query term, preferring an exact match if available.
      QHash<T, bool> termMatches;
      for (const auto& node : this->search(words[term], -1, predicat))
      {
         auto it = termMatches.find(node.value);
         if (it == termMatches.end())
            termMatches.insert(node.value, node.level != 0);
         else
            it.value() = it.value() && node.level != 0;
      }
      for (auto it = termMatches.cbegin(); it != termMatches.cend(); ++it)
      {
         auto& match = matches[it.key()];
         match.terms.append(term);
         match.partialMatches += it.value() ? 1 : 0;
      }
   }
   if (matches.isEmpty())
      return {};

   // Preserve the legacy relevance levels without enumerating subsets:
   // more matched terms first, then fewer prefix matches, then earlier query terms.
   // At most 24 terms keeps these binomial coefficients and levels within int.
   QVector<QVector<int>> choose(N + 1, QVector<int>(N + 1, 0));
   for (int n = 0; n <= N; ++n)
   {
      choose[n][0] = choose[n][n] = 1;
      for (int k = 1; k < n; ++k)
         choose[n][k] = choose[n - 1][k - 1] + choose[n - 1][k];
   }
   QVector<int> groupBase(N + 1, 0);
   for (int k = N - 1; k >= 1; --k)
      groupBase[k] = groupBase[k + 1] + choose[N][k + 1] * (k + 2);

   QList<NodeResult<T>> finalResult;
   finalResult.reserve(matches.size());
   for (auto it = matches.cbegin(); it != matches.cend(); ++it)
   {
      const auto& match = it.value();
      const int count = match.terms.size();
      int rank = 0;
      int previous = -1;
      for (int i = 0; i < count; ++i)
      {
         for (int skipped = previous + 1; skipped < match.terms[i]; ++skipped)
            rank += choose[N - skipped - 1][count - i - 1];
         previous = match.terms[i];
      }
      NodeResult<T> result(it.key());
      result.level = groupBase[count] + match.partialMatches * choose[N][count] + rank;
      finalResult.append(result);
   }

   if (maxNbResult >= 0 && finalResult.size() > maxNbResult)
   {
      std::partial_sort(finalResult.begin(), finalResult.begin() + maxNbResult, finalResult.end());
      finalResult.resize(maxNbResult);
   }
   else
      std::sort(finalResult.begin(), finalResult.end());

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
