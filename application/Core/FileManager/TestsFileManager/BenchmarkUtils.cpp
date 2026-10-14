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

#include <BenchmarkUtils.h>

#include <cstdlib>

#include <QElapsedTimer>
#include <QSet>
#include <QVarLengthArray>

namespace
{
   const int VOCABULARY_BITS = 15; ///< 32767 words.
   const int HAN_BITS = 11; ///< 2047 characters.
   const int EXTENSION_BITS = 3; ///< 7 extensions.

   const int NB_BATCHES = 30;
   const qint64 MIN_BATCH_DURATION = 20000000; ///< [ns].
   const int MAX_BLOCK_SIZE = 256; ///< [byte]. See 'measure(..)'.
   const int BLOCK_SIZE_STEP = 8; ///< [byte].

   /**
     * A rank in [0, 2^nbBits - 1[ following the Zipf's law, as the words of a language do:
     * the rank 'r' is about twice as frequent as the rank '2 * r'.
     */
   int zipf(QRandomGenerator& rng, int nbBits)
   {
      const int bit = rng.bounded(nbBits);
      return (1 << bit) - 1 + rng.bounded(1 << bit);
   }
}

/**
  * Words of two to four syllables, from the most frequent to the least one in the names:
  * they share their beginnings as the ones of a language do.
  */
QStringList BenchmarkUtils::createVocabulary(QRandomGenerator& rng)
{
   const QString consonants("bcdfghjklmnprstvz");
   const QString vowels("aeiou");

   QStringList vocabulary;
   QSet<QString> knownWords;
   while (vocabulary.size() < (1 << VOCABULARY_BITS) - 1)
   {
      QString word;
      for (int i = 2 + rng.bounded(3); i > 0; --i)
         word.append(consonants.at(rng.bounded(static_cast<int>(consonants.size())))).append(vowels.at(rng.bounded(static_cast<int>(vowels.size()))));

      if (!knownWords.contains(word))
      {
         knownWords.insert(word);
         vocabulary << word;
      }
   }
   return vocabulary;
}

/**
  * A word of the vocabulary given by 'createVocabulary(..)': the first ones are the most frequently picked.
  */
const QString& BenchmarkUtils::pickWord(QRandomGenerator& rng, const QStringList& vocabulary)
{
   return vocabulary.at(zipf(rng, VOCABULARY_BITS));
}

/**
  * The name of a file:
  *  - 10 %: a photo, "DSC01234.jpg".
  *  - 15 %: a Chinese title of two to seven characters.
  *  - 75 %: two to five words, one name out of five is written like "SomeWordsTogether".
  * A number follows the title of half of the two last kinds.
  */
QString BenchmarkUtils::createName(QRandomGenerator& rng, const QStringList& vocabulary)
{
   static const QStringList EXTENSIONS { "mp3", "jpg", "mkv", "flac", "avi", "pdf", "txt" };
   static const QStringList SEPARATORS { " ", ".", "_", " - " };

   const int kind = rng.bounded(100);
   if (kind < 10)
      return QString("DSC%1.jpg").arg(rng.bounded(100000), 5, 10, QChar('0'));

   QString name;
   if (kind < 25)
   {
      for (int i = 2 + rng.bounded(6); i > 0; --i)
         name.append(QChar(FIRST_HAN + zipf(rng, HAN_BITS)));
   }
   else
   {
      const bool camelCase = rng.bounded(5) == 0;
      const QString& separator = SEPARATORS.at(rng.bounded(static_cast<int>(SEPARATORS.size())));
      for (int i = 2 + rng.bounded(4); i > 0; --i)
      {
         QString word = pickWord(rng, vocabulary);
         if (camelCase)
            word[0] = word.at(0).toUpper();
         else if (!name.isEmpty())
            name.append(separator);
         name.append(word);
      }
   }

   if (rng.bounded(2) == 0)
      name.append(QString(" %1").arg(rng.bounded(1, 100), 2, 10, QChar('0')));

   return name.append('.').append(EXTENSIONS.at(zipf(rng, EXTENSION_BITS)));
}

/**
  * The duration [ns] of one call to 'function'.
  * It's the one of the fastest batch of calls: the least disturbed by the rest of the system.
  */
double BenchmarkUtils::measure(const std::function<void()>& function)
{
   QElapsedTimer timer;

   // Double the size of a batch until it lasts long enough to be measured.
   qint64 nbCalls = 1;
   qint64 duration;
   for (;; nbCalls *= 2)
   {
      timer.start();
      for (qint64 i = 0; i < nbCalls; ++i)
         function();
      duration = timer.nsecsElapsed();
      if (duration >= MIN_BATCH_DURATION)
         break;
   }

   // On Windows, depending on the blocks of the heap which are in use, the allocation of a block of some small sizes
   // followed by its release lasts a hundred times longer than usual (1.7 us instead of 19 ns for 16 to 24 bytes),
   // during all the calls. One more block of each small size before a batch changes this state.
   QVarLengthArray<void*, NB_BATCHES * MAX_BLOCK_SIZE / BLOCK_SIZE_STEP> blocks;

   qint64 shortestDuration = duration;
   for (int batch = 1; batch < NB_BATCHES; ++batch)
   {
      for (size_t size = BLOCK_SIZE_STEP; size <= MAX_BLOCK_SIZE; size += BLOCK_SIZE_STEP)
         blocks.append(malloc(size));

      timer.start();
      for (qint64 i = 0; i < nbCalls; ++i)
         function();
      shortestDuration = qMin(shortestDuration, timer.nsecsElapsed());
   }

   for (void* block : blocks)
      free(block);

   return static_cast<double>(shortestDuration) / nbCalls;
}
