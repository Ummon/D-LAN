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

#include <WordIndexBenchmark.h>

#include <algorithm>
#include <functional>

#include <QElapsedTimer>
#include <QRandomGenerator>
#include <QtDebug>

#ifdef Q_OS_WIN32
   #include <windows.h>
#endif

#include <Common/StringUtils.h>

#include <BenchmarkUtils.h>
using namespace FM;

namespace
{
   const int NB_NAMES = 300000;
   const int MAX_NB_RESULT = 300; ///< The default value of the setting 'max_number_of_search_result_to_send'.

   const int NB_BUILDS = 5; ///< The number of times the index is built to measure 'addItems()' and 'rmItems()'.

   /**
     * The memory allocated by the process [byte], -1 if unknown.
     */
   qint64 allocatedMemory()
   {
#ifdef Q_OS_WIN32
      // 'malloc(..)' allocates in the heap of the process.
      HEAP_SUMMARY summary {};
      summary.cb = sizeof(summary);
      if (HeapSummary(GetProcessHeap(), 0, &summary))
         return static_cast<qint64>(summary.cbAllocated);
#endif
      return -1;
   }

   QString toMilliseconds(qint64 nanoseconds)
   {
      return QString::number(nanoseconds / 1e6, 'f', 1);
   }
}

WordIndexBenchmark::WordIndexBenchmark() :
   indexMemory(-1)
{
}

void WordIndexBenchmark::initTestCase()
{
#if DEBUG
   qWarning() << "Built in debug: the durations aren't the ones of a release";
#endif

   // Always the same names: the results of two runs can be compared.
   QRandomGenerator rng(42);
   this->vocabulary = BenchmarkUtils::createVocabulary(rng);

   this->names.reserve(NB_NAMES);
   for (int i = 0; i < NB_NAMES; ++i)
      this->names << Common::StringUtils::splitInWordsAndSubWords(BenchmarkUtils::createName(rng, this->vocabulary));

   const qint64 memoryBefore = allocatedMemory();
   this->fill(this->index);
   if (memoryBefore != -1)
      this->indexMemory = allocatedMemory() - memoryBefore;
}

/**
  * The memory used by the index of all the names.
  */
void WordIndexBenchmark::memory()
{
   if (this->indexMemory == -1)
      QSKIP("The allocated memory isn't known on this platform");

   qInfo().noquote() << this->indexMemory << "bytes," << QString::number(this->indexMemory / 1048576.0, 'f', 1) << "MiB";
}

/**
  * The duration to index all the names.
  */
void WordIndexBenchmark::addItems()
{
   QList<qint64> durations;
   for (int i = 0; i < NB_BUILDS; ++i)
   {
      WordIndex<const QStringList*> index;
      QElapsedTimer timer;
      timer.start();
      this->fill(index);
      durations << timer.nsecsElapsed();
   }

   qInfo().noquote() << toMilliseconds(*std::min_element(durations.cbegin(), durations.cend())) << "ms";
}

/**
  * The duration to remove all the names from the index.
  */
void WordIndexBenchmark::rmItems()
{
   QList<qint64> durations;
   for (int i = 0; i < NB_BUILDS; ++i)
   {
      WordIndex<const QStringList*> index;
      this->fill(index);

      QElapsedTimer timer;
      timer.start();
      for (const QStringList& words : std::as_const(this->names))
         index.rmItem(words, &words);
      durations << timer.nsecsElapsed();

      QCOMPARE(index.toStringLog(), WordIndex<const QStringList*>().toStringLog());
   }

   qInfo().noquote() << toMilliseconds(*std::min_element(durations.cbegin(), durations.cend())) << "ms";
}

void WordIndexBenchmark::search_data()
{
   QTest::addColumn<QString>("terms");
   QTest::addColumn<bool>("found");
   QTest::addColumn<bool>("filtered");

   const auto addRow = [](const char* name, const QString& terms, bool found = true, bool filtered = false)
   {
      QTest::newRow(name) << terms << found << filtered;
   };

   const QString common = this->vocabulary.at(0);
   const QString rare = this->vocabulary.at(30000);
   const QString han = QString(QChar(BenchmarkUtils::FIRST_HAN));

   // No result: the most frequent case, a peer receives the searches of all the other ones.
   addRow("unknown word", common + "x", false); // Fails after the nodes of the known word.
   addRow("unknown script", QString(3, QChar(0x0436)), false); // Cyrillic: fails after all the children of the root.

   // One term.
   addRow("rare word", rare);
   addRow("common word", common); // A node with a lot of items.
   addRow("prefix of some words", this->vocabulary.at(100).left(3));
   addRow("prefix of many words", "dsc"); // A lot of nodes with one item.
   addRow("Han word", han + QChar(BenchmarkUtils::FIRST_HAN + 1));
   addRow("Han character", han); // A prefix ending at a node with a lot of children.

   // Several terms.
   addRow("rare and common words", rare + " mp3");
   addRow("three common words", common + ' ' + this->vocabulary.at(1) + ' ' + this->vocabulary.at(2));
   addRow("five words", common + ' ' + this->vocabulary.at(50) + ' ' + this->vocabulary.at(2000) + " 07 mp3");
   addRow("rare word and prefix of many words", rare + " dsc"); // All the nodes of a large part of the index are visited.
   addRow("two Han characters", han + ' ' + QChar(BenchmarkUtils::FIRST_HAN + 1)); // Prefixes ending at nodes with a lot of children.

   // As a search among the files of a given type.
   addRow("common word, filtered", common, true, true);
}

/**
  * The duration of a search as 'FileManager::find(..)' does it for another peer.
  */
void WordIndexBenchmark::search()
{
   QFETCH(QString, terms);
   QFETCH(bool, found);
   QFETCH(bool, filtered);

   const QStringList words = Common::StringUtils::splitInWords(terms);

   std::function<bool(const QStringList* const&)> predicate;
   if (filtered)
      predicate = [](const QStringList* const& words) { return words->contains(QLatin1String("mp3")); };

   const qsizetype nbMatchingItems = this->index.search(words, -1, predicate).size();
   QCOMPARE(nbMatchingItems != 0, found);

   qsizetype nbResults = 0;
   const double duration = BenchmarkUtils::measure([&] { nbResults += this->index.search(words, MAX_NB_RESULT, predicate).size(); });
   QCOMPARE(nbResults != 0, found);

   qInfo().noquote() << QString::number(duration / 1e3, 'f', 3) << "us," << nbMatchingItems << "matching items";
}

void WordIndexBenchmark::fill(WordIndex<const QStringList*>& index) const
{
   for (const QStringList& words : this->names)
      index.addItem(words, &words);
}
