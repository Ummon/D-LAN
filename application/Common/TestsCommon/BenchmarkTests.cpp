#include <BenchmarkTests.h>

#include <set>

#include <QtDebug>
#include <QMap>
#include <QElapsedTimer>
#include <QRandomGenerator64>

#include <Containers/SortedArray.h>
using namespace Common;

BenchmarkTests::BenchmarkTests()
{
}

void BenchmarkTests::sortedArray()
{
   QRandomGenerator64 rng(42);
   const int wordSize = 5;
   const int nbWords = 200000;
   const int M = 7;

   QList<QString> names;
   names.reserve(nbWords);
   for (int i = 0; i < nbWords; i++)
   {
      QString word(wordSize, 'a');
      for (int j = 0; j < word.size(); j++)
         word[j] = 'A' + static_cast<char>(rng.bounded(26));
      names << word;
   }

   QList<QString> namesNotInserted;
   names.reserve(nbWords / 20);
   for (int i = 0; i < nbWords / 20; i++)
   {
      QString word(wordSize, 'a');
      for (int j = 0; j < word.size(); j++)
         word[j] = 'A' + static_cast<char>(rng.bounded(26));
      namesNotInserted << word;
   }

   QElapsedTimer timer;

   ///// Insert /////
   // SortedArray.
   qDebug() << "SortedArray, insert: Elapsed time [ms]:";
   timer.start();
   QList<SortedArray<QString, M>> arrayBenchmarks;
   for (int n = nbWords / 20; n <= nbWords; n += nbWords / 20)
   {
      SortedArray<QString, M> array;
      for (int i = 0; i < n; i++)
         array.insert(names[i]);
      arrayBenchmarks << array;
   }
   qDebug() << timer.elapsed();

   // QMap.
   qDebug() << "QMap, insert: Elapsed time [ms]:";
   timer.start();
   QList<QMap<QString, int>> mapBenchmarks;
   for (int n = nbWords / 20; n <= nbWords; n += nbWords / 20)
   {
      QMap<QString, int> map;
      for (int i = 0; i < n; i++)
         map.insert(names[i], 0);
      mapBenchmarks << map;
   }
   qDebug() << timer.elapsed();

   // std::set.
   qDebug() << "std::set, insert: Elapsed time [ms]:";
   timer.start();
   QList<std::set<QString>> setBenchmarks;
   for (int n = nbWords / 20; n <= nbWords; n += nbWords / 20)
   {
      std::set<QString> set;
      for (int i = 0; i < n; i++)
         set.insert(names[i]);
      setBenchmarks << set;
   }
   qDebug() << timer.elapsed();

   ///// Lookup /////
   // SortedArray.
   qDebug() << "SortedArray (M="<< M <<"), lookup [ms] for 100 *" << nbWords / 20 << "known elements + 100 *" << nbWords / 20 << "unknown elements";
   for (int i = 0; i < arrayBenchmarks.size(); i++)
   {
      timer.start();
      for (int k = 0; k < 100; k++)
         for (int j = 0; j < nbWords / 20; j++)
         {
            arrayBenchmarks[i].contains(names[j]); // Known values.
            arrayBenchmarks[i].contains(namesNotInserted[j]); // Unknown values.
         }
      qDebug() << arrayBenchmarks[i].size() << "\t" << timer.elapsed();
   }

   // QMap.
   qDebug() << "QMap, lookup [ms] for 100 *" << nbWords / 20 << "known elements + 100 *" << nbWords / 20 << "unknown elements";
   for (int i = 0; i < mapBenchmarks.size(); i++)
   {
      timer.start();
      for (int k = 0; k < 100; k++)
         for (int j = 0; j < nbWords / 20; j++)
         {
            mapBenchmarks[i].contains(names[j]); // Known values.
            mapBenchmarks[i].contains(namesNotInserted[j]); // Unknown values.
         }
      qDebug() << mapBenchmarks[i].size() << "\t" << timer.elapsed();
   }

   // std::set.
   qDebug() << "std::set, lookup [ms] for 100 *" << nbWords / 20 << "known elements + 100 *" << nbWords / 20 << "unknown elements";
   for (int i = 0; i < setBenchmarks.size(); i++)
   {
      timer.start();
      for (int k = 0; k < 100; k++)
         for (int j = 0; j < nbWords / 20; j++)
         {
            setBenchmarks[i].find(names[j]); // Known values.
            setBenchmarks[i].find(namesNotInserted[j]); // Unknown values.
         }
      qDebug() << setBenchmarks[i].size() << "\t" << timer.elapsed();
   }

   ///// Delete /////
   // SortedArray.
   qDebug() << "SortedArray, delete: Elapsed time [ms]:";
   timer.start();
   int j = 0;
   for (int n = nbWords / 20; n <= nbWords; n += nbWords / 20)
   {
      for (int i = 0; i < n; i++)
         arrayBenchmarks[j].remove(names[i]);
      QCOMPARE(arrayBenchmarks[j].size(), 0);
      j++;
   }
   qDebug() << timer.elapsed();

   // QMap.
   qDebug() << "QMap, delete: Elapsed time [ms]:";
   timer.start();
   j = 0;
   for (int n = nbWords / 20; n <= nbWords; n += nbWords / 20)
   {
      for (int i = 0; i < n; i++)
         mapBenchmarks[j].remove(names[i]);
      QCOMPARE(mapBenchmarks[j].size(), 0);
      j++;
   }
   qDebug() << timer.elapsed();

   // std::set.
   qDebug() << "std::set, delete: Elapsed time [ms]:";
   timer.start();
   j = 0;
   for (int n = nbWords / 20; n <= nbWords; n += nbWords / 20)
   {
      for (int i = 0; i < n; i++)
         setBenchmarks[j].erase(names[i]);
      QCOMPARE(setBenchmarks[j].size(), (size_t)0);
      j++;
   }
   qDebug() << timer.elapsed();
}
