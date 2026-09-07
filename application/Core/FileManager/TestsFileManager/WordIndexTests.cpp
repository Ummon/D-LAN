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

#include <WordIndexTests.h>
using namespace FM;

#include <priv/WordIndex/WordIndex.h>

WordIndexTests::WordIndexTests()
{
}

void WordIndexTests::initTestCase()
{
}

void WordIndexTests::testWordIndex()
{
   qDebug() << "===== testWordIndex() =====";

   WordIndex<int> index;
   int arbre = 1;
   int arbalete = 2;
   int ar = 2;
   int arbuste = 3;

   index.addItem(QString("arbre"), arbre);
   index.addItem(QString("arbalete"), arbalete);

   QList<int> result0 = WordIndex<int>::resultToList(index.search("arime"));
   QVERIFY(result0.size() == 0);

   index.addItem("ar", ar);
   index.addItem("arbuste", arbuste);

   qDebug() << index.toStringLog();

   QList<int> result1 = WordIndex<int>::resultToList(index.search("ar"));
   QVERIFY(result1.size() == 1);
   QVERIFY(result1.contains(ar));

   QList<int> result2 = WordIndex<int>::resultToList(index.search("arb"));
   QVERIFY(result2.size() == 3);
   QVERIFY(result2.contains(arbre));
   QVERIFY(result2.contains(arbalete));
   QVERIFY(result2.contains(arbuste));

   QList<int> result3 = WordIndex<int>::resultToList(index.search("arbr"));
   QVERIFY(result3.size() == 1);
   QVERIFY(result3.contains(arbre));

   QList<int> result4 = WordIndex<int>::resultToList(index.search("arbre"));
   QVERIFY(result4.size() == 1);
   QVERIFY(result4.contains(arbre));

   QList<int> result5 = WordIndex<int>::resultToList(index.search("arbres"));
   QVERIFY(result5.size() == 0);

   index.rmItem("arbuste", arbuste);

   QList<int> result6 = WordIndex<int>::resultToList(index.search("arb"));
   QVERIFY(result6.size() == 2);
   QVERIFY(result6.contains(arbre));
   QVERIFY(result6.contains(arbalete));

   QList<int> result7 = WordIndex<int>::resultToList(index.search("arbuste"));
   QVERIFY(result7.size() == 0);

   index.rmItem("arbalete", arbalete);

   QList<int> result8 = WordIndex<int>::resultToList(index.search("arb"));
   QVERIFY(result8.size() == 1);
   QVERIFY(result8.contains(arbre));

   QList<int> result9 = WordIndex<int>::resultToList(index.search("arbalete"));
   QVERIFY(result9.size() == 0);

   index.rmItem("arbre", arbre);

   QList<int> result10 = WordIndex<int>::resultToList(index.search("arb"));
   QVERIFY(result10.size() == 0);
}

void WordIndexTests::removalPreservesRemainingWords_data()
{
   QTest::addColumn<QString>("remaining");
   QTest::addColumn<QString>("removed");
   QTest::newRow("root-siblings") << QString("alpha") << QString("beta");
   QTest::newRow("remove-prefix") << QString("alphabet") << QString("alpha");
   QTest::newRow("remove-longer-word") << QString("alpha") << QString("alphabet");
}

void WordIndexTests::removalPreservesRemainingWords()
{
   QFETCH(QString, remaining);
   QFETCH(QString, removed);
   WordIndex<int> index;
   index.addItem(remaining, 1);
   index.addItem(removed, 2);
   QVERIFY(index.rmItem(removed, 2));
   QCOMPARE(WordIndex<int>::resultToList(index.search(remaining)), (QList<int> { 1 }));
   QCOMPARE(WordIndex<int>::resultToList(index.search(remaining.left(3))), (QList<int> { 1 }));
   QCOMPARE(WordIndex<int>::resultToList(index.search(QStringList { remaining })), (QList<int> { 1 }));

   // Further changes must still traverse the original word from the root.
   index.addItem(QString("gamma"), 3);
   QVERIFY(index.rmItem(QString("gamma"), 3));
   QCOMPARE(WordIndex<int>::resultToList(index.search(remaining)), (QList<int> { 1 }));
   index.renameItem(QStringList { remaining }, QStringList { "delta" }, 1);
   QVERIFY(index.search(remaining).isEmpty());
   QCOMPARE(WordIndex<int>::resultToList(index.search(QString("delta"))), (QList<int> { 1 }));
   QVERIFY(index.rmItem(QString("delta"), 1));
   QVERIFY(index.search(QString("delta")).isEmpty());
   index.addItem(remaining, 4);
   QCOMPARE(WordIndex<int>::resultToList(index.search(remaining)), (QList<int> { 4 }));
}

void WordIndexTests::cleanupTestCase()
{
}

void WordIndexTests::multiTermRanking()
{
   WordIndex<int> index;
   index.addItem(QStringList { "alpha", "beta", "gamma" }, 1);
   index.addItem(QStringList { "alphabet", "beta", "gamma" }, 2);
   index.addItem(QStringList { "alpha", "beta" }, 3);
   index.addItem(QStringList { "alpha", "gamma" }, 4);
   index.addItem(QStringList { "beta", "gamma" }, 5);
   index.addItem(QStringList { "alphabet", "beta" }, 6);
   index.addItem(QStringList { "alpha" }, 7);
   index.addItem(QStringList { "alphabet" }, 8);
   const QStringList terms { "alpha", "beta", "gamma" };
   const auto result = index.search(terms); // The default negative limit is unlimited.
   QCOMPARE(WordIndex<int>::resultToList(result), (QList<int> { 1, 2, 3, 4, 5, 6, 7, 8 }));
   const QList<int> levels { 0, 1, 4, 5, 6, 7, 13, 16 };
   for (int i = 0; i < result.size(); ++i)
      QCOMPARE(result[i].level, levels[i]);
   QCOMPARE(WordIndex<int>::resultToList(index.search(terms, 2)), (QList<int> { 1, 2 }));
   QCOMPARE(WordIndex<int>::resultToList(index.search(terms, 2, [](int value) { return value > 2; })),
      (QList<int> { 3, 4 }));
   QVERIFY(index.search(terms, 0).isEmpty());
   QVERIFY(index.search(QStringList()).isEmpty());

   // Multiple indexed words matching one query term must not inflate its weight.
   index.addItem(QString("alphabetical"), 1);
   const auto duplicate = index.search(terms);
   QCOMPARE(duplicate.size(), result.size());
   QCOMPARE(duplicate.first().value, 1);
   QCOMPARE(duplicate.first().level, 0);
}

void WordIndexTests::prefixRankingSurvivesTrieChanges()
{
   WordIndex<int> index;
   index.addItem(QStringList { "alphabet", "beta" }, 1);
   const auto checkPrefix = [&] {
      const auto single = index.search(QString("alpha"));
      QCOMPARE(single.size(), 1);
      QCOMPARE(single.first().value, 1);
      QCOMPARE(single.first().level, 1);
      const auto multiple = index.search(QStringList { "alpha", "beta" });
      QCOMPARE(multiple.size(), 1);
      QCOMPARE(multiple.first().level, 1);
      const auto exact = index.search(QString("alphabet"));
      QCOMPARE(exact.size(), 1);
      QCOMPARE(exact.first().level, 0);
      QVERIFY(index.search(QString("al")).isEmpty()); // Short words still require an exact match.
   };
   checkPrefix(); // Query ends inside a compressed node.
   index.addItem(QString("alpine"), 3);
   checkPrefix(); // Query spans a branch and ends inside its child.
   index.addItem(QStringList { "alpha", "beta" }, 2);
   const auto ranked = index.search(QStringList { "alpha", "beta" }, 2);
   QCOMPARE(WordIndex<int>::resultToList(ranked), (QList<int> { 2, 1 }));
   QCOMPARE(ranked[0].level, 0);
   QCOMPARE(ranked[1].level, 1);
   QCOMPARE(WordIndex<int>::resultToList(index.search(QStringList { "alpha", "beta" }, 1)),
      (QList<int> { 2 }));
   QVERIFY(index.rmItem(QStringList { "alpha", "beta" }, 2));
   checkPrefix();
   QVERIFY(index.rmItem(QString("alpine"), 3));
   checkPrefix(); // Compaction must not turn the prefix into an exact match.
}

void WordIndexTests::longQueries()
{
   WordIndex<int> index;
   QStringList terms;
   for (int i = 0; i < WordIndex<int>::MAX_SEARCH_TERMS; ++i)
      terms << QString("absent%1").arg(i);
   // Previously even an empty index enumerated millions of combinations.
   QVERIFY(index.search(terms, 100).isEmpty());
   index.addItem(terms, 1);
   index.addItem(terms.last(), 2);
   const auto result = index.search(terms, 100);
   QCOMPARE(WordIndex<int>::resultToList(result), (QList<int> { 1, 2 }));
   QCOMPARE(result.first().level, 0);
   QVERIFY(result.last().level > 0);
   QCOMPARE(WordIndex<int>::resultToList(index.search(terms, 1)), (QList<int> { 1 }));
   terms << "one-term-too-many";
   QVERIFY(index.search(terms, 100).isEmpty()); // Reject; do not silently ignore trailing constraints.
}
