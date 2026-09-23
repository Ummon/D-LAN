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

#include <algorithm>
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

void WordIndexTests::shortPrefixMatching_data()
{
   QTest::addColumn<QString>("prefix");
   QTest::addColumn<QString>("word");
   QTest::addColumn<bool>("matches");
   QTest::newRow("one-han") << QStringLiteral("\u65E5") << QStringLiteral("\u65E5\u672C\u8A9E") << true;
   QTest::newRow("two-han") << QStringLiteral("\u65E5\u672C") << QStringLiteral("\u65E5\u672C\u8A9E") << true;
   QTest::newRow("supplementary-han") << QStringLiteral("\U00020000") << QStringLiteral("\U00020000\u65E5") << true;
   QTest::newRow("kana") << QStringLiteral("\u306B") << QStringLiteral("\u306B\u307B\u3093") << true;
   QTest::newRow("hangul") << QStringLiteral("\uD55C") << QStringLiteral("\uD55C\uAE00") << true;
   QTest::newRow("short-latin") << QString("al") << QString("alpha") << false;
   QTest::newRow("long-latin") << QString("alp") << QString("alpha") << true;
   QTest::newRow("two-graphemes-combining") << QStringLiteral("a\u0301b") << QStringLiteral("a\u0301bcd") << false;
   QTest::newRow("three-graphemes-combining") << QStringLiteral("a\u0301bc") << QStringLiteral("a\u0301bcd") << true;
   QTest::newRow("two-graphemes-surrogate") << QStringLiteral("\U00010428a") << QStringLiteral("\U00010428abcd") << false;
   QTest::newRow("three-graphemes-surrogate") << QStringLiteral("\U00010428ab") << QStringLiteral("\U00010428abcd") << true;
}

void WordIndexTests::shortPrefixMatching()
{
   QFETCH(QString, prefix);
   QFETCH(QString, word);
   QFETCH(bool, matches);

   WordIndex<int> index;
   index.addItem(word, 1);
   const auto result = index.search(prefix);
   QCOMPARE(result.size(), matches ? 1 : 0);
   if (matches)
   {
      QCOMPARE(result.first().value, 1);
      QCOMPARE(result.first().level, 1);
   }

   // Exercise the same normalization and list overload used by FileManager.
   WordIndex<int> normalizedIndex;
   normalizedIndex.addItem(Common::StringUtils::splitInWords(word), 1);
   const QStringList terms = Common::StringUtils::splitInWords(prefix);
   QCOMPARE(terms.size(), 1);
   QCOMPARE(normalizedIndex.search(terms).size(), matches ? 1 : 0);
   normalizedIndex.addItem(terms, 2);
   const auto ranked = normalizedIndex.search(terms);
   QCOMPARE(ranked.first().value, 2);
   QCOMPARE(ranked.first().level, 0);
   QCOMPARE(ranked.size(), matches ? 2 : 1);
}

void WordIndexTests::normalizedKanaAndHangul()
{
   WordIndex<int> index;
   const auto add = [&](const QString& word, int item) {
      index.addItem(Common::StringUtils::splitInWords(word), item);
   };
   const auto search = [&](const QString& word) {
      return WordIndex<int>::resultToList(index.search(Common::StringUtils::splitInWords(word)));
   };
   add(QStringLiteral("\u304B\u304D"), 1);
   add(QStringLiteral("\u304C\u304D"), 2);
   add(QStringLiteral("\u30D1\u30F3"), 3);
   add(QStringLiteral("\uD55C\uAE00"), 4);
   QCOMPARE(search(QStringLiteral("\u304B")), (QList<int> { 1 }));
   QCOMPARE(search(QStringLiteral("\u304C")), (QList<int> { 2 }));
   QCOMPARE(search(QStringLiteral("\u304B\u3099")), (QList<int> { 2 }));
   QCOMPARE(search(QStringLiteral("\uFF8A\uFF9F")), (QList<int> { 3 }));
   QCOMPARE(search(QStringLiteral("\uD55C")), (QList<int> { 4 }));
   QCOMPARE(search(QStringLiteral("\u1112\u1161\u11AB")), (QList<int> { 4 }));
   // A complete syllable must not match another syllable's decomposed prefix.
   QVERIFY(search(QStringLiteral("\uD558")).isEmpty());
}

void WordIndexTests::prefixGraphemeBoundaries_data()
{
   QTest::addColumn<QString>("prefix");
   QTest::addColumn<QString>("combined");
   QTest::newRow("hiragana-dakuten") << QStringLiteral("\u308F") << QStringLiteral("\u308F\u3099");
   QTest::newRow("katakana-handakuten") << QStringLiteral("\u30C8") << QStringLiteral("\u30C8\u309A");
   QTest::newRow("hangul-jamo") << QStringLiteral("\u1100") << QStringLiteral("\u1100\u1161");
   QTest::newRow("latin-combining") << QString("abc") << QStringLiteral("abc\u0301");
   QTest::newRow("supplementary-mark") << QStringLiteral("\u65E5") << QStringLiteral("\u65E5\U000E0100");
   QTest::newRow("emoji-joiner") << QStringLiteral("abc\U0001F469") << QStringLiteral("abc\U0001F469\u200D\U0001F4BB");
}

void WordIndexTests::prefixGraphemeBoundaries()
{
   QFETCH(QString, prefix);
   QFETCH(QString, combined);
   WordIndex<int> index;
   index.addItem(combined + "x", 1);
   const auto check = [&](const QList<int>& expected) {
      QCOMPARE(WordIndex<int>::resultToList(index.search(prefix)), expected);
      QCOMPARE(WordIndex<int>::resultToList(index.search(QStringList { prefix })), expected);
      QCOMPARE(WordIndex<int>::resultToList(index.search(combined)), (QList<int> { 1 }));
      int predicateCalls = 0;
      const auto limited = index.search(prefix, 1, [&](int) { ++predicateCalls; return true; });
      QCOMPARE(limited.size(), expected.isEmpty() ? 0 : 1);
      QCOMPARE(predicateCalls, expected.isEmpty() ? 0 : 1);
   };
   check({}); // Query ends inside a compressed node.
   index.addItem(prefix + "y", 2);
   check({ 2 }); // A branch separates the base from the combining continuation.
   index.addItem(prefix, 3);
   check({ 3, 2 }); // Exact matches still rank before valid prefixes.
   QVERIFY(index.rmItem(prefix, 3));
   check({ 2 });
   QVERIFY(index.rmItem(prefix + "y", 2));
   check({}); // Compaction must preserve the boundary decision.

   // A supplementary format character and a combining mark can share a high surrogate.
   index.addItem(prefix + QStringLiteral("\U000E0001"), 4);
   check({ 4 });
   QVERIFY(index.rmItem(prefix + QStringLiteral("\U000E0001"), 4));
   check({});
}

void WordIndexTests::singleWordResultLimits()
{
   WordIndex<int> index;
   index.addItem(QString("of"), 1);
   index.addItem(QString("office"), 2);
   index.addItem(QString("alpha"), 3);
   index.addItem(QString("alphabet"), 4);

   int predicateCalls = 0;
   const auto predicate = [&](int) { ++predicateCalls; return true; };
   for (const QString& query : QStringList { "of", "alp", "alpha" })
   {
      QVERIFY(index.search(query, 0).isEmpty());
      QVERIFY(index.search(query, 0, predicate).isEmpty());
      QVERIFY(index.search(QStringList { query }, 0, predicate).isEmpty());
   }
   QCOMPARE(predicateCalls, 0);

   QCOMPARE(index.search(QString("of"), -1).size(), 1);
   QCOMPARE(index.search(QString("alp"), -1).size(), 2);
   QCOMPARE(index.search(QString("alp"), -2).size(), 2);
   QCOMPARE(index.search(QString("alp"), 1).size(), 1);
   QCOMPARE(WordIndex<int>::resultToList(index.search(QString("alp"), 1, [](int item) { return item == 4; })),
      (QList<int> { 4 }));
}

void WordIndexTests::multiWordPredicateOncePerItem()
{
   WordIndex<int> index;
   index.addItem(QStringList { "alpha", "beta" }, 1);
   index.addItem(QString("alpha"), 2);
   index.addItem(QString("alphabet"), 3);
   index.addItem(QString("gamma"), 4);

   // Item 1 matches both terms and item 3 matches 'alpha' as a prefix: each is filtered once.
   QList<int> filtered;
   const auto results = index.search(QStringList { "alpha", "beta" }, -1, [&](int item) {
      filtered << item;
      return item != 2;
   });
   std::sort(filtered.begin(), filtered.end());
   QCOMPARE(filtered, (QList<int> { 1, 2, 3 }));
   QCOMPARE(WordIndex<int>::resultToList(results), (QList<int> { 1, 3 }));
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
