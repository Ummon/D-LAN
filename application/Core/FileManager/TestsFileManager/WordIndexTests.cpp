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

void WordIndexTests::cleanupTestCase()
{
}
