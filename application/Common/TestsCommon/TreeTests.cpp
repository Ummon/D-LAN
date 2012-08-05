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
  
#include <TreeTests.h>
using namespace Common;

#include <QList>

/**
  * Test tree :
  *
  *         1
  *       /   \
  *     2       3
  *           / | \
  *         4   5   6
  *        / \        \
  *       7   8         9
  */

TreeTests::TreeTests()
{
}

void TreeTests::initTestCase()
{
   this->tree.setItem(1);
   QVERIFY(this->tree.insertChild(2)->getItem() == 2);
   IntTree* sub3 = this->tree.insertChild(3);
   QVERIFY(sub3 != nullptr);
   IntTree* sub4 = sub3->insertChild(4);
   QVERIFY(sub4 != nullptr);
   QVERIFY(sub3->insertChild(6)->getItem() == 6);
   QVERIFY(sub3->insertChild(5, 1)->getItem() == 5);
   QVERIFY(sub4->insertChild(7)->getItem() == 7);
   QVERIFY(sub4->insertChild(8, 100)->getItem() == 8); // Should be put at the end.
}

void TreeTests::insertElements()
{
   QVERIFY(this->tree[1][2].insertChild(9)->getItem() == 9);
}

void TreeTests::retrieveElements()
{
   QVERIFY(this->tree.getItem() == 1);
   QVERIFY(this->tree[0].getItem() == 2);
   QVERIFY(this->tree[1][0][1].getItem() == 8);
   try
   {
      this->tree[1][99][1].getItem();
      QFAIL("this->tree[1][99] must throw an exception");
   }
   catch(OutOfRangeException&)
   {
   }
}

void TreeTests::iterateBreathFirst()
{
   {
      QList<int> expected { 1, 2, 3, 4, 5, 6, 7, 8, 9 };
      QList<int> actual;
      this->tree.mapBreadthFirst([&](IntTree* tree) { actual << tree->getItem(); return true; }, true);
      QVERIFY(expected == actual);
   }

   {
      QList<int> expected { 4, 5, 6, 7, 8, 9 };
      QList<int> actual;
      this->tree[1].mapBreadthFirst([&](IntTree* tree) { actual << tree->getItem(); return true; });
      QVERIFY(expected == actual);
   }
}

void TreeTests::iterateDepthFirst()
{
   {
      QList<int> expected { 1, 2, 3, 4, 7, 8, 5, 6, 9 };
      QList<int> actual;
      this->tree.mapDepthFirst([&](IntTree* tree) { actual << tree->getItem(); return true; }, true);
      QVERIFY(expected == actual);
   }

   {
      QList<int> expected { 4, 7, 8, 5, 6, 9 };
      QList<int> actual;
      this->tree[1].mapDepthFirst([&](IntTree* tree) { actual << tree->getItem(); return true; });
      QVERIFY(expected == actual);
   }
}

void TreeTests::iterateReverseDepthFirst()
{
   {
      QList<int> expected { 2, 7, 8, 4, 5, 9, 6, 3, 1 };
      QList<int> actual;
      this->tree.mapReverseDepthFirst([&](IntTree* tree) { actual << tree->getItem(); return true; }, true);
      QVERIFY(expected == actual);
   }

   {
      QList<int> expected { 7, 8, 4, 5, 9, 6 };
      QList<int> actual;
      this->tree[1].mapReverseDepthFirst([&](IntTree* tree) { actual << tree->getItem(); return true; });
      QVERIFY(expected == actual);
   }
}

void TreeTests::removeElements()
{
   delete this->tree[1].getChild(0);
   QList<int> expected1 { 1, 2, 3, 5, 6, 9 };
   testElementsAgainstList(expected1, &this->tree, true);

   delete this->tree.getChild(1);
   QList<int> expected2 { 1, 2 };
   testElementsAgainstList(expected2, &this->tree, true);
}

void TreeTests::testElementsAgainstList(const QList<int> &expected, IntTree* tree, bool withRoot)
{
   QList<int> actual;
   for (TreeBreadthFirstIterator<IntTree> i(tree, withRoot); i.hasNext();)
      actual << i.next()->getItem();

   QVERIFY(actual == expected);
}
