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
#include <limits>

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

void TreeTests::defaultInitialization()
{
   IntTree tree;
   QCOMPARE(tree.getItem(), 0);

   SimpleTree<int> integerTree;
   QCOMPARE(integerTree.getItem(), 0);

   SimpleTree<int*> pointerTree;
   QVERIFY(pointerTree.getItem() == nullptr);

   SimpleTree<int> initializedTree(42, nullptr);
   QCOMPARE(initializedTree.getItem(), 42);
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

void TreeTests::invalidIndices()
{
   IntTree tree;
   const IntTree& constTree = tree;

   // Exercise both empty and populated trees, including the exact upper bound.
   for (int childCount : { 0, 2 })
   {
      while (tree.getNbChildren() < childCount)
         tree.insertChild(tree.getNbChildren());

      for (int pos : { std::numeric_limits<int>::min(), -1, childCount, std::numeric_limits<int>::max() })
      {
         QVERIFY(tree.getChild(pos) == nullptr);
         QVERIFY_THROWS_EXCEPTION(OutOfRangeException, tree[pos]);
         QVERIFY_THROWS_EXCEPTION(OutOfRangeException, constTree[pos]);

         tree.moveChild(pos, 0);
         tree.moveChild(0, pos);
         QCOMPARE(tree.getNbChildren(), childCount);
         for (int i = 0; i < childCount; ++i)
            QCOMPARE(tree[i].getItem(), i);
      }
   }

   tree.moveChild(0, 1);
   QCOMPARE(tree[0].getItem(), 1);
   QCOMPARE(constTree[1].getItem(), 0);
}

void TreeTests::clampInsertionPosition()
{
   IntTree tree;
   IntTree* first = tree.insertChild(2, -1);
   QCOMPARE(tree.getChild(0), first);
   IntTree* prepended = tree.insertChild(1, std::numeric_limits<int>::min());
   QCOMPARE(tree.getChild(0), prepended);
   QCOMPARE(prepended->getParent(), &tree);
   IntTree* appended = tree.insertChild(4, std::numeric_limits<int>::max());
   QCOMPARE(tree.getChild(2), appended);
   tree.insertChild(3, 2);
   tree.insertChild(5, tree.getNbChildren());
   tree.insertChild(0, 0);

   QCOMPARE(tree.getNbChildren(), 6);
   for (int i = 0; i < tree.getNbChildren(); ++i)
      QCOMPARE(tree[i].getItem(), i);
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

void TreeTests::deleteDuringReverseDepthFirst()
{
   for (bool includeRoot : { false, true })
   {
      IntTree root(0, nullptr);
      IntTree* first = root.insertChild(1);
      first->insertChild(2);
      first->insertChild(3);
      root.insertChild(4)->insertChild(5);

      QList<int> actual;
      QVERIFY(root.mapReverseDepthFirst([&](IntTree* node) {
         actual << node->getItem();
         if (node != &root)
            delete node;
         return true;
      }, includeRoot));

      QList<int> expected { 2, 3, 1, 5, 4 };
      if (includeRoot)
         expected << 0;
      QCOMPARE(actual, expected);
      QCOMPARE(root.getNbChildren(), 0);
   }
}

void TreeTests::modifyDescendantsDuringReverseDepthFirst()
{
   IntTree root(0, nullptr);
   IntTree* first = root.insertChild(1);
   first->insertChild(2);
   first->insertChild(3);
   root.insertChild(4)->insertChild(5);

   QList<int> actual;
   TreeReverseDepthFirstIterator<IntTree> iterator(&root, true);
   while (iterator.hasNext())
   {
      IntTree* node = iterator.next();
      actual << node->getItem();
      if (node->getNbChildren() > 0)
      {
         node->deleteAllChildren();
         node->insertChild(99); // Already-visited descendants may be replaced.
      }
   }

   const QList<int> expected { 2, 3, 1, 5, 4, 0 };
   QCOMPARE(actual, expected);
   QCOMPARE(root.getNbChildren(), 1);
   QCOMPARE(root[0].getItem(), 99);
   QVERIFY(iterator.next() == nullptr);
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
