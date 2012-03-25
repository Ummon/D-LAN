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
   QVERIFY(sub3 != 0);
   IntTree* sub4 = sub3->insertChild(4);
   QVERIFY(sub4 != 0);
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

void TreeTests::iterate()
{
   QList<int> expected1 {1, 2, 3, 4, 5, 6, 7, 8, 9};
   QList<int> expected2 {4, 5, 6, 7, 8, 9};

   QList<int> actual1;
   for (TreeBreadthIterator<IntTree> i(&this->tree, true); i.hasNext();)
      actual1 << i.next()->getItem();

   QList<int> actual2;
   for (TreeBreadthIterator<IntTree> i(&this->tree[1]); i.hasNext();)
      actual2 << i.next()->getItem();

   QVERIFY(actual1 == expected1);
   QVERIFY(actual2 == expected2);
}

void TreeTests::removeElements()
{
}
