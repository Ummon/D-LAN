#include <QTest>
#include <QDebug>

#include <algorithm>
#include <functional>
#include <random>
#include <vector>

#include <Containers/SortedArray.h>

/**
  * Checks the B-tree structure behind the public interface.
  */
struct Common::SortedArrayTestAccess
{
   template<typename T, int M>
   static void checkTree(const SortedArray<T, M>& array)
   {
      int leafDepth = -1;
      int size = 0;
      checkNode<T, M>(array.d.constData()->root, nullptr, 0, leafDepth, size);
   }

private:
   template<typename T, int M>
   static void checkNode(const typename SortedArray<T, M>::Node* node, const typename SortedArray<T, M>::Node* parent,
      int depth, int& leafDepth, int& size)
   {
      QVERIFY(node->parent == parent);
      QVERIFY(node->nbItems <= M - 1);
      if (parent)
         QVERIFY(node->nbItems >= M / 2);

      for (int i = node->nbItems + 1; i < M; ++i)
         QVERIFY(node->children[i] == nullptr);

      const bool leaf = node->children[0] == nullptr;
      size = node->nbItems;
      for (int i = 0; i <= node->nbItems; ++i)
      {
         QCOMPARE(node->children[i] == nullptr, leaf);
         if (!leaf)
         {
            int childSize = 0;
            checkNode<T, M>(node->children[i], node, depth + 1, leafDepth, childSize);
            if (QTest::currentTestFailed())
               return;
            size += childSize;
         }
      }

      if (leaf)
      {
         if (leafDepth == -1)
            leafDepth = depth;
         QCOMPARE(depth, leafDepth); // All leaves are at the same depth.
      }
      QCOMPARE(node->size, size);
   }
};

class SortedArrayInvariantTests : public QObject
{
   Q_OBJECT

private slots:
   void randomOperations_data();
   void randomOperations();
   void setSortedFunctionBuild_data();
   void setSortedFunctionBuild();

private:
   template<int M>
   void randomOperations(unsigned int seed);

   template<int M>
   void setSortedFunctionBuild();

   template<int M>
   void checkBuild(int count);

   template<int M>
   void checkAgainstReference(const Common::SortedArray<int, M>& array, const std::vector<int>& reference,
      const std::function<bool(int, int)>& less);
};

namespace
{
   const int MAX_VALUE = 63; // Small value range, so operations often hit existing items.
}

void SortedArrayInvariantTests::randomOperations_data()
{
   QTest::addColumn<int>("order");
   for (int order : {3, 5, 7, 9})
      QTest::newRow(QByteArray::number(order).constData()) << order;
}

void SortedArrayInvariantTests::randomOperations()
{
   QFETCH(int, order);
   for (unsigned int seed = 1; seed <= 4 && !QTest::currentTestFailed(); ++seed)
      switch (order)
      {
      case 3: randomOperations<3>(seed); break;
      case 5: randomOperations<5>(seed); break;
      case 7: randomOperations<7>(seed); break;
      default: randomOperations<9>(seed); break;
      }
}

/**
  * Applies random mutations and compares every query to a sorted std::vector after each one.
  */
template<int M>
void SortedArrayInvariantTests::randomOperations(unsigned int seed)
{
   std::mt19937 random(seed);
   bool descending = false;
   const auto less = [&descending](int a, int b) { return descending ? a > b : a < b; };
   const auto lowerBound = [&](const std::vector<int>& reference, int value) {
      return std::lower_bound(reference.begin(), reference.end(), value, less);
   };
   const auto contains = [&](const std::vector<int>& reference, std::vector<int>::const_iterator i, int value) {
      return i != reference.end() && *i == value;
   };

   Common::SortedArray<int, M> array;
   std::vector<int> reference;

   for (int step = 0; step < 1500; ++step)
   {
      const int operation = random() % 100;
      const int value = random() % (MAX_VALUE + 1);
      const auto i = lowerBound(reference, value);
      const bool present = contains(reference, i, value);

      if (operation < 45)
      {
         bool exists;
         QCOMPARE(array.insert(value, &exists), int(i - reference.begin()));
         QCOMPARE(exists, present);
         if (!present)
            reference.insert(i, value);
      }
      else if (operation < 70)
      {
         QCOMPARE(array.remove(value), present);
         if (present)
            reference.erase(i);
      }
      else if (operation < 85)
      {
         if (!reference.empty())
         {
            const int index = random() % reference.size();
            array.removeFromIndex(index);
            reference.erase(reference.begin() + index);
         }
      }
      else if (operation < 92)
      {
         QCOMPARE(array[value], value);
         if (!present)
            reference.insert(i, value);
      }
      else if (operation < 95)
      {
         // Mutating a copy must leave the original untouched.
         Common::SortedArray<int, M> copy = array;
         copy.insert(MAX_VALUE + 1);
         copy.remove(value);
         Common::SortedArrayTestAccess::checkTree(copy);
      }
      else if (operation < 97)
      {
         descending = !descending;
         array.setSortedFunction([descending](const int& a, const int& b) { return descending ? a > b : a < b; });
         std::sort(reference.begin(), reference.end(), less);
      }
      else if (operation < 98)
      {
         Common::SortedArray<int, M> moved(std::move(array));
         QVERIFY(array.isEmpty());
         array = std::move(moved);
      }

      checkAgainstReference<M>(array, reference, less);
      if (QTest::currentTestFailed())
      {
         qWarning() << "Order" << M << "seed" << seed << "step" << step << "operation" << operation << "value" << value;
         return;
      }
   }
}

template<int M>
void SortedArrayInvariantTests::checkAgainstReference(const Common::SortedArray<int, M>& array, const std::vector<int>& reference,
   const std::function<bool(int, int)>& less)
{
   Common::SortedArrayTestAccess::checkTree(array);
   if (QTest::currentTestFailed())
      return;

   QCOMPARE(array.size(), int(reference.size()));
   QCOMPARE(array.toList(), QList<int>(reference.begin(), reference.end()));
   for (int index = 0; index < int(reference.size()); ++index)
      QCOMPARE(array.getFromIndex(index), reference[index]);

   // Include values outside the range at both ends.
   for (int value = -1; value <= MAX_VALUE + 1; ++value)
   {
      const auto i = std::lower_bound(reference.begin(), reference.end(), value, less);
      const bool present = i != reference.end() && *i == value;
      const int index = int(i - reference.begin());
      const int nearest = reference.empty() ? -1 : present ? index : std::max(index - 1, 0);

      QCOMPARE(array.contains(value), present);
      QCOMPARE(array.indexOf(value), present ? index : -1);
      QCOMPARE(array.indexOfNearest(value), nearest);
      if (nearest != -1)
         QCOMPARE(*array.iteratorOfNearest(value), reference[nearest]);
      if (present)
         QCOMPARE(*array.iteratorOf(value), value);
   }
}

void SortedArrayInvariantTests::setSortedFunctionBuild_data()
{
   QTest::addColumn<int>("order");
   for (int order : {3, 5, 7, 9})
      QTest::newRow(QByteArray::number(order).constData()) << order;
}

void SortedArrayInvariantTests::setSortedFunctionBuild()
{
   QFETCH(int, order);
   switch (order)
   {
   case 3: setSortedFunctionBuild<3>(); break;
   case 5: setSortedFunctionBuild<5>(); break;
   case 7: setSortedFunctionBuild<7>(); break;
   default: setSortedFunctionBuild<9>(); break;
   }
}

/**
  * Rebuilds trees of every small size, and of the sizes around each height change, which
  * are the edge cases of the bottom-up build.
  */
template<int M>
void SortedArrayInvariantTests::setSortedFunctionBuild()
{
   std::vector<int> counts;
   for (int count = 0; count <= 200; ++count)
      counts.push_back(count);
   for (long long capacity = M * M * M; capacity <= 20000; capacity *= M)
      for (long long count : {capacity - 2, capacity - 1, capacity, capacity + 1})
         counts.push_back(int(count));

   for (int count : counts)
   {
      checkBuild<M>(count);
      if (QTest::currentTestFailed())
      {
         qWarning() << "Order" << M << "count" << count;
         return;
      }
   }
}

template<int M>
void SortedArrayInvariantTests::checkBuild(int count)
{
   const auto groupOf = [](const int& a, const int& b) { return a / 4 < b / 4; };

   for (bool fromDescending : {false, true})
   {
      Common::SortedArray<int, M> array;
      for (int value = 0; value < count; ++value)
         array.insert(value);
      if (fromDescending)
         array.setSortedFunction([](const int& a, const int& b) { return a > b; });

      std::vector<int> expected(count);
      for (int i = 0; i < count; ++i)
         expected[i] = fromDescending ? count - 1 - i : i;
      Common::SortedArrayTestAccess::checkTree(array);
      QCOMPARE(array.toList(), QList<int>(expected.begin(), expected.end()));

      // Equivalent values collapse to the last one in the old iteration order.
      array.setSortedFunction(groupOf);
      expected.clear();
      for (int group = 0; group * 4 < count; ++group)
         expected.push_back(fromDescending ? group * 4 : std::min(group * 4 + 3, count - 1));
      Common::SortedArrayTestAccess::checkTree(array);
      QCOMPARE(array.toList(), QList<int>(expected.begin(), expected.end()));
   }
}

QTEST_APPLESS_MAIN(SortedArrayInvariantTests)
#include "SortedArrayInvariantTests.moc"
