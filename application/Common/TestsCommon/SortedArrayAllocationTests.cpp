#include <QTest>

#include <algorithm>
#include <cstdlib>
#include <new>

#include <Containers/SortedArray.h>

namespace
{
   thread_local int allocationsBeforeThrow = -1;

   class AllocationFailure
   {
   public:
      explicit AllocationFailure(int budget) { allocationsBeforeThrow = budget; }
      ~AllocationFailure() { allocationsBeforeThrow = -1; }
   };
}

// Keep allocation failure injection in a separate executable so it cannot affect
// unrelated Common tests. Enable it only around the container operation itself.
void* operator new(std::size_t size)
{
   if (allocationsBeforeThrow == 0)
      throw std::bad_alloc();
   if (allocationsBeforeThrow > 0)
      --allocationsBeforeThrow;
   if (void* result = std::malloc(size ? size : 1))
      return result;
   throw std::bad_alloc();
}

void operator delete(void* ptr) noexcept { std::free(ptr); }
void operator delete(void* ptr, std::size_t) noexcept { std::free(ptr); }

class SortedArrayAllocationTests : public QObject
{
   Q_OBJECT

private slots:
   void insertionFailure_data();
   void insertionFailure();
   void moveFailure_data();
   void moveFailure();

private:
   template<int M>
   void checkFailures(int count, bool shared, bool subscript, bool descending);
};

void SortedArrayAllocationTests::insertionFailure_data()
{
   QTest::addColumn<int>("order");
   QTest::addColumn<int>("count");
   QTest::addColumn<bool>("shared");
   QTest::addColumn<bool>("subscript");
   QTest::addColumn<bool>("descending");

   // Cover root splits, splits that stop below the root, and cascades through
   // several levels. Exercise the default order as well as the smallest order.
   for (int order : {3, 7})
      for (int count : (order == 3 ? QList<int>{2, 4, 6, 14, 62} : QList<int>{6, 10, 30}))
         for (bool shared : {false, true})
            for (bool subscript : {false, true})
               for (bool descending : {false, true})
               {
                  const QByteArray name = QByteArray::number(order) + '-' + QByteArray::number(count)
                     + (shared ? "-shared" : "-unique") + (subscript ? "-subscript" : "-insert")
                     + (descending ? "-descending" : "-ascending");
                  QTest::newRow(name.constData()) << order << count << shared << subscript << descending;
               }
}

void SortedArrayAllocationTests::insertionFailure()
{
   QFETCH(int, order);
   QFETCH(int, count);
   QFETCH(bool, shared);
   QFETCH(bool, subscript);
   QFETCH(bool, descending);
   if (order == 3)
      checkFailures<3>(count, shared, subscript, descending);
   else
      checkFailures<7>(count, shared, subscript, descending);
}

template<int M>
void SortedArrayAllocationTests::checkFailures(int count, bool shared, bool subscript, bool descending)
{
   const auto less = [descending](int a, int b) { return descending ? a > b : a < b; };
   for (int budget = 0; budget < 256; ++budget)
   {
      Common::SortedArray<int, M> array(less);
      for (int i = 1; i <= count; ++i)
         array.insert(i);
      Common::SortedArray<int, M> snapshot(less);
      if (shared)
         snapshot = array;
      const QList<int> before = array.toList();
      const int value = count + 1;
      bool threw = false;
      int result = -1;
      {
         AllocationFailure failure(budget);
         try
         {
            result = subscript ? array[value] : array.insert(value);
         }
         catch (const std::bad_alloc&)
         {
            threw = true;
         }
      }

      if (shared)
         QCOMPARE(snapshot.toList(), before);

      if (threw)
      {
         QCOMPARE(array.size(), count);
         QCOMPARE(array.toList(), before);
         QVERIFY(!array.contains(value));
         const auto& unchanged = array;
         for (int i = 0; i < count; ++i)
         {
            QCOMPARE(unchanged.getFromIndex(i), before[i]);
            QCOMPARE(array.indexOf(before[i]), i);
         }

         // A failed allocation must leave the tree usable for further mutations.
         result = subscript ? array[value] : array.insert(value);
      }

      QCOMPARE(result, subscript ? value : descending ? 0 : count);
      QList<int> after = before;
      after.append(value);
      std::sort(after.begin(), after.end(), less);
      QCOMPARE(array.toList(), after);
      QVERIFY(array.remove(value));
      QCOMPARE(array.toList(), before);
      while (!array.isEmpty())
         array.removeFromIndex(array.size() / 2);
      QCOMPARE(array.begin(), array.end());
      if (shared)
         QCOMPARE(snapshot.toList(), before);

      if (!threw)
      {
         QVERIFY(budget > 0); // Ensure this fixture exercised allocation failure.
         return;
      }
   }
   QFAIL("Insertion did not succeed after exhausting the allocation failure points");
}

void SortedArrayAllocationTests::moveFailure_data()
{
   QTest::addColumn<bool>("assignment");
   QTest::addColumn<bool>("shared");
   for (bool assignment : {false, true})
      for (bool shared : {false, true})
         QTest::newRow(assignment ? (shared ? "assignment-shared" : "assignment-unique")
            : (shared ? "construction-shared" : "construction-unique")) << assignment << shared;
}

void SortedArrayAllocationTests::moveFailure()
{
   QFETCH(bool, assignment);
   QFETCH(bool, shared);
   using Array = Common::SortedArray<int, 3>;
   for (int budget = 0; budget < 32; ++budget)
   {
      Array source([](int a, int b) { return a > b; });
      for (int i = 0; i < 40; ++i)
         source.insert(i);
      const QList<int> before = source.toList();
      Array snapshot;
      if (shared)
         snapshot = source;
      Array destination;
      destination.insert(100);
      bool threw = false;
      {
         AllocationFailure failure(budget);
         try
         {
            if (assignment)
               destination = std::move(source);
            else
            {
               Array moved(std::move(source));
               destination = moved; // Copy assignment does not allocate.
            }
         }
         catch (const std::bad_alloc&) { threw = true; }
      }
      if (shared)
         QCOMPARE(snapshot.toList(), before);
      if (threw)
      {
         QCOMPARE(source.toList(), before);
         QCOMPARE(destination.toList(), (QList<int>{100}));
         source.insert(99);
         QCOMPARE(source.getFromIndex(0), 99); // Source ordering retained.
         destination.insert(1);
         QCOMPARE(destination.getFromIndex(0), 1); // Destination ordering retained.
      }
      else
      {
         QCOMPARE(destination.toList(), before);
         QVERIFY(source.isEmpty());
         source.clear();
         source.insert(1);
         source.insert(2);
         QCOMPARE(source.toList(), (QList<int>{2, 1}));
         QVERIFY(budget > 0);
         return;
      }
   }
   QFAIL("Move did not succeed after exhausting the allocation failure points");
}

QTEST_APPLESS_MAIN(SortedArrayAllocationTests)
#include "SortedArrayAllocationTests.moc"
