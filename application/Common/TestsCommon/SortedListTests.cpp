#include <QTest>
#include <QRandomGenerator64>

#include <algorithm>

#include <Containers/SortedList.h>
using namespace Common;

class SortedListTests : public QObject
{
   Q_OBJECT

private slots:
   void defaultOrdering();
   void sortedListEquivalentItems();
   void sortedListInsertionComparisons_data();
   void sortedListInsertionComparisons();
   void sortedListGetItems();
   void sortedListUpdateItem();
   void sortedListUpdateItemComplexity();
};

void SortedListTests::defaultOrdering()
{
   SortedList<int> list;
   list.insert(16);
   list.insert(2);
   list.insert(9);
   list.insert(2);
   QCOMPARE(list.getList(), (QList<int> { 2, 9, 16 }));
   list.insert(QList<int> { 1, 2, 2, 4, 9, 32 });
   QCOMPARE(list.getList(), (QList<int> { 1, 2, 4, 9, 16, 32 }));
   list.removeOne(9);
   list.removeOne(42);
   QCOMPARE(list.getList(), (QList<int> { 1, 2, 4, 16, 32 }));
   list.clear();
   QVERIFY(list.getList().isEmpty());
   list.insert(7);
   QCOMPARE(list.getList(), (QList<int> { 7 }));
}

void SortedListTests::sortedListEquivalentItems()
{
   QString first = QStringLiteral("Alpha");
   QString second = QStringLiteral("ALPHA");
   QString third = QStringLiteral("alpha");
   QString before = QStringLiteral("0");
   QString after = QStringLiteral("Zulu");
   SortedList<QString*, QString> list([](const QString* const& str) { return str->toLower(); });

   list.insert(&after);
   list.insert(&first);
   list.insert(&second);
   list.insert(&before);
   list.insert(&third);
   const QList<QString*> expected { &before, &first, &second, &third, &after };
   QCOMPARE(list.getList(), expected);
   for (QString* item : expected)
      list.insert(item);
   QCOMPARE(list.getList(), expected);

   // Readers retain snapshots while directory entries are added or renamed.
   const auto snapshot = list.getList();
   second = QStringLiteral("zz");
   list.itemChanged(&second);
   QCOMPARE(list.getList(), (QList<QString*> { &before, &first, &third, &after, &second }));
   QCOMPARE(snapshot, expected);
   second = QStringLiteral("ALPHA");
   list.itemChanged(&second);
   QCOMPARE(list.getList(), (QList<QString*> { &before, &first, &third, &second, &after }));

   list.removeOne(&third);
   list.insert(QList<QString*> { &first, &third, &after });
   QCOMPARE(list.getList(), (QList<QString*> { &before, &first, &second, &third, &after }));
}

void SortedListTests::sortedListInsertionComparisons_data()
{
   QTest::addColumn<int>("order");
   QTest::newRow("ascending") << 0;
   QTest::newRow("descending") << 1;
   QTest::newRow("shuffled") << 2;
}

void SortedListTests::sortedListInsertionComparisons()
{
   QFETCH(int, order);
   constexpr int count = 28048;
   QList<int> items;
   for (int i = 0; i < count; ++i)
      items.append(i);
   const auto expected = items;
   if (order == 1)
      std::reverse(items.begin(), items.end());
   else if (order == 2)
   {
      QRandomGenerator64 rng(42);
      std::shuffle(items.begin(), items.end(), rng);
   }

   qint64 keyExtractions = 0;
   SortedList<int> list([&keyExtractions](int a) { ++keyExtractions; return a; });
   for (int item : items)
      list.insert(item);
   QCOMPARE(list.getList(), expected);
   // Deterministic complexity checks instead of timing thresholds: sorted input
   // should append cheaply, and arbitrary input must not scan every prior item.
   // Each comparison extracts two keys, so allow twice the comparison budget.
   const qint64 maxKeyExtractions = 2 * qint64(count) * (order == 0 ? 1 : 20);
   QVERIFY2(keyExtractions <= maxKeyExtractions, qPrintable(QString("Key extractions: %1, maximum: %2")
      .arg(keyExtractions).arg(maxKeyExtractions)));
   for (int item : items)
      list.insert(item);
   QCOMPARE(list.getList(), expected);
}

void SortedListTests::sortedListGetItems()
{
   QString first = QStringLiteral("Alpha");
   QString second = QStringLiteral("ALPHA");
   QString third = QStringLiteral("alpha");
   QString before = QStringLiteral("0");
   QString after = QStringLiteral("Zulu");
   SortedList<QString*, QString> list([](const QString* const& str) { return str->toLower(); });

   QVERIFY(list.getItems("alpha").isEmpty());

   list.insert(&after);
   list.insert(&first);
   list.insert(&second);
   list.insert(&before);
   list.insert(&third);

   QCOMPARE(list.getItems("alpha"), (QList<QString*> { &first, &second, &third }));
   QCOMPARE(list.getItems("0"), (QList<QString*> { &before }));
   QCOMPARE(list.getItems("zulu"), (QList<QString*> { &after }));
   QVERIFY(list.getItems("XXX").isEmpty());
}

void SortedListTests::sortedListUpdateItem()
{
   QString first = "Alpha", second = "ALPHA", third = "alpha", before = "0", after = "Zulu";
   const auto key = [](const QString* str) { return str->toLower(); };
   SortedList<QString*, QString> list(key);
   QString absent = "ALPHA";
   bool called = false;
   const auto absentUpdate = [&] { called = true; };
   QVERIFY(!list.updateItem(&absent, absentUpdate));
   QVERIFY(!called);
   for (auto item : { &before, &first, &second, &third, &after })
      list.insert(item);
   const auto snapshot = list.getList();

   // Missing identity within an equivalent-key group must not modify the list.
   QVERIFY(!list.updateItem(&absent, absentUpdate));
   QVERIFY(!called);
   absent = "middle";
   QVERIFY(!list.updateItem(&absent, absentUpdate));
   absent = "zzzz";
   QVERIFY(!list.updateItem(&absent, absentUpdate));
   QVERIFY(!called);

   // A changed item goes after existing equivalents, including on a no-op update.
   QVERIFY(list.updateItem(&first, [] {}));
   QCOMPARE(list.getList(), (QList<QString*> { &before, &second, &third, &first, &after }));
   QVERIFY(list.updateItem(&after, [&] { after = "Alpha"; }));
   QCOMPARE(list.getList(), (QList<QString*> { &before, &second, &third, &first, &after }));
   QCOMPARE(snapshot, (QList<QString*> { &before, &first, &second, &third, &after }));

   // Moving across either boundary and back into the middle must preserve ordering.
   QVERIFY(list.updateItem(list.getList()[2], [&] { third = "!"; }));
   QCOMPARE(list.getList(), (QList<QString*> { &third, &before, &second, &first, &after }));
   QVERIFY(list.updateItem(&before, [&] { before = "zz"; }));
   QCOMPARE(list.getList(), (QList<QString*> { &third, &second, &first, &after, &before }));
   QVERIFY(list.updateItem(&third, [&] { third = "beta"; }));
   QCOMPARE(list.getList(), (QList<QString*> { &second, &first, &after, &third, &before }));
   QVERIFY(list.updateItem(&before, [&] { before = "alpha"; }));
   QCOMPARE(list.getList(), (QList<QString*> { &second, &first, &after, &before, &third }));

   SortedList<int> defaultKeys;
   defaultKeys.insert(7);
   QVERIFY(defaultKeys.updateItem(7, [&] { called = true; }));
   QVERIFY(called);
   QCOMPARE(defaultKeys.getList(), (QList<int> { 7 }));
}

void SortedListTests::sortedListUpdateItemComplexity()
{
   // Count identity comparisons too: counting only key reads would miss a linear
   // contains()/removeOne() scan before an otherwise logarithmic search.
   struct Item
   {
      int* key;
      qint64* comparisons;
      bool operator<(const Item& other) const { return *this->key < *other.key; }
      bool operator==(const Item& other) const { ++*this->comparisons; return this->key == other.key; }
   };
   constexpr int count = 28048;
   QList<int> keys;
   keys.reserve(count);
   for (int i = 0; i < count; ++i)
      keys.append(2 * i);
   qint64 comparisons = 0, keyExtractions = 0;
   SortedList<Item, int> list([&](const Item& item) { ++keyExtractions; return *item.key; });
   for (int& key : keys)
      list.insert(Item { &key, &comparisons });
   const auto snapshot = list.getList();
   comparisons = keyExtractions = 0;
   for (int& key : keys)
      QVERIFY(list.updateItem(Item { &key, &comparisons }, [&] { ++key; }));
   QVERIFY2(comparisons <= count, qPrintable(QString::number(comparisons)));
   QVERIFY2(keyExtractions <= qint64(count) * 30, qPrintable(QString::number(keyExtractions)));
   // In-place key changes must not detach a shared list or move its elements.
   QCOMPARE(list.getList().constData(), snapshot.constData());
   for (int i = 0; i < count; ++i)
      QCOMPARE(*list.getList()[i].key, 2 * i + 1);
}

QTEST_APPLESS_MAIN(SortedListTests)
#include "SortedListTests.moc"
