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

QTEST_APPLESS_MAIN(SortedListTests)
#include "SortedListTests.moc"
