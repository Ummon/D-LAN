#include <QtTest>
#include <QAbstractItemModelTester>
#include <QItemSelectionModel>
#include <QTemporaryDir>

#include <Chat/RoomsModel.h>
#include <Common/Global.h>
#include <Common/RemoteCoreController/Builder.h>

namespace
{
   void setPeers(Protos::GUI::State::Room* room, int count)
   {
      room->clear_peer_ids();
      for (int i = 0; i < count; ++i)
         room->add_peer_ids()->set_hash(QByteArray(Common::Hash::HASH_SIZE, char(i + 1)).toStdString());
   }

   struct Fixture
   {
      QSharedPointer<RCC::ICoreConnection> connection = RCC::Builder::newCoreConnection(1000);
      GUI::RoomsModel model { connection };
      QAbstractItemModelTester tester { &model, QAbstractItemModelTester::FailureReportingMode::QtTest };
      Protos::GUI::State state;

      void add(const QString& name, int peers, bool joined = false)
      {
         auto* room = state.add_rooms();
         room->set_name(name.toStdString());
         room->set_joined(joined);
         setPeers(room, peers);
      }
      void send() { emit connection->newState(state); }
      QStringList names() const
      {
         QStringList result;
         for (int row = 0; row < model.rowCount(); ++row)
            result.append(model.index(row, 0).data().toString());
         return result;
      }
   };
}

class TestsRoomsModel : public QObject
{
   Q_OBJECT
private slots:
   void insertsAtSortedBoundaries_data()
   {
      QTest::addColumn<bool>("byCount");
      QTest::newRow("by-name") << false;
      QTest::newRow("by-peer-count") << true;
   }

   void insertsAtSortedBoundaries()
   {
      QFETCH(bool, byCount);
      Fixture f;
      if (byCount)
         f.model.setSortType(Protos::GUI::Settings::BY_NB_PEERS);
      QSignalSpy inserts(&f.model, &QAbstractItemModel::rowsInserted);
      QSignalSpy layouts(&f.model, &QAbstractItemModel::layoutChanged);
      QSignalSpy moves(&f.model, &QAbstractItemModel::rowsMoved);
      int expectedRow = 0;
      QString insertedName;
      int oldCount = 0;
      connect(&f.model, &QAbstractItemModel::rowsAboutToBeInserted, this,
         [&](const QModelIndex& parent, int first, int last) {
            QVERIFY(!parent.isValid());
            QCOMPARE(first, expectedRow);
            QCOMPARE(last, expectedRow);
            QCOMPARE(f.model.rowCount(), oldCount);
            QVERIFY(!f.names().contains(insertedName));
         });
      connect(&f.model, &QAbstractItemModel::rowsInserted, this,
         [&](const QModelIndex&, int first, int) {
            QCOMPARE(f.model.rowCount(), oldCount + 1);
            QCOMPARE(f.model.index(first, 0).data().toString(), insertedName);
         });
      const QStringList input { "Charlie", "Alpha", "Zulu", "Beta" };
      const QList<int> positions { 0, 0, 2, 1 };
      const QList<int> counts { 2, 3, 1, 2 };
      QPersistentModelIndex charlie;
      for (int i = 0; i < input.size(); ++i)
      {
         oldCount = i;
         insertedName = input[i];
         expectedRow = positions[i];
         f.add(insertedName, counts[i]);
         f.send();
         if (i == 0)
            charlie = f.model.index(0, 0);
         QCOMPARE(charlie.data().toString(), QString("Charlie"));
      }
      QCOMPARE(f.names(), QStringList({ "Alpha", "Beta", "Charlie", "Zulu" }));
      QCOMPARE(charlie.row(), 2);
      QCOMPARE(inserts.count(), 4);
      QVERIFY(layouts.isEmpty());
      QVERIFY(moves.isEmpty());
   }

   void countChangesMoveRowsAndPreserveSelection()
   {
      Fixture f;
      f.model.setSortType(Protos::GUI::Settings::BY_NB_PEERS);
      f.add("Alpha", 3);
      f.add("Beta", 2);
      f.add("Zulu", 1);
      f.send();
      QPersistentModelIndex selected = f.model.index(2, 0);
      QPersistentModelIndex countIndex = f.model.index(2, 1);
      QItemSelectionModel selection(&f.model);
      selection.select(selected, QItemSelectionModel::Select | QItemSelectionModel::Rows);
      QSignalSpy moves(&f.model, &QAbstractItemModel::rowsMoved);
      QSignalSpy changes(&f.model, &QAbstractItemModel::dataChanged);
      QSignalSpy layouts(&f.model, &QAbstractItemModel::layoutChanged);
      int expectedOld = 2;
      int expectedDestination = 0;
      qsizetype previousCount = 1;
      connect(&f.model, &QAbstractItemModel::rowsAboutToBeMoved, this,
         [&](const QModelIndex&, int first, int last, const QModelIndex&, int destination) {
            QCOMPARE(first, expectedOld);
            QCOMPARE(last, expectedOld);
            QCOMPARE(destination, expectedDestination);
            QCOMPARE(countIndex.data().toLongLong(), previousCount);
         });

      setPeers(f.state.mutable_rooms(2), 4);
      f.send();
      QCOMPARE(f.names(), QStringList({ "Zulu", "Alpha", "Beta" }));
      QCOMPARE(selected.row(), 0);
      QCOMPARE(countIndex.data().toInt(), 4);
      QVERIFY(selection.isSelected(selected));
      QCOMPARE(moves.count(), 1);

      expectedOld = 0;
      expectedDestination = 3; // Qt's destination uses the rows before removal.
      previousCount = 4;
      setPeers(f.state.mutable_rooms(2), 2);
      f.send();
      QCOMPARE(f.names(), QStringList({ "Alpha", "Beta", "Zulu" }));
      QCOMPARE(selected.row(), 2); // Equal counts use name as the tie breaker.
      QCOMPARE(moves.count(), 2);

      // Joining counts as one additional participant and moves Zulu between the others.
      expectedOld = 2;
      expectedDestination = 1;
      previousCount = 2;
      f.state.mutable_rooms(2)->set_joined(true);
      f.send();
      QCOMPARE(f.names(), QStringList({ "Alpha", "Zulu", "Beta" }));
      QCOMPARE(selected.row(), 1);
      QCOMPARE(countIndex.data().toInt(), 3);
      QCOMPARE(moves.count(), 3);
      QCOMPARE(changes.count(), 3);
      QVERIFY(layouts.isEmpty());
      QVERIFY(selection.isSelected(selected));
      f.send();
      QCOMPARE(moves.count(), 3);
      QCOMPARE(changes.count(), 3);
   }

   void unchangedRankAndSortModeChanges()
   {
      Fixture f;
      f.add("Alpha", 1);
      f.add("Zulu", 2);
      f.send();
      QPersistentModelIndex alpha = f.model.index(0, 0);
      QPersistentModelIndex zulu = f.model.index(1, 0);
      QSignalSpy moves(&f.model, &QAbstractItemModel::rowsMoved);
      QSignalSpy changes(&f.model, &QAbstractItemModel::dataChanged);
      setPeers(f.state.mutable_rooms(1), 5);
      f.send();
      QCOMPARE(f.names(), QStringList({ "Alpha", "Zulu" }));
      QVERIFY(moves.isEmpty());
      QCOMPARE(changes.count(), 1);
      f.model.setSortType(Protos::GUI::Settings::BY_NB_PEERS);
      QCOMPARE(alpha.row(), 1);
      QCOMPARE(zulu.row(), 0);
      setPeers(f.state.mutable_rooms(1), 6); // Sort key changes but rank stays the same.
      f.send();
      QVERIFY(moves.isEmpty());
      QCOMPARE(f.model.index(0, 1).data().toInt(), 6);
      // Membership changes with an unchanged count must not move the room either.
      f.state.mutable_rooms(1)->mutable_peer_ids(0)->set_hash(QByteArray(Common::Hash::HASH_SIZE, 'x').toStdString());
      f.send();
      QVERIFY(moves.isEmpty());
      QCOMPARE(changes.count(), 3);
      f.model.setSortType(Protos::GUI::Settings::BY_NAME);
      QCOMPARE(alpha.row(), 0);
      QCOMPARE(zulu.row(), 1);
      QCOMPARE(alpha.data().toString(), QString("Alpha"));
      QCOMPARE(zulu.data().toString(), QString("Zulu"));
   }

   void mixedSnapshotsAcrossTreeBoundaries()
   {
      Fixture f;
      f.model.setSortType(Protos::GUI::Settings::BY_NB_PEERS);
      for (int revision = 0; revision < 6; ++revision)
      {
         f.state.clear_rooms();
         QList<QPair<QString, int>> expected;
         for (int i = 100; i >= 0; --i)
         {
            if ((i + revision) % 7 == 0)
               continue;
            const QString name = QString("room-%1").arg(i, 3, 10, QChar('0'));
            const int peers = (i * 3 + revision * 7) % 20;
            const bool joined = (i + revision) % 3 == 0;
            f.add(name, peers, joined);
            expected.append({ name, peers + int(joined) });
         }
         std::sort(expected.begin(), expected.end(), [](const auto& a, const auto& b) {
            return a.second == b.second ? a.first < b.first : a.second > b.second;
         });
         f.send();
         QCOMPARE(f.model.rowCount(), expected.size());
         for (int row = 0; row < expected.size(); ++row)
         {
            QCOMPARE(f.model.index(row, 0).data().toString(), expected[row].first);
            QCOMPARE(f.model.index(row, 1).data().toInt(), expected[row].second);
         }
      }
      QPersistentModelIndex removed = f.model.index(0, 0);
      emit f.connection->disconnected(false);
      QCOMPARE(f.model.rowCount(), 0);
      QVERIFY(!removed.isValid());
      QVERIFY(f.model.getRoomName(QModelIndex()).isEmpty());
   }
};

int main(int argc, char** argv)
{
   QCoreApplication app(argc, argv);
   QTemporaryDir data;
   if (!data.isValid())
      return 1;
   Common::Global::setDataFolder(Common::Global::DataFolderType::LOCAL, data.path());
   Common::Global::setDataFolder(Common::Global::DataFolderType::ROAMING, data.path());
   TestsRoomsModel tests;
   return QTest::qExec(&tests, argc, argv);
}
#include "TestsRoomsModel.moc"
