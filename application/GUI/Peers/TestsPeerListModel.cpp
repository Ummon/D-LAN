#include <QtTest>
#include <QAbstractItemModelTester>
#include <QItemSelectionModel>
#include <QPersistentModelIndex>
#include <QTemporaryDir>

#include <Common/Global.h>
#include <Common/ProtoHelper.h>
#include <Common/RemoteCoreController/Builder.h>
#include <Peers/PeerListModel.h>

namespace
{
   struct Fixture
   {
      QSharedPointer<RCC::ICoreConnection> connection = RCC::Builder::newCoreConnection(1000);
      GUI::PeerListModel model { connection };
      QAbstractItemModelTester tester { &model, QAbstractItemModelTester::FailureReportingMode::QtTest };
      Protos::GUI::State state;
      QList<Common::Hash> ids;

      void add(const char* nick, quint64 amount)
      {
         const auto id = Common::Hash::rand();
         this->ids.append(id);
         auto* peer = this->state.add_peers();
         peer->mutable_peer_id()->set_hash(id.getData(), Common::Hash::HASH_SIZE);
         peer->set_nick(nick);
         peer->set_sharing_amount(amount);
         peer->set_core_version("1.0");
      }
      void send() { emit this->connection->newState(this->state); }
      int row(const Common::Hash& id) const
      {
         for (int i = 0; i < this->model.rowCount(); ++i)
            if (this->model.getPeerID(i) == id)
               return i;
         return -1;
      }
   };
}

class TestsPeerListModel : public QObject
{
   Q_OBJECT
private slots:
   void displayChangesOnlyNotifyAffectedRows()
   {
      Fixture f;
      for (int i = 0; i < 5; ++i)
         f.add("peer", 500 - i * 100);
      f.send();
      QSignalSpy changes(&f.model, &QAbstractItemModel::dataChanged);
      QSignalSpy layouts(&f.model, &QAbstractItemModel::layoutChanged);
      QSignalSpy aboutLayouts(&f.model, &QAbstractItemModel::layoutAboutToBeChanged);
      QSignalSpy moves(&f.model, &QAbstractItemModel::rowsMoved);
      QSignalSpy inserts(&f.model, &QAbstractItemModel::rowsInserted);
      QSignalSpy removes(&f.model, &QAbstractItemModel::rowsRemoved);
      QPersistentModelIndex selected(f.model.index(1, 1));

      f.state.mutable_peers(1)->set_download_rate(1000);
      f.state.mutable_peers(2)->set_core_version("2.0");
      f.state.mutable_peers(4)->set_status(Protos::GUI::State::Peer::VERSION_OUTDATED);
      auto* upload = f.state.add_uploads();
      upload->mutable_peer_id()->set_hash(f.ids[1].getData(), Common::Hash::HASH_SIZE);
      f.send();
      QCOMPARE(changes.size(), 2); // Adjacent changed rows share a notification.
      QCOMPARE(changes[0][0].value<QModelIndex>(), f.model.index(1, 0));
      QCOMPARE(changes[0][1].value<QModelIndex>(), f.model.index(2, 2));
      QCOMPARE(changes[1][0].value<QModelIndex>(), f.model.index(4, 0));
      QCOMPARE(changes[1][1].value<QModelIndex>(), f.model.index(4, 2));
      QVERIFY(layouts.isEmpty() && aboutLayouts.isEmpty() && moves.isEmpty() && inserts.isEmpty() && removes.isEmpty());
      QCOMPARE(selected.row(), 1);
      const auto transfer = f.model.index(1, 0).data().value<GUI::PeerListModel::TransferInformation>();
      QCOMPARE(transfer.downloadRate, quint32(1000));
      QVERIFY(transfer.isDownloadingOurData);
      QVERIFY(f.model.index(2, 1).data(Qt::ToolTipRole).toString().contains("2.0"));
      QCOMPARE(f.model.index(4, 1).data(Qt::ForegroundRole).value<QColor>(), GUI::PeerListModel::COLOR_PEER_ERROR);

      changes.clear();
      f.send();
      QVERIFY(changes.isEmpty());
      Common::ProtoHelper::setIP(*f.state.mutable_peers(1)->mutable_ip(), QHostAddress("127.0.0.2"));
      f.send();
      QCOMPARE(f.model.getPeerIP(1), QHostAddress("127.0.0.2"));
      QVERIFY(changes.isEmpty() && layouts.isEmpty());

      f.state.clear_uploads();
      f.send();
      QCOMPARE(changes.size(), 1);
      QVERIFY(!f.model.index(1, 0).data().value<GUI::PeerListModel::TransferInformation>().isDownloadingOurData);
      changes.clear();
      f.model.colorize(f.ids[1], Qt::red);
      QCOMPARE(changes.size(), 1);
      QCOMPARE(changes[0][0].value<QModelIndex>().row(), 1);
      QCOMPARE(f.model.index(1, 0).data(Qt::BackgroundRole).value<QColor>(), QColor(Qt::red));
      QVERIFY(layouts.isEmpty());
   }

   void sortingChangesPreservePeerIdentity_data()
   {
      QTest::addColumn<bool>("byNick");
      QTest::newRow("nick") << true;
      QTest::newRow("sharing") << false;
   }

   void sortingChangesPreservePeerIdentity()
   {
      QFETCH(bool, byNick);
      Fixture f;
      f.model.setSortType(byNick ? Protos::GUI::Settings::BY_NICK : Protos::GUI::Settings::BY_SHARING_AMOUNT);
      f.add("A", 300);
      f.add("B", 200);
      f.add("C", 100);
      f.send();
      QPersistentModelIndex selected(f.model.index(1, 1));
      QItemSelectionModel selection(&f.model);
      selection.select(selected, QItemSelectionModel::Select | QItemSelectionModel::Rows);
      QSignalSpy moves(&f.model, &QAbstractItemModel::rowsMoved);
      QSignalSpy layouts(&f.model, &QAbstractItemModel::layoutChanged);
      QSignalSpy changes(&f.model, &QAbstractItemModel::dataChanged);

      // A sorting field can change without changing the peer's position.
      if (byNick)
         f.state.mutable_peers(1)->set_nick("b");
      else
         f.state.mutable_peers(1)->set_sharing_amount(201);
      f.send();
      QVERIFY(moves.isEmpty() && layouts.isEmpty());
      QCOMPARE(changes.size(), 1);

      // Exercise moves both to the end and to the beginning.
      for (bool toEnd : { true, false })
      {
         if (byNick)
            f.state.mutable_peers(1)->set_nick(toEnd ? "Z" : "0");
         else
            f.state.mutable_peers(1)->set_sharing_amount(toEnd ? 1 : 1000);
         f.send();
         QCOMPARE(selected.row(), toEnd ? 2 : 0);
         QCOMPARE(f.model.getPeerID(selected.row()), f.ids[1]);
         QCOMPARE(selection.selectedRows().size(), 1);
         QCOMPARE(f.model.getPeerID(selection.selectedRows()[0].row()), f.ids[1]);
      }
      QCOMPARE(moves.size(), 2);
      QVERIFY(layouts.isEmpty());

      // Explicit changes of sort mode also preserve persistent indexes.
      f.model.setSortType(byNick ? Protos::GUI::Settings::BY_SHARING_AMOUNT : Protos::GUI::Settings::BY_NICK);
      QCOMPARE(layouts.size(), 1);
      QCOMPARE(f.model.getPeerID(selected.row()), f.ids[1]);
      QCOMPARE(f.model.getPeerID(selection.selectedRows()[0].row()), f.ids[1]);
   }

   void mixedMembershipAndDataChanges()
   {
      Fixture f;
      f.add("A", 300);
      f.add("B", 200);
      f.add("C", 100);
      f.send();
      QPersistentModelIndex retained(f.model.index(1, 1));
      QPersistentModelIndex removed(f.model.index(0, 1));
      QSignalSpy changes(&f.model, &QAbstractItemModel::dataChanged);
      QSignalSpy removals(&f.model, &GUI::PeerListModel::peersRemoved);
      QSignalSpy layouts(&f.model, &QAbstractItemModel::layoutChanged);
      f.state.mutable_peers()->DeleteSubrange(0, 1);
      f.state.mutable_peers(0)->set_upload_rate(42);
      f.add("D", 400);
      f.send();
      QVERIFY(!removed.isValid());
      QCOMPARE(f.model.getPeerID(retained.row()), f.ids[1]);
      QCOMPARE(f.model.getPeerID(0), f.ids[3]);
      QCOMPARE(changes.size(), 1);
      QCOMPARE(changes[0][0].value<QModelIndex>().row(), retained.row());
      QCOMPARE(removals.size(), 1);
      QCOMPARE(removals[0][0].value<QList<Common::Hash>>(), QList<Common::Hash> { f.ids[0] });
      QVERIFY(layouts.isEmpty());

      f.model.setDisplayOnlyPeersWithStatusOK(true);
      f.state.mutable_peers(0)->set_status(Protos::GUI::State::Peer::VERSION_OUTDATED);
      f.send();
      QVERIFY(!retained.isValid());
      QCOMPARE(f.model.rowCount(), 2);
      f.state.mutable_peers(0)->set_status(Protos::GUI::State::Peer::OK);
      f.send();
      QCOMPARE(f.model.rowCount(), 3);

      f.model.setRoom("test");
      auto* room = f.state.add_rooms();
      room->set_name("test");
      room->add_peer_ids()->set_hash(f.ids[2].getData(), Common::Hash::HASH_SIZE);
      f.send();
      QCOMPARE(f.model.rowCount(), 1);
      QCOMPARE(f.model.getPeerID(0), f.ids[2]);
      emit f.connection->disconnected(false);
      QCOMPARE(f.model.rowCount(), 0);
      QVERIFY(layouts.isEmpty());
   }
};

int main(int argc, char** argv)
{
   qInstallMessageHandler(nullptr);
   QApplication app(argc, argv);
   QTemporaryDir data;
   if (!data.isValid())
      return 1;
   Common::Global::setDataFolder(Common::Global::DataFolderType::LOCAL, data.path());
   Common::Global::setDataFolder(Common::Global::DataFolderType::ROAMING, data.path());
   // The real delegate paints this DisplayRole payload as a chart. The generic
   // model tester also requires DisplayRole values to be convertible to text.
   QMetaType::registerConverter<GUI::PeerListModel::TransferInformation, QString>([](const auto& transfer) {
      return QString("%1/%2/%3").arg(transfer.downloadRate).arg(transfer.uploadRate).arg(transfer.isDownloadingOurData);
   });
   TestsPeerListModel tests;
   return QTest::qExec(&tests, argc, argv);
}

#include "TestsPeerListModel.moc"
