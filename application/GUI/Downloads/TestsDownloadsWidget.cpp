#include <QtTest>
#include <QAbstractItemModelTester>
#include <QTemporaryDir>
#include <QTreeView>
#include <limits>

#include <Common/Global.h>
#include <Common/Settings.h>
#include <Common/RemoteCoreController/priv/CoreConnection.h>
#include <Downloads/DownloadsWidget.h>
#include <Utils.h>

// Opening external applications is outside these widget tests.
void GUI::Utils::openLocations(const QStringList&, QWidget*) { QFAIL("Unexpected openLocations"); }
void GUI::Utils::openFile(const QString&) { QFAIL("Unexpected openFile"); }

namespace
{
   class Connection : public RCC::CoreConnection
   {
   public:
      QList<quint64> references;
      QList<quint64> moved;
      void moveDownloads(const QList<quint64>& refs, const QList<quint64>& ids,
         Protos::GUI::MoveDownloads::Position) override
      {
         this->references = refs;
         this->moved = ids;
      }
   };

   struct Fixture
   {
      QSharedPointer<Connection> connection = QSharedPointer<Connection>::create();
      GUI::PeerListModel peers { connection };
      GUI::SharedEntryListModel shares;
      GUI::DownloadsWidget widget { connection, peers, shares };
      QTreeView* view = widget.findChild<QTreeView*>("tblDownloads");
      Protos::GUI::State state;

      void add(quint64 id, const char* path = "/", Protos::Common::DownloadStatus status = Protos::Common::DOWNLOADING)
      {
         auto* download = this->state.add_downloads();
         download->set_id(id);
         download->set_status(status);
         auto* entry = download->mutable_local_entry();
         entry->set_name(QString("file-%1.bin").arg(id).toStdString());
         entry->set_path(path);
         entry->set_type(Protos::Common::Entry::FILE);
         entry->set_size(10000);
      }

      void send() { emit this->connection->newState(this->state); }
      void switchView() { QVERIFY(QMetaObject::invokeMethod(&this->widget, "switchView")); }
      void showCompleted(bool show)
      {
         auto* filter = this->widget.findChild<GUI::CheckBoxList*>()->model();
         QVERIFY(filter->setData(filter->index(1, 0), show, Qt::UserRole));
      }
   };
}

class TestsDownloadsWidget : public QObject
{
   Q_OBJECT
private slots:
   void init()
   {
      SETTINGS.set("download_view", quint32(Protos::GUI::Settings::LIST_VIEW));
   }

   void inactiveRowsStayUntouchedAndSwitchCatchesUp()
   {
      Fixture f;
      auto* flat = qobject_cast<GUI::DownloadsFlatModel*>(f.view->model());
      QVERIFY(flat);
      f.add(1);
      f.send();
      f.switchView();
      auto* tree = qobject_cast<GUI::DownloadsTreeModel*>(f.view->model());
      QVERIFY(tree);
      QCOMPARE(tree->rowCount(), 1);
      f.switchView();
      QAbstractItemModelTester flatTester(flat, QAbstractItemModelTester::FailureReportingMode::QtTest);
      QAbstractItemModelTester treeTester(tree, QAbstractItemModelTester::FailureReportingMode::QtTest);
      QSignalSpy treeChanges(tree, &QAbstractItemModel::dataChanged);
      QSignalSpy treeInserts(tree, &QAbstractItemModel::rowsInserted);
      QSignalSpy treeRemoves(tree, &QAbstractItemModel::rowsRemoved);

      f.state.clear_downloads();
      f.add(2);
      f.add(3);
      f.send();
      QCOMPARE(flat->rowCount(), 2);
      QCOMPARE(tree->rowCount(), 1);
      QCOMPARE(tree->getDownloadIDs(tree->index(0, 0)), QList<quint64> { 1 });
      QVERIFY(treeChanges.isEmpty() && treeInserts.isEmpty() && treeRemoves.isEmpty());
      f.switchView();
      QCOMPARE(tree->rowCount(), 2);
      QCOMPARE(tree->getDownloadIDs(tree->index(0, 0)), QList<quint64> { 2 });

      QSignalSpy flatChanges(flat, &QAbstractItemModel::dataChanged);
      QSignalSpy flatRemoves(flat, &QAbstractItemModel::rowsRemoved);
      f.state.clear_downloads();
      f.send();
      QCOMPARE(tree->rowCount(), 0);
      QCOMPARE(flat->rowCount(), 0);
      QCOMPARE(flat->getTotalBytesInQueue(), quint64(0));
      QVERIFY(flatChanges.isEmpty());
      QCOMPARE(flatRemoves.size(), 1);
      f.switchView();
      QCOMPARE(flat->rowCount(), 0);
   }

   void emptyQueueReleasesBothModels_data()
   {
      QTest::addColumn<bool>("listActive");
      QTest::newRow("clear from list") << true;
      QTest::newRow("clear from tree") << false;
   }

   void emptyQueueReleasesBothModels()
   {
      QFETCH(bool, listActive);
      Fixture f;
      auto* flat = qobject_cast<GUI::DownloadsFlatModel*>(f.view->model());
      QVERIFY(flat);
      for (int i = 0; i < 250; ++i)
         f.add(i + 1, "/directory/nested/");
      f.send();
      f.switchView();
      auto* tree = qobject_cast<GUI::DownloadsTreeModel*>(f.view->model());
      QVERIFY(tree);
      QCOMPARE(flat->rowCount(), 250);
      QCOMPARE(tree->getDownloadIDs(tree->index(0, 0)).size(), 250);
      f.view->expandAll();
      if (listActive)
         f.switchView();

      QAbstractItemModelTester flatTester(flat, QAbstractItemModelTester::FailureReportingMode::QtTest);
      QAbstractItemModelTester treeTester(tree, QAbstractItemModelTester::FailureReportingMode::QtTest);
      QPersistentModelIndex flatIndex(flat->index(0, 0));
      QPersistentModelIndex treeIndex(tree->index(0, 0));
      QSignalSpy flatRemoves(flat, &QAbstractItemModel::rowsRemoved);
      QSignalSpy treeRemoves(tree, &QAbstractItemModel::rowsRemoved);
      f.state.clear_downloads();
      f.send();
      QCOMPARE(flat->rowCount(), 0);
      QCOMPARE(tree->rowCount(), 0);
      QVERIFY(!flatIndex.isValid());
      QVERIFY(!treeIndex.isValid());
      QCOMPARE(flat->getTotalBytesInQueue(), quint64(0));
      QCOMPARE(flatRemoves.size(), 1);
      QCOMPARE(treeRemoves.size(), 1);

      // Repeated empty states and view switches must not issue extra removals.
      f.send();
      f.switchView();
      f.send();
      QCOMPARE(f.view->model()->rowCount(), 0);
      QCOMPARE(flatRemoves.size(), 1);
      QCOMPARE(treeRemoves.size(), 1);

      // Reusing an ID and path must recreate the rows without stale tree nodes.
      f.add(1, "/directory/nested/");
      f.send();
      f.switchView();
      QCOMPARE(flat->rowCount(), 1);
      QCOMPARE(tree->getDownloadIDs(tree->index(0, 0)), QList<quint64>{1});
   }

   void progressAndEtaKeepSamplingWhileFlatIsInactive()
   {
      Fixture f;
      auto* flat = qobject_cast<GUI::DownloadsFlatModel*>(f.view->model());
      QVERIFY(flat);
      QSignalSpy progress(&f.widget, &GUI::DownloadsWidget::globalProgressChanged);
      f.switchView();
      f.add(1);
      f.state.mutable_downloads(0)->set_downloaded_bytes(1000);
      f.state.mutable_stats()->set_download_rate(1000);
      for (int i = 0; i < 9; ++i)
         f.send();
      QCOMPARE(flat->rowCount(), 0);
      QCOMPARE(flat->getTotalBytesInQueue(), quint64(10000));
      QCOMPARE(flat->getTotalBytesDownloadedInQueue(), quint64(1000));
      QCOMPARE(flat->getEta(), std::numeric_limits<quint64>::max());
      QVERIFY(!progress.isEmpty());
      QCOMPARE(progress.last().at(0).toULongLong(), quint64(1000));
      const int progressSignals = progress.size();
      // Neither switches nor filter edits are new throughput samples.
      for (int i = 0; i < 3; ++i)
      {
         f.switchView();
         f.showCompleted(false);
         f.switchView();
         f.showCompleted(true);
      }
      QCOMPARE(progress.size(), progressSignals);
      QCOMPARE(flat->getEta(), std::numeric_limits<quint64>::max());
      f.send();
      QCOMPARE(flat->getEta(), quint64(9));
   }

   void filtersAndQueueActionsUseLatestSnapshot()
   {
      Fixture f;
      f.add(1);
      f.send();
      f.switchView(); // Flat rows still contain only ID 1 from now on.
      f.state.clear_downloads();
      f.add(2, "/", Protos::Common::COMPLETE);
      f.add(3);
      f.add(4);
      f.send();
      f.showCompleted(false);
      auto* model = qobject_cast<GUI::DownloadsModel*>(f.view->model());
      QCOMPARE(model->rowCount(), 2);
      f.view->selectionModel()->select(model->index(1, 0), QItemSelectionModel::Select | QItemSelectionModel::Rows);
      QVERIFY(QMetaObject::invokeMethod(&f.widget, "moveSelectedEntriesToTop"));
      QCOMPARE(f.connection->references, QList<quint64> { 3 });
      QCOMPARE(f.connection->moved, QList<quint64> { 4 });
      f.switchView();
      model = qobject_cast<GUI::DownloadsModel*>(f.view->model());
      QCOMPARE(model->rowCount(), 2);
      QCOMPARE(model->getDownloadIDs(model->index(0, 0)), QList<quint64> { 3 });
      f.showCompleted(true);
      QCOMPARE(model->rowCount(), 3);
   }

   void treeExpansionSurvivesInactiveUpdates()
   {
      Fixture f;
      f.add(1, "/directory/");
      f.send();
      f.switchView();
      f.view->expand(f.view->model()->index(0, 0));
      f.switchView();
      f.add(2, "/directory/");
      f.send();
      f.switchView();
      const auto directory = f.view->model()->index(0, 0);
      QVERIFY(f.view->isExpanded(directory));
      QCOMPARE(f.view->model()->rowCount(directory), 2);
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
   SETTINGS.setSettingsMessage(new Protos::GUI::Settings());
   TestsDownloadsWidget tests;
   return QTest::qExec(&tests, argc, argv);
}

#include "TestsDownloadsWidget.moc"
