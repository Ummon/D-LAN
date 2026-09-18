#include <QtTest>
#include <QAbstractItemModelTester>
#include <QItemSelectionModel>
#include <QTemporaryDir>

#include <Browse/BrowseModel.h>
#include <Common/Global.h>
#include <Common/RemoteCoreController/Builder.h>

namespace
{
   class Model : public GUI::BrowseModel
   {
   public:
      using GUI::BrowseModel::BrowseModel;
      using GUI::BrowseModel::synchronizeRoot;
      void update(const QModelIndex& parent, const Protos::Common::Entries& entries)
      {
         this->synchronize(static_cast<Tree*>(parent.internalPointer()), entries);
      }
   };

   Protos::Common::Entry entry(const QString& name, bool directory = false)
   {
      Protos::Common::Entry result;
      result.set_name(name.toStdString());
      result.set_path("/");
      result.set_type(directory ? Protos::Common::Entry::DIR : Protos::Common::Entry::FILE);
      result.set_is_empty(true);
      result.set_size(100);
      return result;
   }

   Protos::Common::Entries files(const QStringList& names)
   {
      Protos::Common::Entries result;
      for (const auto& name : names)
         result.add_entries()->CopyFrom(entry(name));
      return result;
   }

   Protos::Common::Entries roots(const QStringList& names)
   {
      Protos::Common::Entries result;
      for (const auto& name : names)
      {
         auto* root = result.add_entries();
         root->CopyFrom(entry(name, true));
         root->set_path("");
         root->set_name("");
         root->mutable_shared_entry()->mutable_id()->set_hash(name.toStdString());
         root->mutable_shared_entry()->set_shared_name(name.toStdString());
      }
      return result;
   }

   QStringList names(const Model& model, const QModelIndex& parent = {})
   {
      QStringList result;
      for (int row = 0; row < model.rowCount(parent); ++row)
         result.append(model.index(row, 0, parent).data().toString());
      return result;
   }

   struct Fixture
   {
      QSharedPointer<RCC::ICoreConnection> connection = RCC::Builder::newCoreConnection(1000);
      GUI::SharedEntryListModel shares;
      Model model { connection, shares, Common::Hash(), false };
      Fixture() { model.synchronizeRoot(roots({ "share" })); }
   };
}

class TestsBrowseModel : public QObject
{
   Q_OBJECT
private slots:
   void batchesDirectoryChangesAndPreservesNodes()
   {
      Fixture f;
      const auto parent = f.model.index(0, 0);
      f.model.update(parent, files({ "a", "b", "c", "d", "e", "f", "g", "h" }));
      QAbstractItemModelTester tester(&f.model, QAbstractItemModelTester::FailureReportingMode::QtTest);
      QPersistentModelIndex retained = f.model.index(3, 0, parent);
      QPersistentModelIndex deleted = f.model.index(1, 0, parent);
      void* retainedNode = retained.internalPointer();
      QItemSelectionModel selection(&f.model);
      selection.select(retained, QItemSelectionModel::Select);
      QSignalSpy insertions(&f.model, &QAbstractItemModel::rowsInserted);
      QSignalSpy removals(&f.model, &QAbstractItemModel::rowsRemoved);
      QSignalSpy changes(&f.model, &QAbstractItemModel::dataChanged);
      QSignalSpy resets(&f.model, &QAbstractItemModel::modelReset);
      auto updated = files({ "0", "1", "d", "e", "h", "i", "j" });
      updated.mutable_entries(2)->set_size(200);
      updated.mutable_entries(3)->set_size(300);
      f.model.update(parent, updated);
      QCOMPARE(names(f.model, parent), QStringList({ "0", "1", "d", "e", "h", "i", "j" }));
      QCOMPARE(insertions.count(), 2);
      QCOMPARE(insertions[0][1].toInt(), 0);
      QCOMPARE(insertions[0][2].toInt(), 1);
      QCOMPARE(insertions[1][1].toInt(), 5);
      QCOMPARE(insertions[1][2].toInt(), 6);
      QCOMPARE(removals.count(), 2);
      QCOMPARE(removals[0][1].toInt(), 2);
      QCOMPARE(removals[0][2].toInt(), 4);
      QCOMPARE(removals[1][1].toInt(), 4);
      QCOMPARE(removals[1][2].toInt(), 5);
      QCOMPARE(changes.count(), 1);
      QCOMPARE(changes[0][0].value<QModelIndex>().row(), 2);
      QCOMPARE(changes[0][1].value<QModelIndex>().row(), 3);
      QCOMPARE(changes[0][0].value<QModelIndex>().parent(), parent);
      QVERIFY(retained.isValid());
      QCOMPARE(retained.internalPointer(), retainedNode);
      QCOMPARE(retained.row(), 2);
      QCOMPARE(f.model.getEntry(retained).size(), quint64(200));
      QVERIFY(selection.isSelected(retained));
      QVERIFY(!deleted.isValid());
      QCOMPARE(resets.count(), 0);

      f.model.update(parent, updated);
      QCOMPARE(insertions.count(), 2);
      QCOMPARE(removals.count(), 2);
      QCOMPARE(changes.count(), 1);
      f.model.update(parent, {});
      QCOMPARE(f.model.rowCount(parent), 0);
      QVERIFY(f.model.getEntry(parent).is_empty());
      QVERIFY(!retained.isValid());
      QCOMPARE(removals.count(), 3);
      QCOMPARE(removals.last()[1].toInt(), 0);
      QCOMPARE(removals.last()[2].toInt(), 6);
      f.model.update(parent, files({ "new" }));
      QCOMPARE(f.model.rowCount(parent), 1);
      QVERIFY(!f.model.getEntry(parent).is_empty());
   }

   void retainedDirectoriesKeepLoadedDescendants()
   {
      Fixture f;
      const auto parent = f.model.index(0, 0);
      Protos::Common::Entries entries;
      entries.add_entries()->CopyFrom(entry("keep", true));
      entries.add_entries()->CopyFrom(entry("remove", true));
      entries.add_entries()->CopyFrom(entry("a-file"));
      f.model.update(parent, entries);
      QPersistentModelIndex kept = f.model.index(0, 0, parent);
      QPersistentModelIndex removed = f.model.index(1, 0, parent);
      f.model.update(kept, files({ "child" }));
      f.model.update(removed, files({ "deleted-child" }));
      QPersistentModelIndex child = f.model.index(0, 0, kept);
      QPersistentModelIndex deletedChild = f.model.index(0, 0, removed);
      const auto childNode = child.internalPointer();
      QAbstractItemModelTester tester(&f.model, QAbstractItemModelTester::FailureReportingMode::QtTest);
      entries.clear_entries();
      entries.add_entries()->CopyFrom(entry("first", true));
      auto* retainedEntry = entries.add_entries();
      retainedEntry->CopyFrom(entry("keep", true));
      retainedEntry->set_is_empty(false);
      retainedEntry->set_size(999);
      entries.add_entries()->CopyFrom(entry("a-file"));
      f.model.update(parent, entries);
      QVERIFY(kept.isValid());
      QCOMPARE(kept.row(), 1);
      QCOMPARE(f.model.rowCount(kept), 1);
      QVERIFY(child.isValid());
      QCOMPARE(child.parent(), QModelIndex(kept));
      QCOMPARE(child.internalPointer(), childNode);
      QCOMPARE(f.model.getEntry(child).shared_entry().id().hash(), std::string("share"));
      QVERIFY(!removed.isValid());
      QVERIFY(!deletedChild.isValid());
      QCOMPARE(names(f.model, parent), QStringList({ "first", "keep", "a-file" }));
   }

   void rootMovesPreserveDescendantsAndBatchNewShares()
   {
      Fixture f;
      f.model.synchronizeRoot(roots({ "a", "b", "c", "d" }));
      QPersistentModelIndex retained = f.model.index(2, 0);
      f.model.update(retained, files({ "child" }));
      QPersistentModelIndex child = f.model.index(0, 0, retained);
      QPersistentModelIndex deleted = f.model.index(1, 0);
      QAbstractItemModelTester tester(&f.model, QAbstractItemModelTester::FailureReportingMode::QtTest);
      QSignalSpy insertions(&f.model, &QAbstractItemModel::rowsInserted);
      QSignalSpy removals(&f.model, &QAbstractItemModel::rowsRemoved);
      QSignalSpy moves(&f.model, &QAbstractItemModel::rowsMoved);
      QSignalSpy changes(&f.model, &QAbstractItemModel::dataChanged);
      auto updated = roots({ "c", "x", "y", "a" });
      updated.mutable_entries(0)->set_is_empty(false);
      updated.mutable_entries(0)->mutable_shared_entry()->set_shared_name("renamed");
      f.model.synchronizeRoot(updated);
      QCOMPARE(names(f.model), QStringList({ "renamed", "x", "y", "a" }));
      QCOMPARE(insertions.count(), 1);
      QCOMPARE(insertions[0][1].toInt(), 1);
      QCOMPARE(insertions[0][2].toInt(), 2);
      QCOMPARE(removals.count(), 1);
      QCOMPARE(removals[0][1].toInt(), 4);
      QCOMPARE(removals[0][2].toInt(), 5);
      QCOMPARE(moves.count(), 1);
      QCOMPARE(changes.count(), 1);
      QCOMPARE(retained.row(), 0);
      QVERIFY(child.isValid());
      QCOMPARE(child.parent(), QModelIndex(retained));
      QVERIFY(!deleted.isValid());
      f.model.synchronizeRoot(updated);
      QCOMPARE(insertions.count(), 1);
      QCOMPARE(removals.count(), 1);
      QCOMPARE(moves.count(), 1);
      QCOMPARE(changes.count(), 1);
      f.model.synchronizeRoot({});
      QCOMPARE(f.model.rowCount(), 0);
      QVERIFY(!child.isValid());
   }

   void largeDirectoryUsesSingleRange()
   {
      Fixture f;
      const auto parent = f.model.index(0, 0);
      f.model.update(parent, files({ "z" }));
      QPersistentModelIndex retained = f.model.index(0, 0, parent);
      Protos::Common::Entries entries;
      for (int i = 0; i < 2000; ++i)
         entries.add_entries()->CopyFrom(entry(QString("a%1").arg(i, 4, 10, QChar('0'))));
      entries.add_entries()->CopyFrom(entry("z"));
      QSignalSpy insertions(&f.model, &QAbstractItemModel::rowsInserted);
      QSignalSpy removals(&f.model, &QAbstractItemModel::rowsRemoved);
      f.model.update(parent, entries);
      QCOMPARE(insertions.count(), 1);
      QCOMPARE(insertions[0][1].toInt(), 0);
      QCOMPARE(insertions[0][2].toInt(), 1999);
      QCOMPARE(retained.row(), 2000);
      f.model.update(parent, files({ "z" }));
      QCOMPARE(removals.count(), 1);
      QCOMPARE(removals[0][1].toInt(), 0);
      QCOMPARE(removals[0][2].toInt(), 1999);
      QCOMPARE(retained.row(), 0);
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
   TestsBrowseModel tests;
   return QTest::qExec(&tests, argc, argv);
}
#include "TestsBrowseModel.moc"
