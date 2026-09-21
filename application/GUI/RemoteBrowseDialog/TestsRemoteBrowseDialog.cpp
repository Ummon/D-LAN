#include <QtTest>
#include <QAbstractItemModelTester>
#include <QDialogButtonBox>
#include <QLineEdit>
#include <QListView>
#include <QPushButton>
#include <QTreeView>
#include <QTemporaryDir>

#include <Common/Global.h>
#include <Common/Settings.h>
#include <Common/RemoteCoreController/priv/CoreConnection.h>
#include <Protos/gui_settings.pb.h>
#include <RemoteBrowseDialog/RemoteBrowseDialog.h>
#include <Utils.h>

void GUI::Utils::openLocations(const QStringList&, QWidget*) { QFAIL("Unexpected openLocations"); }

namespace
{
   using Entries = google::protobuf::RepeatedPtrField<Protos::GUI::LocalBrowseResult::Entry>;

   Entries entries(std::initializer_list<const char*> directories, std::initializer_list<const char*> files = {})
   {
      Entries result;
      for (const auto* name : directories)
      {
         auto* entry = result.Add();
         entry->set_name(name);
         entry->set_type(Protos::GUI::LocalBrowseResult::DIR);
         entry->set_size(1);
      }
      for (const auto* name : files)
      {
         auto* entry = result.Add();
         entry->set_name(name);
         entry->set_type(Protos::GUI::LocalBrowseResult::FILE);
         entry->set_size(10);
      }
      return result;
   }

   class BrowseResult : public RCC::ILocalBrowseResult
   {
   public:
      BrowseResult() : ILocalBrowseResult(1000) {}
      void start() override {}
   };

   class QuickAccessResult : public RCC::ILocalBrowseQuickAccessResult
   {
   public:
      QuickAccessResult() : ILocalBrowseQuickAccessResult(1000) {}
      void start() override {}
   };

   class Connection : public RCC::CoreConnection
   {
   public:
      struct Request { QString path; QSharedPointer<BrowseResult> result; };
      QList<Request> requests;
      QSharedPointer<QuickAccessResult> quickAccess = QSharedPointer<QuickAccessResult>::create();
      QMap<QString, Entries> filesystem {
         { "", entries({ "/" }) },
         { "/", entries({ "home", "other", "empty" }) },
         { "/home/", entries({ "child" }, { "a.txt", "b.txt" }) },
         { "/home/child/", entries({}, { "nested.txt" }) },
         { "/other/", entries({}, { "other.txt" }) }
      };
      bool isLocal() const override { return false; }
      QSharedPointer<RCC::ILocalBrowseResult> localBrowse(const QString& path, bool) override
      {
         auto result = QSharedPointer<BrowseResult>::create();
         this->requests.append({path, result});
         return result;
      }
      QSharedPointer<RCC::ILocalBrowseQuickAccessResult> localBrowseQuickAccess() override { return this->quickAccess; }
      void reply()
      {
         const auto request = this->requests.takeFirst();
         emit request.result->result(this->filesystem.value(request.path));
      }
      void drain()
      {
         for (int i = 0; !this->requests.isEmpty() && i < 30; ++i)
            this->reply();
         QVERIFY(this->requests.isEmpty());
      }
      void sendQuickAccess()
      {
         google::protobuf::RepeatedPtrField<Protos::GUI::LocalBrowseQuickAccessResult::QuickAccess> result;
         auto* first = result.Add();
         first->set_name("Home");
         first->set_path("/home/");
         auto* second = result.Add();
         second->set_name("Other");
         second->set_path("/other/");
         emit this->quickAccess->result(result);
      }
   };

   struct Fixture
   {
      QSharedPointer<Connection> connection = QSharedPointer<Connection>::create();
      GUI::RemoteBrowseDialog dialog {connection};
      QTreeView* tree = dialog.findChild<QTreeView*>("treeView");
      QLineEdit* path = dialog.findChild<QLineEdit*>("txtPath");
      QPushButton* back = dialog.findChild<QPushButton*>("butPrevious");
      QPushButton* next = dialog.findChild<QPushButton*>("butNext");
      QPushButton* up = dialog.findChild<QPushButton*>("butUp");
      QPushButton* ok = dialog.findChild<QDialogButtonBox*>("buttonBox")->button(QDialogButtonBox::Ok);
      GUI::RemoteBrowseModel* model = static_cast<GUI::RemoteBrowseModel*>(tree->model());
      Fixture()
      {
         connection->sendQuickAccess(); // Deliberately arrives before the root listing.
         connection->drain();
      }
      void edit(const QString& value)
      {
         path->setText(value);
         emit path->textEdited(value);
      }
      QString selected() const { return model->getPath(tree->currentIndex()); }
   };
}

class TestsRemoteBrowseDialog : public QObject
{
   Q_OBJECT
private slots:
   void defaultAndFolderHistory()
   {
      Fixture f;
      QCOMPARE(f.dialog.findChild<QListView*>("quickAccessListView")->currentIndex().row(), 0);
      QCOMPARE(f.selected(), "/home/");
      QCOMPARE(f.path->text(), "/home/");
      QVERIFY(f.ok->isEnabled());
      QVERIFY(!f.back->isEnabled());
      QVERIFY(!f.next->isEnabled());
      f.edit("/home/child");
      f.connection->drain();
      f.edit("/home/child/nested.txt");
      QVERIFY(f.ok->isEnabled());
      f.back->click();
      QCOMPARE(f.selected(), "/home/");
      f.next->click();
      QCOMPARE(f.selected(), "/home/child/");
      f.back->click();
      f.edit("/home/a.txt");
      QVERIFY(f.next->isEnabled()); // Files do not truncate forward history.
      f.edit("/other/");
      f.connection->drain();
      QVERIFY(!f.next->isEnabled());
      f.back->click();
      QCOMPARE(f.selected(), "/home/");
      f.up->click();
      QCOMPARE(f.selected(), "/");
      QCOMPARE(f.path->text(), "/");
      QVERIFY(!f.up->isEnabled());
   }

   void editingValidationAndKeyboardSelection()
   {
      Fixture f;
      f.edit("/home/missing");
      QVERIFY(!f.ok->isEnabled());
      QVERIFY(f.path->styleSheet().contains("red"));
      QMetaObject::invokeMethod(&f.dialog, "accept");
      QCOMPARE(f.dialog.result(), int(QDialog::Rejected));
      f.edit("/home/a.txt");
      QCOMPARE(f.selected(), "/home/a.txt");
      QCOMPARE(f.dialog.getSelectedPaths(), QStringList({"/home/a.txt"}));
      QVERIFY(f.ok->isEnabled());
      QVERIFY(f.path->styleSheet().isEmpty());
      for (const auto& path : {QString("\\home\\a.txt"), QString("/home\\a.txt")})
      {
         f.edit(path);
         QCOMPARE(f.selected(), "/home/a.txt");
         QVERIFY(f.ok->isEnabled());
      }
      f.dialog.show();
      f.tree->setFocus();
      QTest::keyClick(f.tree, Qt::Key_Down);
      QCOMPARE(f.path->text(), "/home/b.txt");
      f.up->click(); // Up operates on the containing folder when a file is selected.
      QCOMPARE(f.selected(), "/");
      for (const auto& invalid : {QString(), QString("relative"), QString("/home/a.txt/"), QString("/home/a.txt/child")})
      {
         f.edit(invalid);
         QVERIFY(!f.ok->isEnabled());
      }
      f.edit("/home/./child/../a.txt");
      QVERIFY(f.ok->isEnabled());
      QCOMPARE(f.selected(), "/home/a.txt");
      f.tree->clearSelection();
      QVERIFY(!f.ok->isEnabled());
   }

   void pendingLookupDoesNotOverrideNewInputOrSelection()
   {
      Fixture f;
      f.edit("/other/other.txt");
      QVERIFY(!f.ok->isEnabled());
      f.edit("/home/a.txt");
      f.connection->drain();
      QCOMPARE(f.selected(), "/home/a.txt");
      f.edit("/home/child/nested.txt");
      f.tree->setCurrentIndex(f.model->index(2, 0, f.tree->currentIndex().parent()));
      f.connection->drain();
      QCOMPARE(f.path->text(), "/home/b.txt");
      QCOMPARE(f.selected(), "/home/b.txt");
      f.edit("/empty/missing");
      f.connection->drain();
      QVERIFY(!f.ok->isEnabled());
      f.edit("/empty/");
      QVERIFY(f.ok->isEnabled());
      QVERIFY(f.connection->requests.isEmpty()); // An empty reply must not be fetched forever.
   }

   void quickAccessAndMultipleSelection()
   {
      Fixture f;
      auto* quick = f.dialog.findChild<QListView*>("quickAccessListView");
      quick->setCurrentIndex(quick->model()->index(1, 0));
      f.connection->drain();
      QCOMPARE(f.selected(), "/other/");
      f.back->click();
      QCOMPARE(f.selected(), "/home/");
      // Clicking an already-current shortcut still navigates after using history.
      emit quick->clicked(quick->currentIndex());
      QCOMPARE(f.selected(), "/other/");
      f.edit("/home/a.txt");
      const auto otherFile = f.model->index(2, 0, f.tree->currentIndex().parent());
      f.tree->selectionModel()->select(otherFile, QItemSelectionModel::Select | QItemSelectionModel::Rows);
      QCOMPARE(f.dialog.getSelectedPaths().size(), 2);
      QVERIFY(f.ok->isEnabled());
      f.edit("/home/b.txt");
      QCOMPARE(f.dialog.getSelectedPaths(), QStringList({"/home/b.txt"}));
      f.edit("/missing");
      emit f.tree->clicked(f.tree->currentIndex());
      QCOMPARE(f.path->text(), "/home/b.txt");
      QVERIFY(f.ok->isEnabled());
   }

   void timeoutAndWindowsPaths()
   {
      Fixture f;
      f.edit("/other/other.txt");
      const auto request = f.connection->requests.takeFirst();
      emit request.result->timeout();
      QVERIFY(!f.ok->isEnabled());
      f.edit("/home/a.txt");
      QVERIFY(f.ok->isEnabled());

      auto connection = QSharedPointer<Connection>::create();
      connection->filesystem = {
         { "", entries({ "C:/" }) },
         { "C:/", entries({ "Users" }) },
         { "C:/Users/", entries({}, { "File.txt" }) }
      };
      GUI::RemoteBrowseModel model(connection);
      QAbstractItemModelTester tester(&model, QAbstractItemModelTester::FailureReportingMode::QtTest);
      QSignalSpy resolved(&model, &GUI::RemoteBrowseModel::indexFromPath);
      model.getIndexFromPath("c:\\users\\FILE.txt");
      connection->drain();
      QCOMPARE(resolved.size(), 1);
      QCOMPARE(model.getPath(resolved.takeFirst()[0].value<QModelIndex>()), "C:/Users/File.txt");
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
   TestsRemoteBrowseDialog tests;
   return QTest::qExec(&tests, argc, argv);
}

#include "TestsRemoteBrowseDialog.moc"
