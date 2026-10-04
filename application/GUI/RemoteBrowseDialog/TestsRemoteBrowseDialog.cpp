#include <QtTest>
#include <QAbstractItemModelTester>
#include <QCheckBox>
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

   // The entries whose name starts with a dot are hidden.
   Entries entries(std::initializer_list<const char*> directories, std::initializer_list<const char*> files = {})
   {
      Entries result;
      for (const auto* name : directories)
      {
         auto* entry = result.Add();
         entry->set_name(name);
         entry->set_type(Protos::GUI::LocalBrowseResult::DIR);
         entry->set_size(1);
         entry->set_hidden(*name == '.');
      }
      for (const auto* name : files)
      {
         auto* entry = result.Add();
         entry->set_name(name);
         entry->set_type(Protos::GUI::LocalBrowseResult::FILE);
         entry->set_size(10);
         entry->set_hidden(*name == '.');
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
      QStringList requestedPaths;
      QSharedPointer<QuickAccessResult> quickAccess = QSharedPointer<QuickAccessResult>::create();
      QMap<QString, Entries> filesystem {
         { "", entries({ "/" }) },
         { "/", entries({ "home", "other", "empty", ".config" }) },
         { "/home/", entries({ "child", ".secret" }, { "a.txt", "b.txt", ".profile" }) },
         { "/home/child/", entries({}, { "nested.txt" }) },
         { "/home/.secret/", entries({ ".deep" }, { "inner.txt" }) },
         { "/home/.secret/.deep/", entries({}, { "deep.txt" }) },
         { "/other/", entries({}, { "other.txt" }) },
         { "/.config/", entries({}, { ".only" }) }
      };
      bool isLocal() const override { return false; }
      QSharedPointer<RCC::ILocalBrowseResult> localBrowse(const QString& path, bool) override
      {
         auto result = QSharedPointer<BrowseResult>::create();
         this->requests.append({path, result});
         this->requestedPaths.append(path);
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
         auto* third = result.Add();
         third->set_name("Secret");
         third->set_path("/home/.secret");
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
      QPushButton* refresh = dialog.findChild<QPushButton*>("butRefresh");
      QCheckBox* showHidden = dialog.findChild<QCheckBox*>("chkShowHidden");
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
      f.dialog.show();
      QVERIFY(QTest::qWaitForWindowActive(&f.dialog));
      QVERIFY(f.path->hasFocus());
      QCOMPARE(f.path->cursorPosition(), f.path->text().size());
      QTRY_VERIFY(!f.path->hasSelectedText());
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

   void refreshOpenedFoldersAndPreserveSelection()
   {
      Fixture f;
      f.edit("/home/child/");
      f.connection->drain();
      const QPersistentModelIndex child = f.tree->currentIndex();
      f.edit("/other/");
      f.connection->drain();
      f.tree->collapse(f.tree->currentIndex());
      f.edit("/home/a.txt");
      const QPersistentModelIndex selected = f.tree->currentIndex();
      const QPersistentModelIndex home = selected.parent();
      const QPersistentModelIndex removed = f.model->index(2, 0, home);
      f.tree->selectionModel()->select(child, QItemSelectionModel::Select | QItemSelectionModel::Rows);
      QAbstractItemModelTester tester(f.model, QAbstractItemModelTester::FailureReportingMode::QtTest);
      tester.setUseFetchMore(false);
      f.connection->drain();
      QSignalSpy reset(f.model, &QAbstractItemModel::modelReset);
      QSignalSpy changed(f.model, &QAbstractItemModel::dataChanged);

      f.connection->filesystem["/home/"] = entries({"aaa", "child"}, {"a.txt", "new.txt"});
      f.connection->filesystem["/home/"].Mutable(2)->set_size(42);
      f.connection->filesystem["/home/child/"] = entries({}, {"fresh.txt"});
      f.connection->requestedPaths.clear();
      f.dialog.show();
      QVERIFY(QTest::qWaitForWindowActive(&f.dialog));
      QTest::mouseClick(f.refresh, Qt::LeftButton);
      QVERIFY(f.refresh->hasFocus());
      f.refresh->click(); // Repeated clicks cannot duplicate the active refresh.
      f.connection->drain();
      QCOMPARE(f.connection->requestedPaths, QStringList({"/", "/home/", "/home/child/"}));
      QVERIFY(f.refresh->isEnabled());
      QVERIFY(f.refresh->hasFocus());
      QVERIFY(selected.isValid());
      QCOMPARE(f.tree->currentIndex(), QModelIndex(selected));
      auto selectedPaths = f.dialog.getSelectedPaths();
      selectedPaths.sort();
      QCOMPARE(selectedPaths, QStringList({"/home/a.txt", "/home/child/"}));
      QCOMPARE(selected.row(), 2);
      QVERIFY(!removed.isValid());
      QCOMPARE(f.model->index(0, 0, child).data().toString(), "fresh.txt");
      QVERIFY(f.tree->isExpanded(home));
      QVERIFY(f.tree->isExpanded(child));
      QVERIFY(!changed.isEmpty());
      QVERIFY(reset.isEmpty());
      f.back->click();
      QCOMPARE(f.selected(), "/home/child/");
   }

   void refreshEmptyFolderAndReplaceEntryType()
   {
      Fixture f;
      f.edit("/empty/");
      f.connection->drain();
      const QPersistentModelIndex empty = f.tree->currentIndex();
      f.connection->filesystem["/empty/"] = entries({}, {"created.txt"});
      f.connection->requestedPaths.clear();
      f.refresh->click();
      f.connection->drain();
      QCOMPARE(f.connection->requestedPaths.count("/empty/"), 1);
      QCOMPARE(f.model->index(0, 0, empty).data().toString(), "created.txt");
      f.edit("/home/child/");
      f.connection->drain();
      const QPersistentModelIndex oldChild = f.tree->currentIndex();
      const QPersistentModelIndex home = oldChild.parent();
      f.connection->filesystem["/home/"] = entries({}, {"a.txt", "child"});
      f.connection->requestedPaths.clear();
      f.refresh->click();
      f.connection->drain();
      QVERIFY(!oldChild.isValid());
      QCOMPARE(f.connection->requestedPaths.count("/home/child/"), 0);
      QCOMPARE(f.model->rowCount(home), 2);
      QVERIFY(!f.model->isDirectory(f.model->index(1, 0, home)));
      QVERIFY(f.refresh->isEnabled());
   }

   void refreshCoalescesInflightRequestsAndContinuesAfterTimeout()
   {
      Fixture f;
      f.connection->requestedPaths.clear();
      f.edit("/home/child/"); // Expanding starts a request which has not replied yet.
      QCOMPARE(f.connection->requestedPaths, QStringList({"/home/child/"}));
      f.refresh->click();
      f.connection->drain();
      QCOMPARE(f.connection->requestedPaths, QStringList({"/home/child/", "/", "/home/"}));
      QVERIFY(f.refresh->isEnabled());

      f.connection->requestedPaths.clear();
      f.refresh->click();
      const auto request = f.connection->requests.takeFirst();
      emit request.result->timeout();
      // A late reply to a timed-out request must not populate the next folder.
      emit request.result->result(entries({}, {"stale.txt"}));
      f.connection->drain();
      QCOMPARE(f.connection->requestedPaths, QStringList({"/", "/home/", "/home/child/"}));
      QVERIFY(f.refresh->isEnabled());
      QCOMPARE(f.selected(), "/home/child/");
      QCOMPARE(f.model->index(0, 0, f.tree->currentIndex()).data().toString(), "nested.txt");
   }

   void refreshDeletesFolderWithPendingPathLookup()
   {
      Fixture f;
      f.connection->requestedPaths.clear();
      f.refresh->click();
      f.connection->reply(); // The listing for /home/ is now in flight.
      QCOMPARE(f.connection->requests.first().path, "/home/");
      f.edit("/home/child/new.txt"); // Its lazy request waits behind the refresh.
      f.connection->filesystem["/home/"] = entries({}, {"a.txt"});
      f.connection->drain();
      QCOMPARE(f.connection->requestedPaths, QStringList({"/", "/home/"}));
      QVERIFY(!f.ok->isEnabled());
      QVERIFY(f.path->styleSheet().contains("red"));
      QVERIFY(f.refresh->isEnabled());
      f.edit("/home/a.txt");
      QVERIFY(f.ok->isEnabled());
   }

   void showHiddenEntries()
   {
      Fixture f;
      QAbstractItemModelTester tester(f.model, QAbstractItemModelTester::FailureReportingMode::QtTest);
      tester.setUseFetchMore(false);
      f.connection->drain();
      const QPersistentModelIndex home = f.tree->currentIndex();
      const QPersistentModelIndex root = home.parent();
      QVERIFY(!f.showHidden->isChecked());
      QCOMPARE(f.model->rowCount(root), 3);
      QCOMPARE(f.model->rowCount(home), 3);

      f.connection->requestedPaths.clear();
      f.showHidden->click();
      // The known hidden entries are displayed without waiting for the refresh.
      QCOMPARE(f.model->rowCount(root), 4);
      QCOMPARE(f.model->rowCount(home), 5);
      QCOMPARE(f.model->index(0, 0, home).data().toString(), ".secret");
      QCOMPARE(f.model->index(2, 0, home).data().toString(), ".profile");
      f.connection->drain();
      QCOMPARE(f.connection->requestedPaths, QStringList({"/", "/home/"}));
      QCOMPARE(f.model->rowCount(home), 5);
      QCOMPARE(f.selected(), "/home/");

      f.connection->requestedPaths.clear();
      f.showHidden->click();
      QCOMPARE(f.model->rowCount(root), 3);
      QCOMPARE(f.model->rowCount(home), 3);
      f.connection->drain();
      QCOMPARE(f.connection->requestedPaths, QStringList({"/", "/home/"}));
      QCOMPARE(f.model->rowCount(home), 3);
      QCOMPARE(f.selected(), "/home/");
      QVERIFY(f.ok->isEnabled());

      // An old core doesn't tell which entries are hidden: they are all displayed.
      f.connection->filesystem["/other/"] = entries({}, {"other.txt", ".legacy"});
      f.connection->filesystem["/other/"].Mutable(1)->set_hidden(false);
      f.edit("/other/");
      f.refresh->click();
      f.connection->drain();
      QCOMPARE(f.model->rowCount(f.tree->currentIndex()), 2);
   }

   void showHiddenLabelAndPosition()
   {
      Fixture f;
      QCOMPARE(f.showHidden->text(), "Show hidden files and directories");
      f.dialog.setModes(GUI::RemoteBrowseDialog::DIR);
      QCOMPARE(f.showHidden->text(), "Show hidden directories");
      f.dialog.show();
      QVERIFY(QTest::qWaitForWindowExposed(&f.dialog));
      // At the bottom left, beside the buttons.
      const auto* buttons = f.dialog.findChild<QDialogButtonBox*>("buttonBox");
      QVERIFY(f.showHidden->geometry().right() < buttons->geometry().left());
      QVERIFY(f.showHidden->geometry().top() > f.tree->parentWidget()->geometry().bottom());
   }

   void hiddenPathsAreValidAndRevealed()
   {
      Fixture f;
      QAbstractItemModelTester tester(f.model, QAbstractItemModelTester::FailureReportingMode::QtTest);
      tester.setUseFetchMore(false);
      f.connection->drain();
      const QPersistentModelIndex home = f.tree->currentIndex();
      const QPersistentModelIndex root = home.parent();
      f.edit("/home/.sec"); // Only a part of the name.
      QVERIFY(!f.ok->isEnabled());
      QCOMPARE(f.model->rowCount(home), 3);
      f.edit("/home/.secret/inner.txt");
      f.connection->drain();
      QCOMPARE(f.selected(), "/home/.secret/inner.txt");
      QCOMPARE(f.dialog.getSelectedPaths(), QStringList({"/home/.secret/inner.txt"}));
      QVERIFY(f.ok->isEnabled());
      QVERIFY(f.path->styleSheet().isEmpty());
      // Only the entries of the path are revealed.
      const QPersistentModelIndex secret = f.tree->currentIndex().parent();
      QCOMPARE(secret.row(), 0);
      QCOMPARE(f.model->rowCount(home), 4);
      QCOMPARE(f.model->rowCount(secret), 1);
      QCOMPARE(f.model->rowCount(root), 3);

      f.edit("/home/.profile");
      QCOMPARE(f.selected(), "/home/.profile");
      QVERIFY(f.ok->isEnabled());
      f.edit("/home/.profile/"); // A hidden file isn't a directory either.
      QVERIFY(!f.ok->isEnabled());
      f.edit("/home/.secret/.deep/deep.txt");
      f.connection->drain();
      QCOMPARE(f.selected(), "/home/.secret/.deep/deep.txt");
      QVERIFY(f.ok->isEnabled());
      QCOMPARE(f.model->rowCount(home), 5);

      // The revealed entries remain displayed.
      f.refresh->click();
      f.connection->drain();
      QCOMPARE(f.model->rowCount(home), 5);
      f.showHidden->click();
      f.connection->drain();
      QCOMPARE(f.model->rowCount(root), 4);
      f.showHidden->click();
      f.connection->drain();
      QCOMPARE(f.model->rowCount(root), 3);
      QCOMPARE(f.model->rowCount(home), 5);
      QCOMPARE(f.model->rowCount(secret), 2);
      QCOMPARE(f.selected(), "/home/.secret/.deep/deep.txt");
      QVERIFY(f.ok->isEnabled());
   }

   void hiddenQuickAccess()
   {
      Fixture f;
      auto* quick = f.dialog.findChild<QListView*>("quickAccessListView");
      quick->setCurrentIndex(quick->model()->index(2, 0));
      f.connection->drain();
      QCOMPARE(f.selected(), "/home/.secret/");
      QCOMPARE(f.path->text(), "/home/.secret");
      QVERIFY(f.ok->isEnabled());
      QVERIFY(!f.showHidden->isChecked());
      f.back->click();
      QCOMPARE(f.selected(), "/home/");
      f.next->click();
      QCOMPARE(f.selected(), "/home/.secret/");
   }

   void hidingKeepsTheSelectionDisplayed()
   {
      Fixture f;
      QAbstractItemModelTester tester(f.model, QAbstractItemModelTester::FailureReportingMode::QtTest);
      tester.setUseFetchMore(false);
      f.connection->drain();
      const QPersistentModelIndex home = f.tree->currentIndex();
      const QPersistentModelIndex root = home.parent();
      f.showHidden->click();
      f.connection->drain();
      f.edit("/home/.profile");
      const QPersistentModelIndex profile = f.tree->currentIndex();
      f.edit("/home/.secret/inner.txt");
      f.connection->drain();
      const QPersistentModelIndex inner = f.tree->currentIndex();
      f.tree->selectionModel()->select(profile, QItemSelectionModel::Select | QItemSelectionModel::Rows);
      QCOMPARE(f.model->rowCount(inner.parent()), 2);

      f.showHidden->click();
      f.connection->drain();
      QVERIFY(profile.isValid());
      QVERIFY(inner.isValid());
      QCOMPARE(f.tree->currentIndex(), QModelIndex(inner));
      auto selectedPaths = f.dialog.getSelectedPaths();
      selectedPaths.sort();
      QCOMPARE(selectedPaths, QStringList({"/home/.profile", "/home/.secret/inner.txt"}));
      QVERIFY(f.ok->isEnabled());
      // The other hidden entries aren't displayed anymore.
      QCOMPARE(f.model->rowCount(root), 3);
      QCOMPARE(f.model->rowCount(home), 5);
      QCOMPARE(f.model->rowCount(inner.parent()), 1);
   }

   void directoryWithOnlyHiddenEntries()
   {
      Fixture f;
      f.showHidden->click();
      f.connection->drain();
      f.edit("/.config/");
      f.connection->drain();
      const QPersistentModelIndex config = f.tree->currentIndex();
      QCOMPARE(f.model->rowCount(config), 1);
      f.showHidden->click();
      f.connection->drain();
      QVERIFY(config.isValid());
      QCOMPARE(f.selected(), "/.config/");
      QCOMPARE(f.model->rowCount(config), 0);
      QVERIFY(!f.model->hasChildren(config));
      f.showHidden->click();
      QCOMPARE(f.model->rowCount(config), 1);
      f.connection->drain();
      QCOMPARE(f.model->rowCount(config), 1);
   }

   void hidingADirectoryBeingBrowsed()
   {
      Fixture f;
      QAbstractItemModelTester tester(f.model, QAbstractItemModelTester::FailureReportingMode::QtTest);
      tester.setUseFetchMore(false);
      f.connection->drain();
      const QPersistentModelIndex root = f.tree->currentIndex().parent();
      f.showHidden->click();
      f.connection->drain();
      f.connection->requestedPaths.clear();
      f.model->fetchMore(f.model->index(0, 0, root));
      QCOMPARE(f.connection->requestedPaths, QStringList({"/.config/"}));
      f.showHidden->click(); // '.config' isn't displayed anymore while its entries are awaited.
      const auto stale = f.connection->requests.takeFirst();
      QCOMPARE(stale.path, "/.config/");
      emit stale.result->result(f.connection->filesystem.value(stale.path)); // Must not be given to the root.
      f.connection->drain();
      QCOMPARE(f.connection->requestedPaths, QStringList({"/.config/", "/", "/home/"}));
      QCOMPARE(f.model->rowCount(), 1);
      QCOMPARE(f.model->rowCount(root), 3);
      QCOMPARE(f.selected(), "/home/");
      // The abandoned request doesn't prevent the next ones.
      f.connection->requestedPaths.clear();
      f.refresh->click();
      f.connection->drain();
      QCOMPARE(f.connection->requestedPaths, QStringList({"/", "/home/"}));
   }

   void hidingDuringAPathLookup()
   {
      Fixture f;
      f.showHidden->click();
      f.connection->drain();
      f.edit("/home/.secret/inner.txt"); // Paused until the entries of '.secret' are received.
      QVERIFY(!f.ok->isEnabled());
      f.showHidden->click();
      f.connection->drain();
      QCOMPARE(f.selected(), "/home/.secret/inner.txt");
      QVERIFY(f.ok->isEnabled());
      QCOMPARE(f.model->rowCount(f.tree->currentIndex().parent()), 1);
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
