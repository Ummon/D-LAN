#include <QtTest>
#include <QCheckBox>
#include <QComboBox>
#include <QLabel>
#include <QLineEdit>
#include <QListWidget>
#include <QPushButton>
#include <QRadioButton>
#include <QTableView>
#include <QMimeData>
#include <QTemporaryDir>

#include <Common/Constants.h>
#include <Common/Global.h>
#include <Common/Settings.h>
#include <Common/RemoteCoreController/priv/CoreConnection.h>
#include <Protos/gui_settings.pb.h>
#include <Settings/SettingsWidget.h>
#include <Utils.h>

void GUI::Utils::openLocation(const QString&, QWidget*) { QFAIL("Unexpected openLocation"); }
void GUI::Utils::askForDirectoriesOrFiles(
   QWidget*, QSharedPointer<RCC::ICoreConnection>, const QString&, const std::function<void(const QStringList&)>&
)
{
   QTest::qFail("Unexpected file dialog", __FILE__, __LINE__);
}

class TestsSettingsWidget : public QObject
{
   Q_OBJECT
private slots:
   void tunnelSelection()
   {
      class Connection : public RCC::CoreConnection
      {
      public:
         QList<Protos::GUI::CoreSettings> saved;
         void setCoreSettings(const Protos::GUI::CoreSettings& settings) override { this->saved << settings; }
      };
      auto connection = QSharedPointer<Connection>::create();
      GUI::SharedEntryListModel shares;
      GUI::SettingsWidget widget(connection, shares);
      Protos::GUI::State state;
      state.add_peers()->set_nick("Test");
      state.set_listen_any(Protos::Common::Interface::Address::IPv4);
      for (int i = 0; i < 2; ++i)
      {
         auto* interface = state.add_interfaces();
         interface->set_id(i + 1);
         interface->set_name(i == 0 ? "Wi-Fi" : "utun0");
         interface->set_is_up(true);
         interface->set_is_tunnel(i == 1);
         auto* address = interface->add_addresses();
         address->set_address(i == 0 ? "192.0.2.1" : "198.51.100.1");
         address->set_protocol(Protos::Common::Interface::Address::IPv4);
      }
      emit connection->newState(state);
      auto* showTunnels = widget.findChild<QCheckBox*>("chkShowTunnelInterfaces");
      auto* content = widget.findChild<QWidget*>("scoInterfacesContent");
      QRadioButton* tunnel = nullptr;
      QLabel* tunnelLabel = nullptr;
      for (auto* button : content->findChildren<QRadioButton*>())
         if (button->text() == "198.51.100.1")
            tunnel = button;
      for (auto* label : content->findChildren<QLabel*>())
         if (label->text() == "utun0")
            tunnelLabel = label;
      QVERIFY(showTunnels && tunnel && tunnelLabel);
      QVERIFY(!showTunnels->isChecked());
      QVERIFY(tunnel->parentWidget()->isHidden());
      QVERIFY(tunnelLabel->isHidden());

      showTunnels->setChecked(true);
      QVERIFY(!tunnel->parentWidget()->isHidden());
      QVERIFY(!tunnelLabel->isHidden());
      QVERIFY(connection->saved.isEmpty()); // Showing tunnels does not change binding.
      tunnel->setChecked(true);
      QCOMPARE(connection->saved.size(), 1);
      QCOMPARE(connection->saved.last().listen_address(), std::string("198.51.100.1"));
      showTunnels->setChecked(false);
      QVERIFY(!tunnel->parentWidget()->isHidden()); // Never hide the selected address.

      state.mutable_interfaces(1)->mutable_addresses(0)->set_listened(true);
      emit connection->newState(state);
      QVERIFY(tunnel->isChecked());
      QVERIFY(!tunnel->parentWidget()->isHidden());
      QCOMPARE(connection->saved.size(), 1);

      // A core switching back to automatic discovery hides the tunnel again.
      state.mutable_interfaces(1)->mutable_addresses(0)->set_listened(false);
      emit connection->newState(state);
      QVERIFY(widget.findChild<QRadioButton*>("radIPv4")->isChecked());
      QVERIFY(tunnel->parentWidget()->isHidden());
      QVERIFY(tunnelLabel->isHidden());
      QCOMPARE(connection->saved.size(), 1);

      state.mutable_interfaces()->RemoveLast();
      emit connection->newState(state);
      QVERIFY(showTunnels->isHidden());
   }

   void dropEntriesToShare()
   {
      class Connection : public RCC::CoreConnection
      {
      public:
         bool local = true;
         QList<Protos::GUI::CoreSettings> saved;
         bool isLocal() const override { return this->local; }
         void setCoreSettings(const Protos::GUI::CoreSettings& settings) override { this->saved << settings; }
      };
      auto connection = QSharedPointer<Connection>::create();
      GUI::SharedEntryListModel shares;
      GUI::SettingsWidget widget(connection, shares);
      auto* viewport = widget.findChild<QTableView*>("tblShareDirs")->viewport();

      QTemporaryDir root;
      QVERIFY(root.isValid());
      QVERIFY(QDir(root.path()).mkdir("dir"));
      QFile file(root.filePath("file.txt"));
      QVERIFY(file.open(QIODevice::WriteOnly));
      file.close();

      QMimeData mimeData;
      mimeData.setUrls({
         QUrl::fromLocalFile(root.filePath("dir")),
         QUrl::fromLocalFile(root.filePath("file.txt")),
         QUrl::fromLocalFile(root.filePath("missing")),
         QUrl("https://example.com/a.txt")
      });
      // Returns the action of the drop, or 'IgnoreAction' if it was refused.
      const auto drop = [&]()
      {
         for (const auto type : { QEvent::DragEnter, QEvent::Drop })
         {
            QDropEvent event(QPointF(1, 1), Qt::CopyAction | Qt::MoveAction | Qt::LinkAction, &mimeData, Qt::LeftButton, Qt::NoModifier, type);
            QCoreApplication::sendEvent(viewport, &event);
            if (!event.isAccepted())
               return Qt::IgnoreAction;
            if (type == QEvent::Drop)
               return event.dropAction();
         }
         return Qt::IgnoreAction;
      };

      // Not connected yet.
      QCOMPARE(drop(), Qt::IgnoreAction);

      emit connection->connected(); // Enables the tab containing the shared entries.
      Protos::GUI::State state;
      state.add_peers()->set_nick("Test");
      emit connection->newState(state);

      QCOMPARE(drop(), Qt::LinkAction);
      QCOMPARE(connection->saved.size(), 1);
      const auto& sharedPaths = connection->saved.last().shared_paths();
      QCOMPARE(sharedPaths.size(), 2);
      QCOMPARE(QString::fromStdString(sharedPaths.Get(0).path()), QDir::cleanPath(root.path()) + "/dir/");
      QCOMPARE(QString::fromStdString(sharedPaths.Get(1).path()), QDir::cleanPath(root.path()) + "/file.txt");

      // Already shared entries aren't added again.
      QCOMPARE(drop(), Qt::LinkAction);
      QCOMPARE(shares.rowCount(), 2);
      QCOMPARE(connection->saved.size(), 1);

      // Local paths are meaningless to a remote core.
      connection->local = false;
      QVERIFY(QDir(root.path()).mkdir("other"));
      mimeData.setUrls({ QUrl::fromLocalFile(root.filePath("other")) });
      QCOMPARE(drop(), Qt::IgnoreAction);
      QCOMPARE(shares.rowCount(), 2);
      QCOMPARE(connection->saved.size(), 1);
   }

   void ignoredExtensions()
   {
      class Connection : public RCC::CoreConnection
      {
      public:
         QList<Protos::GUI::CoreSettings> saved;
         void setCoreSettings(const Protos::GUI::CoreSettings& settings) override { this->saved << settings; }
      };
      auto connection = QSharedPointer<Connection>::create();
      GUI::SharedEntryListModel shares;
      GUI::SettingsWidget widget(connection, shares);
      auto* tabs = widget.findChild<QTabWidget*>("tabWidget");
      auto* tab = widget.findChild<QWidget*>("tabCoreAdvancedSettings");
      auto* input = widget.findChild<QLineEdit*>("txtIgnoredExtension");
      auto* add = widget.findChild<QPushButton*>("butAddIgnoredExtension");
      auto* list = widget.findChild<QListWidget*>("lstIgnoredExtensions");
      auto* remove = widget.findChild<QPushButton*>("butRemoveIgnoredExtensions");
      auto* reset = widget.findChild<QPushButton*>("butResetIgnoredExtensions");
      auto* enabled = widget.findChild<QCheckBox*>("chkIgnoredExtensionsEnabled");
      QVERIFY(tabs && tab && input && add && list && remove && reset && enabled);
      const auto shown = [&] {
         QStringList extensions;
         for (int i = 0; i < list->count(); ++i)
            extensions << list->item(i)->text();
         return extensions;
      };
      const auto saved = [&] {
         QStringList extensions;
         for (const auto& extension : connection->saved.last().ignored_extensions().extensions())
            extensions << QString::fromStdString(extension);
         return extensions;
      };

      // It's a setting of the core: the tab follows the connection, right after the "User interface" one.
      QCOMPARE(tabs->indexOf(tab), 3);
      QVERIFY(!tabs->isTabEnabled(3));
      emit connection->connected();
      QVERIFY(tabs->isTabEnabled(3));

      Protos::GUI::State state;
      state.add_peers()->set_nick("Test");
      state.mutable_ignored_extensions()->set_enabled(true);
      state.mutable_ignored_extensions()->add_extensions(".part");
      state.mutable_ignored_extensions()->add_extensions(".!qB");
      emit connection->newState(state);
      QCOMPARE(shown(), (QStringList { ".!qB", ".part" }));
      QVERIFY(enabled->isChecked());
      QVERIFY(!add->isEnabled());
      QVERIFY(!remove->isEnabled());
      QVERIFY(reset->isEnabled());
      QVERIFY(connection->saved.isEmpty()); // What comes from the core isn't sent back.

      // The input is trimmed and gets its dot.
      input->setText("  crdownload ");
      QVERIFY(add->isEnabled());
      add->click();
      QVERIFY(input->text().isEmpty());
      QCOMPARE(shown(), (QStringList { ".!qB", ".crdownload", ".part" }));
      QCOMPARE(connection->saved.size(), 1);
      QCOMPARE(saved(), shown());
      QVERIFY(connection->saved.last().ignored_extensions().enabled());

      // Invalid extensions can't be added.
      for (const QString& invalid : { QString(), QString("  "), QString("."), QString("a b"), QString(". part") })
      {
         input->setText(invalid);
         QVERIFY(!add->isEnabled());
         QTest::keyClick(input, Qt::Key_Return);
         QCOMPARE(connection->saved.size(), 1);
         QCOMPARE(list->count(), 3);
      }

      // An extension already listed is selected instead of being added, whatever its case.
      list->clearSelection();
      input->setText(".PART");
      QTest::keyClick(input, Qt::Key_Return);
      QCOMPARE(connection->saved.size(), 1);
      QCOMPARE(list->selectedItems().size(), 1);
      QCOMPARE(list->selectedItems().first()->text(), QString(".part"));

      // A state from the core showing the same extensions keeps the selection.
      state.mutable_ignored_extensions()->add_extensions(".crdownload");
      emit connection->newState(state);
      QCOMPARE(list->selectedItems().size(), 1);

      QVERIFY(remove->isEnabled());
      remove->click();
      QCOMPARE(shown(), (QStringList { ".!qB", ".crdownload" }));
      QCOMPARE(connection->saved.size(), 2);
      QCOMPARE(saved(), shown());
      QVERIFY(!remove->isEnabled());

      enabled->click();
      QCOMPARE(connection->saved.size(), 3);
      QVERIFY(!connection->saved.last().ignored_extensions().enabled());
      QCOMPARE(saved(), shown());

      reset->click();
      QCOMPARE(connection->saved.size(), 4);
      QStringList defaultExtensions = Common::Constants::DEFAULT_IGNORED_EXTENSIONS;
      defaultExtensions.sort();
      QCOMPARE(saved(), defaultExtensions);
      QCOMPARE(shown(), defaultExtensions);
      QVERIFY(!reset->isEnabled());
      QVERIFY(!connection->saved.last().ignored_extensions().enabled());

      // The core has the last word.
      state.mutable_ignored_extensions()->Clear();
      emit connection->newState(state);
      QVERIFY(shown().isEmpty());
      QVERIFY(!enabled->isChecked());
      QCOMPARE(connection->saved.size(), 4);

      emit connection->disconnected(false);
      QVERIFY(!tabs->isTabEnabled(3));
   }

   void languageChangeKeepsStyle()
   {
      auto connection = QSharedPointer<RCC::CoreConnection>::create();
      GUI::SharedEntryListModel shares;
      GUI::SettingsWidget widget(connection, shares);
      auto* styles = widget.findChild<QComboBox*>("cmbStyles");
      QVERIFY(styles);
      if (styles->count() < 2)
         QSKIP("No style deployed next to the executable");

      QSignalSpy styleChanged(&widget, &GUI::SettingsWidget::styleChanged);
      styles->setCurrentIndex(1);
      QCOMPARE(styleChanged.size(), 1);
      const QString style = styles->currentData().toString();
      QCOMPARE(SETTINGS.get<QString>("style"), style);

      // Refilling the style names must not unload and reapply the style.
      emit widget.findChild<QComboBox*>("cmbLanguages")->currentIndexChanged(0);
      QCOMPARE(styleChanged.size(), 1);
      QCOMPARE(styles->currentData().toString(), style);
      QCOMPARE(SETTINGS.get<QString>("style"), style);
   }
};

int main(int argc, char** argv)
{
   QApplication app(argc, argv);
   QTemporaryDir data;
   if (!data.isValid())
      return 1;
   Common::Global::setDataFolder(Common::Global::DataFolderType::LOCAL, data.path());
   Common::Global::setDataFolder(Common::Global::DataFolderType::ROAMING, data.path());
   SETTINGS.setSettingsMessage(new Protos::GUI::Settings());
   TestsSettingsWidget tests;
   return QTest::qExec(&tests, argc, argv);
}

#include "TestsSettingsWidget.moc"
