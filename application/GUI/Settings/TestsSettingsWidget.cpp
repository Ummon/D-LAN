#include <QtTest>
#include <QCheckBox>
#include <QLabel>
#include <QRadioButton>
#include <QTemporaryDir>

#include <Common/Global.h>
#include <Common/Settings.h>
#include <Common/RemoteCoreController/priv/CoreConnection.h>
#include <Protos/gui_settings.pb.h>
#include <Settings/SettingsWidget.h>
#include <Utils.h>

void GUI::Utils::openLocation(const QString&, QWidget*) { QFAIL("Unexpected openLocation"); }
QStringList GUI::Utils::askForDirectoriesOrFiles(QWidget*, QSharedPointer<RCC::ICoreConnection>, const QString&)
{
   QTest::qFail("Unexpected file dialog", __FILE__, __LINE__);
   return {};
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
         void setCoreSettings(const Protos::GUI::CoreSettings settings) override { this->saved << settings; }
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
