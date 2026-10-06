#include <QtTest>
#include <QCheckBox>
#include <QComboBox>
#include <QLineEdit>
#include <QPushButton>
#include <QTemporaryDir>

#include <Common/Global.h>
#include <Common/Settings.h>
#include <Common/RemoteCoreController/priv/CoreConnection.h>
#include <Protos/gui_settings.pb.h>
#include <Search/SearchDock.h>

namespace
{
   class Connection : public RCC::CoreConnection
   {
   public:
      bool isConnected() const override { return true; }
   };

   const int TYPE_ALL = 0;
   const int TYPE_AUDIO = 3;
   const int UNIT_KIB = 1;
}

class TestsSearchDock : public QObject
{
   Q_OBJECT

private slots:
   /**
     * A search needs a text or a filter, and the filters are only applied when the advanced options are shown:
     * the ones left in the hidden options must not be enough to launch a search.
     */
   void onlyUsedCriteriaLaunchASearch_data()
   {
      QTest::addColumn<bool>("advancedOptionsShown");
      QTest::addColumn<QString>("text");
      QTest::addColumn<int>("type");
      QTest::addColumn<QString>("minSize");
      QTest::addColumn<bool>("searchExpected");
      QTest::addColumn<bool>("filtersExpected");

      QTest::newRow("shown, nothing") << true << "" << TYPE_ALL << "" << false << false;
      QTest::newRow("shown, type") << true << "" << TYPE_AUDIO << "" << true << true;
      QTest::newRow("shown, size") << true << "" << TYPE_ALL << "5" << true << true;
      QTest::newRow("shown, text") << true << " abc " << TYPE_ALL << "" << true << true;
      QTest::newRow("shown, text and filters") << true << "abc" << TYPE_AUDIO << "5" << true << true;

      QTest::newRow("hidden, nothing") << false << "" << TYPE_ALL << "" << false << false;
      QTest::newRow("hidden, type") << false << "" << TYPE_AUDIO << "" << false << false;
      QTest::newRow("hidden, size") << false << "" << TYPE_ALL << "5" << false << false;
      QTest::newRow("hidden, text") << false << "abc" << TYPE_ALL << "" << true << false;
      QTest::newRow("hidden, text and filters") << false << "abc" << TYPE_AUDIO << "5" << true << false;
   }

   void onlyUsedCriteriaLaunchASearch()
   {
      QFETCH(bool, advancedOptionsShown);
      QFETCH(QString, text);
      QFETCH(int, type);
      QFETCH(QString, minSize);
      QFETCH(bool, searchExpected);
      QFETCH(bool, filtersExpected);

      SETTINGS.set("search_advanced_visible", advancedOptionsShown);

      const auto connection = QSharedPointer<Connection>::create();
      GUI::SearchDock dock(connection);
      dock.show();
      emit connection->connected();
      QCOMPARE(dock.findChild<QWidget*>("advancedOptions")->isVisible(), advancedOptionsShown);

      QList<Protos::Common::FindPattern> patterns;
      connect(
         &dock,
         qOverload<const Protos::Common::FindPattern&, bool>(&GUI::SearchDock::search),
         this,
         [&](const Protos::Common::FindPattern& pattern, bool) { patterns << pattern; }
      );

      // The criteria of the previous search are restored when the dock is created.
      dock.findChild<QPushButton*>("butClear")->click();
      dock.findChild<QLineEdit*>("txtSearch")->setText(text);
      dock.findChild<QComboBox*>("cmbType")->setCurrentIndex(type);
      dock.findChild<QLineEdit*>("txtMinSize")->setText(minSize);
      dock.findChild<QComboBox*>("cmbMinSize")->setCurrentIndex(UNIT_KIB);

      dock.findChild<QPushButton*>("butSearch")->click();

      QCOMPARE(patterns.size(), searchExpected ? 1 : 0);
      if (!searchExpected)
         return;

      const Protos::Common::FindPattern& pattern = patterns.first();
      QCOMPARE(QString::fromStdString(pattern.pattern()), text.trimmed());
      QCOMPARE(pattern.extension_filters_size() > 0, filtersExpected && type == TYPE_AUDIO);
      QCOMPARE(pattern.min_size(), filtersExpected ? minSize.toULongLong() * 1024 : 0);
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
   TestsSearchDock tests;
   return QTest::qExec(&tests, argc, argv);
}

#include "TestsSearchDock.moc"
