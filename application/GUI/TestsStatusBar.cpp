#include <QtTest>
#include <QLabel>
#include <QProgressBar>
#include <QPushButton>
#include <QTemporaryDir>
#include <QTranslator>

#include <Common/Global.h>
#include <Common/Settings.h>
#include <Common/RemoteCoreController/priv/CoreConnection.h>
#include <Protos/gui_settings.pb.h>
#include <StatusBar.h>

namespace
{
   class Connection : public RCC::CoreConnection
   {
   public:
      bool connectedToCore = false;
      bool isConnected() const override { return this->connectedToCore; }
      bool isLocal() const override { return true; }
   };

   // Stands for another language: each text is put between brackets.
   class Translator : public QTranslator
   {
   public:
      bool isEmpty() const override { return false; }
      QString translate(const char*, const char* sourceText, const char*, int) const override
      {
         return QString("[%1]").arg(sourceText);
      }
   };

   struct Fixture
   {
      QSharedPointer<Connection> connection = QSharedPointer<Connection>::create();
      GUI::StatusBar bar { connection };

      QString text(const char* label) const { return this->bar.findChild<QLabel*>(label)->text(); }
   };
}

class TestsStatusBar : public QObject
{
   Q_OBJECT
   Translator translator;

private slots:
   void cleanup()
   {
      QCoreApplication::removeTranslator(&this->translator);
   }

   void formTextsFollowTheLanguage()
   {
      Fixture f;
      auto* butLog = f.bar.findChild<QPushButton*>("butLog");
      auto* butConnectToLocal = f.bar.findChild<QPushButton*>("butConnectToLocal");
      QCOMPARE(butLog->toolTip(), QString("Show the log window"));
      QCOMPARE(butConnectToLocal->text(), QString("Connect to local"));

      QVERIFY(QCoreApplication::installTranslator(&this->translator));
      QTRY_COMPARE(butLog->toolTip(), QString("[Show the log window]"));
      QCOMPARE(butConnectToLocal->text(), QString("[Connect to local]"));
      QCOMPARE(f.bar.findChild<QLabel*>("icoDownloadRate")->toolTip(), QString("[Download rate]"));
   }

   /**
     * The texts built from the state of the core aren't part of the form: they have to be built again.
     */
   void stateTextsFollowTheLanguage_data()
   {
      QTest::addColumn<bool>("connected");
      QTest::addColumn<QString>("coreStatus");
      QTest::addColumn<QString>("coreStatusTranslated");
      QTest::addColumn<QString>("totalSharing");
      QTest::addColumn<QString>("totalSharingTranslated");
      QTest::addColumn<QString>("downloadRate");
      QTest::addColumn<QString>("uploadRate");

      QTest::newRow("disconnected")
         << false
         << "Core: disconnected" << "[Core: %1]"
         << "0 peer: 0 B" << "0 [peer]: 0 B"
         << "0 B/s" << "0 B/s";
      QTest::newRow("connected")
         << true
         << "Core: connected - indexing in progress . . ." << "[Core: %1]"
         << "2 peers: 3.0 KiB" << "2 [peers]: 3.0 KiB"
         << "2.0 KiB/s" << "1.0 KiB/s";
   }

   void stateTextsFollowTheLanguage()
   {
      QFETCH(bool, connected);
      QFETCH(QString, coreStatus);
      QFETCH(QString, coreStatusTranslated);
      QFETCH(QString, totalSharing);
      QFETCH(QString, totalSharingTranslated);
      QFETCH(QString, downloadRate);
      QFETCH(QString, uploadRate);

      Fixture f;
      auto* progress = f.bar.findChild<QProgressBar*>("prgCurrentAction");
      if (connected)
      {
         f.connection->connectedToCore = true;
         emit f.connection->connected();

         Protos::GUI::State state;
         state.mutable_stats()->set_download_rate(2048);
         state.mutable_stats()->set_upload_rate(1024);
         state.mutable_stats()->set_cache_status(Protos::GUI::State::Stats::HASHING_IN_PROGRESS);
         state.mutable_stats()->set_progress(4200);
         state.add_peers()->set_sharing_amount(1024);
         state.add_peers()->set_sharing_amount(2048);
         emit f.connection->newState(state);
      }

      QCOMPARE(f.text("lblCoreStatus"), coreStatus);
      QCOMPARE(f.text("lblTotalSharing"), totalSharing);
      QCOMPARE(f.text("lblDownloadRate"), downloadRate);
      QCOMPARE(f.text("lblUploadRate"), uploadRate);
      QCOMPARE(progress->isHidden(), !connected);

      QVERIFY(QCoreApplication::installTranslator(&this->translator));
      QTRY_COMPARE(
         f.text("lblCoreStatus"),
         coreStatusTranslated.arg(connected ? "[connected] - [indexing in progress . . .]" : "[disconnected]")
      );
      QCOMPARE(f.text("lblTotalSharing"), totalSharingTranslated);
      QCOMPARE(f.text("lblDownloadRate"), downloadRate);
      QCOMPARE(f.text("lblUploadRate"), uploadRate);
      QCOMPARE(progress->isHidden(), !connected);
      if (connected)
         QCOMPARE(progress->value(), 4200);
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
   TestsStatusBar tests;
   return QTest::qExec(&tests, argc, argv);
}

#include "TestsStatusBar.moc"
