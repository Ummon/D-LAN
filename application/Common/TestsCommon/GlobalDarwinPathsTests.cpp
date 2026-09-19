#include <QDir>
#include <QFile>
#include <QProcess>
#include <QScopeGuard>
#include <QStandardPaths>
#include <QTemporaryDir>
#include <QTest>

#include <Common/Constants.h>
#include <Common/Global.h>
#include <Common/PersistentData.h>
#include <Common/LogManager/Builder.h>
#include <Core/HashCache/Builder.h>
#include <Protos/gui_settings.pb.h>

#include <memory>

namespace
{
   QString libraryRoot;
   bool missingLocation = false;
   using Global = Common::Global;
   using Folder = Global::DataFolderType;
}

// Supply native-directory results under a temporary root, without modifying
// HOME or writing into the user's real Library or Qt's shared test directory.
QString QStandardPaths::writableLocation(StandardLocation type)
{
   if (missingLocation)
      return {};
   if (type == GenericDataLocation)
      return libraryRoot + "/Application Support";
   if (type == GenericCacheLocation)
      return libraryRoot + "/Caches";
   return {};
}

class GlobalDarwinPathsTests : public QObject
{
   Q_OBJECT
   std::unique_ptr<QTemporaryDir> temporary;

private slots:
   void init()
   {
      temporary = std::make_unique<QTemporaryDir>();
      QVERIFY(temporary->isValid());
      libraryRoot = temporary->filePath("Library");
      missingLocation = false;
      Global::setDataFolderToDefault(Folder::ROAMING);
      Global::setDataFolderToDefault(Folder::LOCAL);
   }

   void cleanup()
   {
      Global::setDataFolderToDefault(Folder::ROAMING);
      Global::setDataFolderToDefault(Folder::LOCAL);
   }

   void defaultLocationsAndCreation()
   {
      const QString support = libraryRoot + "/Application Support/D-LAN";
      const QString cache = libraryRoot + "/Caches/D-LAN";
      const QString logs = libraryRoot + "/Logs/D-LAN";
      QCOMPARE(Global::getDataFolder(Folder::ROAMING, false), support);
      QCOMPARE(Global::getDataFolder(Folder::LOCAL, false), support);
      QCOMPARE(Global::getCacheFolder(false), cache);
      QCOMPARE(Global::getLogFolder(false), logs);
      QVERIFY(!QFileInfo::exists(libraryRoot));
      QCOMPARE(Global::getDataFolder(Folder::ROAMING), support);
      QCOMPARE(Global::getDataFolder(Folder::LOCAL), support);
      QCOMPARE(Global::getCacheFolder(), cache);
      QCOMPARE(Global::getLogFolder(), logs);
      for (const auto& path : {support, cache, logs})
         QVERIFY(QFileInfo(path).isDir());
   }

   void sharedAcrossExecutables()
   {
      const auto originalName = QCoreApplication::applicationName();
      const auto originalOrganization = QCoreApplication::organizationName();
      const auto restore = qScopeGuard([&] {
         QCoreApplication::setApplicationName(originalName);
         QCoreApplication::setOrganizationName(originalOrganization);
      });
      for (const auto& name : {"D-LAN.Core", "D-LAN.GUI", "LogViewer", "PasswordHasher"})
      {
         QCoreApplication::setApplicationName(name);
         QCoreApplication::setOrganizationName("Different organization");
         QCOMPARE(Global::getDataFolder(Folder::ROAMING, false), libraryRoot + "/Application Support/D-LAN");
         QCOMPARE(Global::getDataFolder(Folder::LOCAL, false), libraryRoot + "/Application Support/D-LAN");
         QCOMPARE(Global::getCacheFolder(false), libraryRoot + "/Caches/D-LAN");
         QCOMPARE(Global::getLogFolder(false), libraryRoot + "/Logs/D-LAN");
      }
   }

   void independentOverrides()
   {
      const QString roaming = temporary->filePath("roaming");
      const QString local = temporary->filePath("local");
      QVERIFY(QDir().mkpath(roaming));
      QVERIFY(QDir().mkpath(local));
      Global::setDataFolder(Folder::ROAMING, roaming);
      QCOMPARE(Global::getDataFolder(Folder::ROAMING), roaming);
      QCOMPARE(Global::getCacheFolder(false), libraryRoot + "/Caches/D-LAN");
      QCOMPARE(Global::getLogFolder(false), libraryRoot + "/Logs/D-LAN");
      Global::setDataFolder(Folder::LOCAL, local);
      QCOMPARE(Global::getDataFolder(Folder::LOCAL), local);
      QCOMPARE(Global::getCacheFolder(), local);
      QCOMPARE(Global::getLogFolder(), local);
      Global::setDataFolderToDefault(Folder::LOCAL);
      QCOMPARE(Global::getDataFolder(Folder::ROAMING), roaming);
      QCOMPARE(Global::getDataFolder(Folder::LOCAL, false), libraryRoot + "/Application Support/D-LAN");
      QCOMPARE(Global::getCacheFolder(false), libraryRoot + "/Caches/D-LAN");
      QCOMPARE(Global::getLogFolder(false), libraryRoot + "/Logs/D-LAN");
   }

   void unavailableLocations()
   {
      missingLocation = true;
      QVERIFY_THROWS_EXCEPTION(Global::UnableToGetFolder, Global::getDataFolder(Folder::ROAMING, false));
      QVERIFY_THROWS_EXCEPTION(Global::UnableToGetFolder, Global::getDataFolder(Folder::LOCAL, false));
      QVERIFY_THROWS_EXCEPTION(Global::UnableToGetFolder, Global::getCacheFolder(false));
      QVERIFY_THROWS_EXCEPTION(Global::UnableToGetFolder, Global::getLogFolder(false));
   }

   void creationFailures()
   {
      QFile blocked(libraryRoot);
      QVERIFY(blocked.open(QIODevice::WriteOnly));
      blocked.close();
      QVERIFY_THROWS_EXCEPTION(Global::UnableToGetFolder, Global::getDataFolder(Folder::ROAMING));
      QVERIFY_THROWS_EXCEPTION(Global::UnableToGetFolder, Global::getDataFolder(Folder::LOCAL));
      QVERIFY_THROWS_EXCEPTION(Global::UnableToGetFolder, Global::getCacheFolder());
      QVERIFY_THROWS_EXCEPTION(Global::UnableToGetFolder, Global::getLogFolder());
   }

   void storageConsumers_data()
   {
      QTest::addColumn<QString>("logName");
      QTest::newRow("core") << QString("log_core");
      QTest::newRow("gui") << QString("log_gui");
   }

   void storageConsumers()
   {
      QFETCH(QString, logName);
      QProcess child;
      child.start(QCoreApplication::applicationFilePath(), {"--storage-child", libraryRoot, logName});
      QVERIFY(child.waitForFinished(10000));
      QCOMPARE(child.exitStatus(), QProcess::NormalExit);
      QVERIFY2(child.exitCode() == 0, child.readAllStandardError().constData());
      const QString support = libraryRoot + "/Application Support/D-LAN";
      const QString cache = libraryRoot + "/Caches/D-LAN";
      const QString logs = libraryRoot + "/Logs/D-LAN/" + logName;
      QVERIFY(QFileInfo::exists(cache + '/' + Common::Constants::HASH_CACHE_INDEX_FILENAME));
      const auto files = QDir(logs).entryList({"*.log"}, QDir::Files);
      QCOMPARE(files.size(), 1);
      QFile log(QDir(logs).filePath(files.first()));
      QVERIFY(log.open(QIODevice::ReadOnly));
      QVERIFY(log.readAll().contains("storage-location-marker"));
      QVERIFY(!QFileInfo::exists(support + '/' + logName));
      QVERIFY(!QFileInfo::exists(support + '/' + Common::Constants::HASH_CACHE_INDEX_FILENAME));
      // Discarding rebuildable caches must preserve configuration and state.
      QVERIFY(QDir(cache).removeRecursively());
      for (const auto& name : {Common::Constants::GUI_SETTINGS_FILENAME, Common::Constants::FILE_QUEUE, QString("chat/messages.json")})
         QVERIFY(QFileInfo::exists(support + '/' + name));
   }
};

int main(int argc, char** argv)
{
   QCoreApplication app(argc, argv);
   if (argc == 4 && QByteArray(argv[1]) == "--storage-child")
   {
      libraryRoot = QString::fromLocal8Bit(argv[2]);
      LM::Builder::setLogDirName(QString::fromLocal8Bit(argv[3]));
      try
      {
         Protos::GUI::Settings data;
         Common::PersistentData::setValue(Common::Constants::GUI_SETTINGS_FILENAME, data, Folder::ROAMING, true);
         Common::PersistentData::setValue(Common::Constants::FILE_QUEUE, data, Folder::LOCAL, true);
         if (!QDir().mkpath(Global::getDataFolder(Folder::LOCAL) + "/chat"))
            return 2;
         Common::PersistentData::setValue("chat/messages.json", data, Folder::LOCAL, true);
         const auto hashCache = HC::Builder::newHashCache(Global::getCacheFolder());
         if (!LM::Builder::newLogger("storage-test")->log("storage-location-marker", LM::SV_END_USER) || !LM::Builder::flush())
            return 3;
      }
      catch (...)
      {
         return 4;
      }
      return 0;
   }
   GlobalDarwinPathsTests tests;
   return QTest::qExec(&tests, argc, argv);
}

#include "GlobalDarwinPathsTests.moc"
