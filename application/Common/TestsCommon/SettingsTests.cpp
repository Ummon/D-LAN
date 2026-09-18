#include <atomic>
#include <thread>

#include <QTest>
#include <QTemporaryDir>

#include <Common/Settings.h>
#include <Protos/core_settings.pb.h>
#include <Protos/gui_settings.pb.h>

class SettingsTests : public QObject
{
   Q_OBJECT

private slots:
   void init()
   {
      SETTINGS.setSettingsMessage(new Protos::GUI::Settings());
   }

   void stringsAndBytesOwnTheirData()
   {
      const QString text = QString::fromUtf8("caf\xc3\xa9") + QChar(0) + QString(8192, QChar(0x3042));
      const QByteArray bytes = QByteArray::fromHex("00ff804100") + QByteArray(8192, '\xff');
      SETTINGS.set("core_address", text);
      SETTINGS.set("windows_state", bytes);
      const auto savedText = SETTINGS.get<QString>("core_address");
      const auto savedBytes = SETTINGS.get<QByteArray>("windows_state");
      QCOMPARE(savedText, text);
      QCOMPARE(savedBytes, bytes);

      SETTINGS.set("core_address", QString("changed"));
      SETTINGS.rm("windows_state");
      QCOMPARE(SETTINGS.get<QString>("core_address"), QString("changed"));
      QVERIFY(SETTINGS.get<QByteArray>("windows_state").isEmpty());
      SETTINGS.setSettingsMessage(new Protos::Core::Settings());
      QCOMPARE(savedText, text);
      QCOMPARE(savedBytes, bytes);
   }

   void scalarsEnumsAndMessages()
   {
      SETTINGS.set("socket_timeout", quint32(12345));
      SETTINGS.set("main_window_maximized", true);
      SETTINGS.set("download_view", quint32(Protos::GUI::Settings::LIST_VIEW));
      SETTINGS.set("language", QLocale("fr_CH"));
      const auto hash = Common::Hash::rand();
      SETTINGS.set("password", hash);
      QCOMPARE(SETTINGS.get<quint32>("socket_timeout"), quint32(12345));
      QVERIFY(SETTINGS.get<bool>("main_window_maximized"));
      QCOMPARE(SETTINGS.get<quint32>("download_view"), quint32(Protos::GUI::Settings::LIST_VIEW));
      QCOMPARE(SETTINGS.get<QLocale>("language"), QLocale("fr_CH"));
      QCOMPARE(SETTINGS.get<Common::Hash>("password"), hash);
      auto language = SETTINGS.get<Protos::Common::Language>("language");
      QCOMPARE(language.lang(), std::string("fr"));
      language.set_lang("de");
      SETTINGS.set("language", language);
      QCOMPARE(SETTINGS.get<QLocale>("language"), QLocale("de_CH"));
      SETTINGS.rm("language");
      QVERIFY(!SETTINGS.isSet("language"));
      QCOMPARE(language.country(), std::string("CH"));

      SETTINGS.setSettingsMessage(new Protos::Core::Settings());
      SETTINGS.set("salt", quint64(0xfedcba9876543210ULL));
      SETTINGS.set("time_recheck_chunk_factor", 1.25);
      QCOMPARE(SETTINGS.get<quint64>("salt"), quint64(0xfedcba9876543210ULL));
      QCOMPARE(SETTINGS.get<double>("time_recheck_chunk_factor"), 1.25);
   }

   void repeatedValues()
   {
      QVERIFY(SETTINGS.getRepeated<quint32>("search_column_sizes").isEmpty());
      SETTINGS.set("search_column_sizes", QList<quint32>{10, 20, 30});
      SETTINGS.set("search_column_sizes", 4, quint32(50));
      QCOMPARE(SETTINGS.getRepeated<quint32>("search_column_sizes"), (QList<quint32>{10, 20, 30, 0, 50}));
      SETTINGS.set("windowOrder", QList<quint32>{3, 2, 1, 0});
      SETTINGS.set("windowOrder", 1, quint32(0));
      QCOMPARE(SETTINGS.getRepeated<quint32>("windowOrder"), (QList<quint32>{3, 0, 1, 0}));
      SETTINGS.rm("windowOrder");
      QVERIFY(SETTINGS.getRepeated<quint32>("windowOrder").isEmpty());

      SETTINGS.setSettingsMessage(new Protos::Core::Settings());
      const QList<QString> rooms{QString(), QString::fromUtf8("caf\xc3\xa9"), QString("a") + QChar(0) + "b"};
      SETTINGS.set("joined_chat_rooms", rooms);
      const auto savedRooms = SETTINGS.getRepeated<QString>("joined_chat_rooms");
      QCOMPARE(savedRooms, rooms);
      SETTINGS.rm("joined_chat_rooms");
      QVERIFY(SETTINGS.getRepeated<QString>("joined_chat_rooms").isEmpty());
      QCOMPARE(savedRooms, rooms);

      Protos::Common::SharedEntry entry;
      entry.set_shared_name("shared");
      entry.set_path("/data/");
      entry.mutable_id()->set_hash(std::string(Common::Hash::HASH_SIZE, 'a'));
      SETTINGS.set("shared_entries", QList<Protos::Common::SharedEntry>{entry, entry});
      auto entries = SETTINGS.getRepeated<Protos::Common::SharedEntry>("shared_entries");
      QCOMPARE(entries.size(), qsizetype(2));
      QCOMPARE(entries[0].SerializeAsString(), entry.SerializeAsString());
      entries[0].set_path("/changed/");
      QCOMPARE(SETTINGS.getRepeated<Protos::Common::SharedEntry>("shared_entries")[0].path(), std::string("/data/"));
      SETTINGS.rm("shared_entries");
      QVERIFY(SETTINGS.getRepeated<Protos::Common::SharedEntry>("shared_entries").isEmpty());
      QCOMPARE(entries[1].SerializeAsString(), entry.SerializeAsString());
   }

   void replacementAndReload()
   {
      QTemporaryDir directory;
      QVERIFY(directory.isValid());
      SETTINGS.setFilename("settings-test.json");
      for (int i = 0; i < 3; ++i)
      {
         SETTINGS.setSettingsMessage(new Protos::Core::Settings());
         SETTINGS.set("nick", QString("core"));
         SETTINGS.set("socket_timeout", quint32(100));
         QVERIFY(SETTINGS.isSet("nick"));
         QVERIFY(!SETTINGS.isSet("core_address"));
         QCOMPARE(SETTINGS.get<quint32>("socket_timeout"), quint32(100));

         // The same field name has a different descriptor in the GUI schema.
         SETTINGS.setSettingsMessage(new Protos::GUI::Settings());
         QVERIFY(!SETTINGS.isSet("nick"));
         QVERIFY(!SETTINGS.isSet("unknown_setting"));
         QCOMPARE(SETTINGS.get<quint32>("socket_timeout"), quint32(0));
         SETTINGS.set("socket_timeout", quint32(200));
         SETTINGS.set("core_address", QString("gui"));
         QVERIFY(SETTINGS.saveToACustomDirectory(directory.path()));
         SETTINGS.rm("socket_timeout");
         SETTINGS.rm("core_address");
         QVERIFY(SETTINGS.loadFromACustomDirectory(directory.path()));
         QCOMPARE(SETTINGS.get<quint32>("socket_timeout"), quint32(200));
         QCOMPARE(SETTINGS.get<QString>("core_address"), QString("gui"));
      }
   }

   void concurrentReplacement()
   {
      auto& settings = SETTINGS;
      settings.set("socket_timeout", quint32(100));
      std::atomic<bool> start{false};
      std::atomic<bool> valid{true};
      std::thread reader([&]
      {
         while (!start.load())
            std::this_thread::yield();
         for (int i = 0; i < 5000; ++i)
         {
            const auto timeout = settings.get<quint32>("socket_timeout");
            if (timeout != 100 && timeout != 200)
               valid = false;
         }
      });
      start = true;
      for (int i = 0; i < 200; ++i)
      {
         auto* core = new Protos::Core::Settings();
         core->set_socket_timeout(100);
         settings.setSettingsMessage(core);
         auto* gui = new Protos::GUI::Settings();
         gui->set_socket_timeout(200);
         settings.setSettingsMessage(gui);
      }
      reader.join();
      QVERIFY(valid.load());
   }

   void cleanupTestCase()
   {
      SETTINGS.free();
   }
};

QTEST_GUILESS_MAIN(SettingsTests)
#include "SettingsTests.moc"
