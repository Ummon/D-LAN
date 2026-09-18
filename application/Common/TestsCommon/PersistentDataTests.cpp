#include <QFile>
#include <QJsonDocument>
#include <QJsonObject>
#include <QStringConverter>
#include <QTemporaryDir>
#include <QTest>

#include <Common/PersistentData.h>
#include <Protos/gui_settings.pb.h>

using Common::PersistentData;
using Common::PersistentDataIOException;
using Common::UnknownValueException;
using DataFolderType = Common::Global::DataFolderType;

class PersistentDataTests : public QObject
{
   Q_OBJECT

private slots:
   void roundTrip_data()
   {
      QTest::addColumn<bool>("humanReadable");
      QTest::newRow("JSON") << true;
      QTest::newRow("binary") << false;
   }

   void roundTrip()
   {
      QFETCH(bool, humanReadable);
      QTemporaryDir directory;
      QVERIFY(directory.isValid());
      const QString text = QString::fromUtf8("caf\xc3\xa9 \xf0\x9f\x98\x80") + QChar(0) + QString(8192, QChar(0x3042));
      const QByteArray bytes = QByteArray::fromHex("00ff804100");
      Protos::GUI::Settings source;
      source.set_core_address(text.toStdString());
      source.set_windows_state(bytes.toStdString());
      source.set_download_view(Protos::GUI::Settings::LIST_VIEW);
      source.add_search_column_sizes(42);
      source.mutable_language()->set_lang("fr");
      source.mutable_language()->set_country("CH");

      const auto save = [&]
      {
         PersistentData::setValue(directory.path(), "settings", source, DataFolderType::ROAMING, humanReadable);
      };
      const auto load = [&]
      {
         Protos::GUI::Settings restored;
         PersistentData::getValue(directory.path(), "settings", restored, DataFolderType::ROAMING, humanReadable);
         return restored.SerializeAsString();
      };
      save();
      QCOMPARE(load(), source.SerializeAsString());
      QVERIFY(!QFile::exists(directory.filePath("settings.temp")));

      if (humanReadable)
      {
         QFile file(directory.filePath("settings"));
         QVERIFY(file.open(QIODevice::ReadOnly));
         const QByteArray json = file.readAll();
         QVERIFY(!json.startsWith("\xef\xbb\xbf"));
         const auto document = QJsonDocument::fromJson(json);
         QVERIFY(document.isObject());
         const auto object = document.object();
         QCOMPARE(object.value("core_address").toString(), text);
         QCOMPARE(object.value("windows_state").toString().toLatin1(), bytes.toBase64());
         QCOMPARE(object.value("download_view").toString(), QString("LIST_VIEW"));
         QVERIFY(object.contains("main_window_width")); // Default fields still printed.
         QVERIFY(json.contains('\n')); // Human-readable whitespace still printed.
      }

      source.set_core_address("replacement");
      save();
      QCOMPARE(load(), source.SerializeAsString());
   }

   void readEditorEncodings_data()
   {
      QTest::addColumn<int>("encoding");
      QTest::addColumn<bool>("bom");
      QTest::newRow("UTF-8") << int(QStringConverter::Utf8) << false;
      QTest::newRow("UTF-8 BOM") << int(QStringConverter::Utf8) << true;
      QTest::newRow("UTF-16 LE") << int(QStringConverter::Utf16LE) << true;
      QTest::newRow("UTF-16 BE") << int(QStringConverter::Utf16BE) << true;
      QTest::newRow("UTF-32 LE") << int(QStringConverter::Utf32LE) << true;
      QTest::newRow("UTF-32 BE") << int(QStringConverter::Utf32BE) << true;
   }

   void readEditorEncodings()
   {
      QFETCH(int, encoding);
      QFETCH(bool, bom);
      QTemporaryDir directory;
      QVERIFY(directory.isValid());
      const QString text = QString::fromUtf8("caf\xc3\xa9 \xf0\x9f\x98\x80") + QChar(0) + QChar(0xfeff);
      const QString json = QString::fromUtf8(QJsonDocument(QJsonObject{
         {"core_address", text}, {"unknown_future_field", 123}
      }).toJson());
      QStringEncoder encoder(static_cast<QStringConverter::Encoding>(encoding),
         bom ? QStringConverter::Flag::WriteBom : QStringConverter::Flag::Default);
      const QByteArray encoded = encoder(json);
      QFile file(directory.filePath("settings.json"));
      QVERIFY(file.open(QIODevice::WriteOnly));
      QCOMPARE(file.write(encoded), qint64(encoded.size()));
      file.close();

      Protos::GUI::Settings restored;
      PersistentData::getValue(directory.path(), "settings.json", restored, DataFolderType::ROAMING, true);
      QCOMPARE(QString::fromStdString(restored.core_address()), text);
   }

   void readFailures()
   {
      QTemporaryDir directory;
      QVERIFY(directory.isValid());
      Protos::GUI::Settings restored;
      QVERIFY_THROWS_EXCEPTION(UnknownValueException,
         PersistentData::getValue(directory.path(), "missing.json", restored, DataFolderType::ROAMING, true));

      for (const QByteArray& invalid : {QByteArray(), QByteArray("{\"core_address\":")})
      {
         QFile file(directory.filePath("invalid.json"));
         QVERIFY(file.open(QIODevice::WriteOnly));
         QCOMPARE(file.write(invalid), qint64(invalid.size()));
         file.close();
         QVERIFY_THROWS_EXCEPTION(PersistentDataIOException,
            PersistentData::getValue(directory.path(), "invalid.json", restored, DataFolderType::ROAMING, true));
      }
   }
};

QTEST_GUILESS_MAIN(PersistentDataTests)
#include "PersistentDataTests.moc"
