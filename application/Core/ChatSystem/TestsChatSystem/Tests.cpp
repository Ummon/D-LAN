#include <QCoreApplication>
#include <QCryptographicHash>
#include <QDir>
#include <QFile>
#include <QFileInfo>
#include <QStringList>
#include <QTemporaryDir>
#include <QTest>
#include <memory>

#include <Common/Constants.h>
#include <Common/PersistentData.h>
#include <Common/Settings.h>
#include <Protos/core_settings.pb.h>
#include <priv/ChatMessages.h>

namespace
{
   const auto FOLDER = CS::ChatMessages::FOLDER_TYPE_MESSAGES_SAVED;

   QString roomFile(const QString& room)
   {
      const auto digest = QString::fromLatin1(QCryptographicHash::hash(room.toUtf8(), QCryptographicHash::Sha256).toHex());
      return Common::Constants::DIR_CHAT_MESSAGES + "/rooms/" + Common::Constants::FILE_CHAT_ROOM_MESSAGES.arg(digest);
   }

   Protos::Common::ChatMessages message(const QString& room, quint64 id)
   {
      Protos::Common::ChatMessages result;
      auto* entry = result.add_messages();
      entry->set_id(id);
      entry->set_time(1000);
      entry->set_message("saved message");
      entry->set_chat_room(room.toStdString());
      return result;
   }
}

class Tests : public QObject
{
   Q_OBJECT
private slots:
   void init()
   {
      this->directory = std::make_unique<QTemporaryDir>();
      QVERIFY(this->directory->isValid());
      Common::Global::setDataFolder(FOLDER, this->directory->path());
      QVERIFY(QDir(this->directory->path()).mkpath(Common::Constants::DIR_CHAT_MESSAGES));
   }

   void cleanup()
   {
      Common::Global::setDataFolderToDefault(FOLDER);
      this->directory.reset();
   }

   void distinctRoomHistories()
   {
      const QStringList rooms { "General", "general", "a/b", "a&#47;b", QString(10000, 'x'),
         QString(QChar(0x00E9)), QString("e") + QChar(0x0301) };
      for (int i = 0; i < rooms.size(); ++i)
      {
         CS::ChatMessages history;
         history.add(message(rooms[i], i + 1));
         history.saveForRoom(rooms[i]);
         QVERIFY(QFile::exists(this->directory->filePath(roomFile(rooms[i]))));
         QVERIFY(QFileInfo(roomFile(rooms[i])).fileName().size() < 100);
      }
      QCOMPARE(QDir(this->directory->filePath("chat/rooms")).entryList(QDir::Files).size(), rooms.size());
      for (int i = 0; i < rooms.size(); ++i)
      {
         CS::ChatMessages history;
         history.loadForRoom(rooms[i]);
         QCOMPARE(history.getLastMessageIDs(10), QList<quint64> { quint64(i + 1) });
      }
   }

   void rejectForeignMessagesInNewHistory()
   {
      QVERIFY(QDir(this->directory->path()).mkpath("chat/rooms"));
      auto mixed = message("General", 1);
      mixed.MergeFrom(message("general", 2));
      Common::PersistentData::setValue(roomFile("General"), mixed, FOLDER);
      CS::ChatMessages history;
      history.loadForRoom("General");
      QCOMPARE(history.getLastMessageIDs(10), QList<quint64> { 1 });
      history.saveForRoom("General");
      Protos::Common::ChatMessages saved;
      Common::PersistentData::getValue(roomFile("General"), saved, FOLDER);
      QCOMPARE(saved.messages_size(), 1);
   }

   void rejectCorruptHistory()
   {
      QVERIFY(QDir(this->directory->path()).mkpath("chat/rooms"));
      QFile corrupt(this->directory->filePath(roomFile("General")));
      QVERIFY(corrupt.open(QIODevice::WriteOnly));
      corrupt.write("not a history");
      corrupt.close();
      CS::ChatMessages history;
      history.loadForRoom("General");
      QVERIFY(history.getMessages().isEmpty());
   }

   void retryFailedSave()
   {
      QFile obstruction(this->directory->filePath("chat/rooms"));
      QVERIFY(obstruction.open(QIODevice::WriteOnly));
      obstruction.close();
      CS::ChatMessages history;
      history.add(message("General", 1));
      history.saveForRoom("General");
      QCOMPARE(history.getLastMessageIDs(10), QList<quint64> { 1 });
      QVERIFY(obstruction.remove());
      history.saveForRoom("General");
      QVERIFY(QFile::exists(this->directory->filePath(roomFile("General"))));
   }

   void mainChatKeepsItsFilename()
   {
      CS::ChatMessages history;
      history.add(message(QString(), 1));
      history.saveForRoom();
      QVERIFY(QFile::exists(this->directory->filePath("chat/" + Common::Constants::FILE_CHAT_MESSAGES)));
      CS::ChatMessages reloaded;
      reloaded.loadForRoom();
      QCOMPARE(reloaded.getLastMessageIDs(10), QList<quint64> { 1 });
   }

private:
   std::unique_ptr<QTemporaryDir> directory;
};

int main(int argc, char** argv)
{
   QCoreApplication app(argc, argv);
   auto* settings = new Protos::Core::Settings();
   settings->set_max_number_of_stored_chat_messages(500);
   SETTINGS.setSettingsMessage(settings);
   int result;
   {
      Tests tests;
      result = QTest::qExec(&tests, argc, argv);
   }
   SETTINGS.free();
   google::protobuf::ShutdownProtobufLibrary();
   return result;
}

#include "Tests.moc"
