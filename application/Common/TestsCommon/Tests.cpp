/**
  * D-LAN - A decentralized LAN file sharing software.
  * Copyright (C) 2010-2012 Greg Burri <greg.burri@gmail.com>
  *
  * This program is free software: you can redistribute it and/or modify
  * it under the terms of the GNU General Public License as published by
  * the Free Software Foundation, either version 3 of the License, or
  * (at your option) any later version.
  *
  * This program is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  *
  * You should have received a copy of the GNU General Public License
  * along with this program.  If not, see <http://www.gnu.org/licenses/>.
  */
  
#include <Tests.h>

#include <QtDebug>
#include <QByteArray>
#include <QFile>
#include <QDir>
#include <QElapsedTimer>

#include <Protos/common.pb.h>
#include <Protos/core_settings.pb.h>
#include <Protos/gui_protocol.pb.h>

#include <Network/MessageHeader.h>
#include <PersistentData.h>
#include <Settings.h>
#include <Global.h>
#include <ZeroCopyStreamQIODevice.h>
#include <ProtoHelper.h>
using namespace Common;

Tests::Tests()
{
}

void Tests::initTestCase()
{
   qDebug() << "Application directory path (where the settings and persistent data are put) : " << Global::getDataFolder(Common::Global::ROAMING, false);
}

void Tests::nCombinations()
{
   QCOMPARE(Global::nCombinations(5, 4), 5);
   QCOMPARE(Global::nCombinations(4, 2), 6);
   QCOMPARE(Global::nCombinations(4, 4), 1);
   QCOMPARE(Global::nCombinations(42, 6), 5245786);
}

void Tests::formatByteSize()
{
   QCOMPARE(Global::formatByteSize(-42), QString("0 B"));
   QCOMPARE(Global::formatByteSize(0), QString("0 B"));
   QCOMPARE(Global::formatByteSize(42), QString("42 B"));
   QCOMPARE(Global::formatByteSize(1023), QString("1023 B"));
   QCOMPARE(Global::formatByteSize(1024), QString("1.0 KiB"));
   QCOMPARE(Global::formatByteSize(1484), QString("1.4 KiB"));
   QCOMPARE(Global::formatByteSize(1485), QString("1.5 KiB"));
   QCOMPARE(Global::formatByteSize(1996), QString("1.9 KiB"));
   QCOMPARE(Global::formatByteSize(1997), QString("2.0 KiB"));
   QCOMPARE(Global::formatByteSize(1024 * 1484), QString("1.4 MiB"));
   QCOMPARE(Global::formatByteSize(1024 * 1485), QString("1.5 MiB"));
   QCOMPARE(Global::formatByteSize(1024 * 1996), QString("1.9 MiB"));
   QCOMPARE(Global::formatByteSize(1024 * 1997), QString("2.0 MiB"));
   QCOMPARE(Global::formatByteSize(42LL * 1024 * 1024 * 1024 * 1024 * 1024), QString("42.0 PiB"));
   QCOMPARE(Global::formatByteSize(42LL * 1020 * 1024 * 1024 * 1024 * 1024), QString("41.8 PiB"));


   QCOMPARE(Global::formatByteSize(-42, 2), QString("0 B"));
   QCOMPARE(Global::formatByteSize(0, 2), QString("0 B"));
   QCOMPARE(Global::formatByteSize(42, 2), QString("42 B"));
   QCOMPARE(Global::formatByteSize(1023, 2), QString("1023 B"));
   QCOMPARE(Global::formatByteSize(1024, 2), QString("1.00 KiB"));
   QCOMPARE(Global::formatByteSize(1484, 2), QString("1.45 KiB"));
   QCOMPARE(Global::formatByteSize(1485, 2), QString("1.45 KiB"));
   QCOMPARE(Global::formatByteSize(1996, 2), QString("1.95 KiB"));
   QCOMPARE(Global::formatByteSize(1997, 2), QString("1.95 KiB"));
   QCOMPARE(Global::formatByteSize(1024 * 1484, 2), QString("1.45 MiB"));
   QCOMPARE(Global::formatByteSize(1024 * 1485, 2), QString("1.45 MiB"));
   QCOMPARE(Global::formatByteSize(1024 * 1996, 2), QString("1.95 MiB"));
   QCOMPARE(Global::formatByteSize(1024 * 1997, 2), QString("1.95 MiB"));
   QCOMPARE(Global::formatByteSize(42LL * 1024 * 1024 * 1024 * 1024 * 1024, 2), QString("42.00 PiB"));
   QCOMPARE(Global::formatByteSize(42LL * 1020 * 1024 * 1024 * 1024 * 1024, 2), QString("41.84 PiB"));
}

void Tests::formatTime()
{
   QCOMPARE(Global::formatTime(0), QString(""));
   QCOMPARE(Global::formatTime(60), QString("1m"));
   QCOMPARE(Global::formatTime(120), QString("2m"));
   QCOMPARE(Global::formatTime(160), QString("2m 40s"));
   QCOMPARE(Global::formatTime(12312411LL), QString("5M"));
   QCOMPARE(Global::formatTime(1243151412LL), QString("42y 9M"));
}

void Tests::availableDiskSpace()
{
   qDebug() << "Available disk space [Mo] : " << Global::availableDiskSpace(".") / 1024 / 1024;
}

void Tests::splitInWords()
{
    QCOMPARE(Global::splitInWords("a"), QStringList() << "a");
    QCOMPARE(Global::splitInWords("a b"), QStringList() << "a" << "b");
    QCOMPARE(Global::splitInWords("    a    b    "), QStringList() << "a" << "b");
    QCOMPARE(Global::splitInWords("a_b"), QStringList() << "a" << "b");
    QCOMPARE(Global::splitInWords("ABC DEF"), QStringList() << "abc" << "def");
    QCOMPARE(Global::splitInWords(QString::fromUtf8("àéè")), QStringList() << "aee");
    QCOMPARE(Global::splitInWords("abc%_-[]def"), QStringList() << "abc" << "def");
}

void Tests::hashStringToInt()
{
   QCOMPARE(Global::hashStringToInt(""), 0u);
   QCOMPARE(Global::hashStringToInt("abcde"), 444281822u);
   QCOMPARE(Global::hashStringToInt("abcdef"), 3174932005u);
}

void Tests::writePersistentData()
{
   this->hash = Hash::rand();
   Protos::Common::Hash hashMessage;
   hashMessage.set_hash(this->hash.getData(), Hash::HASH_SIZE);
   PersistentData::setValue("paul", hashMessage, Global::ROAMING);
}

void Tests::readPersistentData()
{
   Protos::Common::Hash hashMessage;
   PersistentData::getValue("paul", hashMessage, Global::ROAMING);
   Hash hashRead(hashMessage.hash().data());

   QVERIFY(this->hash == hashRead);

   try
   {
      PersistentData::getValue("john", hashMessage, Global::ROAMING);
      QFAIL("'john' shouldn't exist");
   }
   catch (UnknownValueException)
   {
      qDebug() << "Ok, exception UnknownValueException catched for the value 'john'";
   }
   catch (...)
   {
      QFAIL("Unknown exception occured");
   }
}

void Tests::removePersistentData()
{
   QVERIFY(PersistentData::rmValue("paul", Global::ROAMING));
}

void Tests::writeSettings()
{
   this->hash = Hash::rand();

   SETTINGS.setFilename("tests_core_settings.txt");
   SETTINGS.setSettingsMessage(new Protos::Core::Settings());

   SETTINGS.set("nick", QString("paul"));
   SETTINGS.set("peer_id", this->hash);
   SETTINGS.save();
}

void Tests::readSettings()
{
   SETTINGS.load();

   QString nick = SETTINGS.get<QString>("nick");
   Hash hash = SETTINGS.get<Hash>("peer_id");

   QCOMPARE(nick, QString("paul"));
   QCOMPARE(hash.toStr(), this->hash.toStr());
}

void Tests::removeSettings()
{
   SETTINGS.remove();
}

void Tests::generateAHash()
{
   const char array[Hash::HASH_SIZE] = {
       0x2d,  0x73,  0x73,  0x6f,
       0x34, -0x59,  0x38,  0x37,
      -0x2C,  0x22, -0x09, -0x55,
      -0x5E,  0x74,  0x0D, -0x7C,
       0x09, -0x54,  0x60, -0x21
   };
   QByteArray byteArray(array, Hash::HASH_SIZE);

   qDebug() << "Reference                     : " << byteArray.toHex();;

   Hash h1 = Hash::rand();
   qDebug() << "h1 (Randomly generated hash)  : " << h1.toStr();

   Hash h2(byteArray);
   qDebug() << "h2 (from QByteArray)          : " << h2.toStr();
   QVERIFY(memcmp(h2.getData(), array, Hash::HASH_SIZE) == 0);

   Hash h3(h2);
   qDebug() << "h3 (copied from h2)           : " << h3.toStr();
   QVERIFY(memcmp(h3.getData(), array, Hash::HASH_SIZE) == 0);

   Hash h4(array);
   qDebug() << "h4 (from char[])              : " << h4.toStr();
   QVERIFY(memcmp(h4.getData(), array, Hash::HASH_SIZE) == 0);
}

void Tests::buildAnHashFromAString()
{
   QString str("2d73736f34a73837d422f7aba2740d8409ac60df");
   Hash h = Hash::fromStr(str);
   QCOMPARE(h.toStr(), str);
}

void Tests::compareTwoHash()
{
   const char array[Hash::HASH_SIZE] = {
       0x2d,  0x73,  0x73,  0x6f,
       0x34, -0x59,  0x38,  0x37,
      -0x2C,  0x22, -0x09, -0x55,
      -0x5E,  0x74,  0x0D, -0x7C,
       0x09, -0x54,  0x60, -0x21
   };
   QByteArray byteArray(array, Hash::HASH_SIZE);
   QString str("2d73736f34a73837d422f7aba2740d8409ac60df");

   Hash h1 = Hash::fromStr(str);
   Hash h2(byteArray);
   Hash h3 = h1;
   Hash h4;
   h4 = h1;

   QVERIFY(h1 == h1);
   QVERIFY(h1 == h2);
   QVERIFY(h1 == h3);
   QVERIFY(h1 == h4);
   QVERIFY(h2 == h3);
   QVERIFY(h2 == h4);
}

void Tests::hasher()
{
   char str1[] = "abc";
   char str2[] = "abc";
   char str3[] = "cba";

   Hasher hasher;
   hasher.addData(str1, sizeof(str1));
   hasher.addData(str3, sizeof(str3));
   Hash h1 = hasher.getResult();

   hasher.reset();
   hasher.addData(str2, sizeof(str2));
   hasher.addData(str3, sizeof(str3));
   Hash h2 = hasher.getResult();

   hasher.reset();
   hasher.addData(str3, sizeof(str3));
   Hash h3 = hasher.getResult();

   hasher.reset();
   hasher.addSalt(42);
   hasher.addData(str1, sizeof(str1));
   Hash h4 = hasher.getResult();

   hasher.reset();
   hasher.addSalt(42);
   hasher.addData(str2, sizeof(str2));
   Hash h5 = hasher.getResult();

   QVERIFY(h1 == h2);
   QVERIFY(h1 != h3);
   QVERIFY(h2 != h3);
   QVERIFY(h4 != h1);
   QVERIFY(h4 == h5);
}

void Tests::messageHeader()
{
   const char data[] = {
      0x00,  0x00,  0x00,  0x01,
      0x00,  0x00,  0x00,  0x2a,
      0x2d,  0x73,  0x73,  0x6f,
      0x34, -0x59,  0x38,  0x37,
     -0x2C,  0x22, -0x09, -0x55,
     -0x5E,  0x74,  0x0D, -0x7C,
      0x09, -0x54,  0x60, -0x21
   };

   const QString peerID("2d73736f34a73837d422f7aba2740d8409ac60df");

   MessageHeader header = MessageHeader::readHeader(data);
   qDebug() << header.toStr();

   QVERIFY(!header.isNull());
   QCOMPARE(header.getType(), MessageHeader::CORE_IM_ALIVE);
   QCOMPARE(header.getSize(), 42u);
   QCOMPARE(header.getSenderID().toStr(), peerID);

   // We use a larger buffer to check if the last four bytes has been alterate.
   char buffer[MessageHeader::HEADER_SIZE + 4];
   memset(buffer, 0, sizeof(buffer));

   MessageHeader::writeHeader(buffer, header);
   QVERIFY(qstrncmp(data, buffer, MessageHeader::HEADER_SIZE) == 0);
   QVERIFY(qstrncmp(buffer + MessageHeader::HEADER_SIZE, "\0\0\0\0", 4) == 0);
}

void Tests::readAndWriteWithZeroCopyStreamQIODevice()
{
   QString filePath(QDir::tempPath().append("/test.bin"));
   QFile file(filePath);
   file.remove();

   Hash hash1 = Hash::fromStr("2c583d414e4a9eb956228209b367e48f59078a4b");
   Hash hash2 = Hash::fromStr("5c9c3741bded231f84b8a8200eaf3e30a9c0a951");

   qDebug() << "hash1 : " << hash1.toStr();
   qDebug() << "hash2 : " << hash2.toStr();

   Protos::Common::Hash hashMessage1;
   Protos::Common::Hash hashMessage2;

   file.open(QIODevice::WriteOnly);
   {
      ZeroCopyOutputStreamQIODevice outputStream(&file);

      hashMessage1.set_hash(hash1.getData(), Hash::HASH_SIZE);
      hashMessage1.SerializeToZeroCopyStream(&outputStream);

      hashMessage2.set_hash(hash2.getData(), Hash::HASH_SIZE);
      hashMessage2.SerializeToZeroCopyStream(&outputStream);
   }
   file.close();

   QFileInfo fileInfo(filePath);
   QCOMPARE(fileInfo.size(), static_cast<long long>(hashMessage1.ByteSize() + hashMessage2.ByteSize()));

   hashMessage1.Clear();
   hashMessage2.Clear();
   file.open(QIODevice::ReadOnly);
   {
      ZeroCopyInputStreamQIODevice inputStream(&file);
      hashMessage1.ParseFromBoundedZeroCopyStream(&inputStream, Hash::HASH_SIZE + 2);
      hashMessage2.ParseFromBoundedZeroCopyStream(&inputStream, Hash::HASH_SIZE + 2);
   }
   file.close();

   QCOMPARE(QByteArray(hashMessage1.hash().data(), Hash::HASH_SIZE), QByteArray(hash1.getData(), Hash::HASH_SIZE));
   QCOMPARE(QByteArray(hashMessage2.hash().data(), Hash::HASH_SIZE), QByteArray(hash2.getData(), Hash::HASH_SIZE));
}

/**
  * TODO: add some tests for these functions:
  *  - setLang(..)
  *  - getLang(..)
  *  - setIP(..)
  *  - getIP(..)
  *  - getRelativePath(..)
  */
void Tests::protoHelper()
{
   const QString path("path");
   const QString name("name");

   Protos::Common::Entry entry;
   entry.set_type(Protos::Common::Entry::FILE);
   entry.set_size(0);
   ProtoHelper::setStr(entry, &Protos::Common::Entry::set_path, path);
   ProtoHelper::setStr(entry, &Protos::Common::Entry::set_name, name);

   QCOMPARE(ProtoHelper::getStr(entry, &Protos::Common::Entry::path), path);
   QCOMPARE(ProtoHelper::getStr(entry, &Protos::Common::Entry::name), name);

   Protos::GUI::CoreSettings::SharedDirectories sharedDirs;
   const QList<QString> dirs = QList<QString>() << "abc" << "def" << "ghi";
   foreach (QString dir, dirs)
      ProtoHelper::addRepeatedStr(sharedDirs, &Protos::GUI::CoreSettings::SharedDirectories::add_dir, dir);
   for (int i = 0; i < dirs.size(); i++)
      QCOMPARE(ProtoHelper::getRepeatedStr(sharedDirs, &Protos::GUI::CoreSettings::SharedDirectories::dir, i), dirs[i]);

   for (int i = 0; i < 5; i++)
      entry.add_chunk()->set_hash(Hash::rand(i).getData(), Hash::HASH_SIZE);
   const QString debugStr = ProtoHelper::getDebugStr(entry);
   qDebug() << endl << "The protocol buffer message (Protos::Common::Entry):" << endl << debugStr;

   QVERIFY(debugStr.indexOf("ac2f75c043fbc36709d315f2245746d8588c3ac1") != -1);
   QVERIFY(debugStr.indexOf("25eb8c48ff89cb854fc09081cc47edfc8619b214") != -1);
   QVERIFY(debugStr.indexOf("a80fed48162bd24b6807a2b15f4bd52f3f1fda94") != -1);
   QVERIFY(debugStr.indexOf("6a98f983b8c80015fd93ca6bf9a98a9577a6e094") != -1);
   QVERIFY(debugStr.indexOf("7aaeb7c5816857c832893afc676d5e37b73968a4") != -1);
}


