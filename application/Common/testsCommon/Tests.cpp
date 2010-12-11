#include <Tests.h>

#include <QtDebug>
#include <QByteArray>
#include <QFile>
#include <QDir>
#include <QElapsedTimer>

#include <Protos/common.pb.h>
#include <Protos/core_settings.pb.h>

#include <Network.h>
#include <PersistentData.h>
#include <Settings.h>
#include <Global.h>
#include <ZeroCopyStreamQIODevice.h>
using namespace Common;

Tests::Tests()
{
}

void Tests::initTestCase()
{
   qDebug() << "Application folder path (where is put the settings and persistent data) : " << APPLICATION_FOLDER_PATH;
}

void Tests::nCombinations()
{
   QCOMPARE(Global::nCombinations(5, 4), 5);
   QCOMPARE(Global::nCombinations(4, 2), 6);
   QCOMPARE(Global::nCombinations(4, 4), 1);
   QCOMPARE(Global::nCombinations(42, 6), 5245786);

   QCOMPARE(Global::nCombinations(2, 4), 0);
   QCOMPARE(Global::nCombinations(-1, 1), 0);
   QCOMPARE(Global::nCombinations(1, -1), 0);
   QCOMPARE(Global::nCombinations(-1, -1), 0);
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

void Tests::availableDiskSpace()
{
   qDebug() << "Available disk space [Mo] : " << Global::availableDiskSpace(".") / 1024 / 1024;
}

void Tests::writePersistentData()
{
   this->hash = Hash::rand();
   Protos::Common::Hash hashMessage;
   hashMessage.set_hash(this->hash.getData(), Hash::HASH_SIZE);
   PersistentData::setValue("paul", hashMessage);
}

void Tests::readPersistentData()
{
   Protos::Common::Hash hashMessage;
   PersistentData::getValue("paul", hashMessage);
   Hash hashRead(hashMessage.hash().data());

   QVERIFY(this->hash == hashRead);

   try
   {
      PersistentData::getValue("john", hashMessage);
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
   QVERIFY(PersistentData::rmValue("paul"));
}

void Tests::writeSettings()
{
   this->hash = Hash::rand();

   SETTINGS.setFilename("tests_core_settings.txt");
   SETTINGS.setSettingsMessage(new Protos::Core::Settings());

   SETTINGS.set("nick", QString("paul"));
   SETTINGS.set("peerID", this->hash);
   SETTINGS.save();
}

void Tests::readSettings()
{
   SETTINGS.load();

   QString nick;
   SETTINGS.get("nick", nick);
   Hash hash;
   SETTINGS.get("peerID", hash);

   QCOMPARE(nick, QString("paul"));
   QCOMPARE(hash.toStr(), this->hash.toStr());
}

void Tests::removeSettings()
{
   SETTINGS.remove();
}

void Tests::generateAHash()
{
   qDebug() << Hash::rand().toStrCArray();

   const char array[Hash::HASH_SIZE] = {
      0x2d, 0x73, 0x73, 0x6f,
      0x34, 0xa7, 0x38, 0x37,
      0xd4, 0x22, 0xf7, 0xab,
      0xa2, 0x74, 0x0d, 0x84,
      0x09, 0xac, 0x60, 0xdf,
      0x34, 0x41, 0xa8, 0x12,
      0x0e, 0xca, 0x3b, 0x83
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
   QString str("12d733783b6f34a2637e3f83732afd422f7aba2b740d8409ac60dfd3");
   Hash h = Hash::fromStr(str);
   QCOMPARE(h.toStr(), str);
}

void Tests::compareTwoHash()
{
   const char array[Hash::HASH_SIZE] = {
      0x2d, 0x73, 0x73, 0x6f,
      0x34, 0xa7, 0x38, 0x37,
      0xd4, 0x22, 0xf7, 0xab,
      0xa2, 0x74, 0x0d, 0x84,
      0x09, 0xac, 0x60, 0xdf,
      0x34, 0x41, 0xa8, 0x12,
      0x0e, 0xca, 0x3b, 0x83
   };
   QByteArray byteArray(array, Hash::HASH_SIZE);
   QString str("2d73736f34a73837d422f7aba2740d8409ac60df3441a8120eca3b83");

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
   hasher.addPredefinedSalt();
   hasher.addData(str1, sizeof(str1));
   Hash h4 = hasher.getResult();

   hasher.reset();
   hasher.addPredefinedSalt();
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
   const char data[Network::HEADER_SIZE] = {
      0x00, 0x00, 0x00, 0x01,
      0x00, 0x00, 0x00, 0x2a,
      0x2d, 0x73, 0x73, 0x6f,
      0x34, 0xa7, 0x38, 0x37,
      0xd4, 0x22, 0xf7, 0xab,
      0xa2, 0x74, 0x0d, 0x84,
      0x09, 0xac, 0x60, 0xdf,
      0x34, 0x41, 0xa8, 0x12,
      0x0e, 0xca, 0x3b, 0x83
   };

   const QString peerID("2d73736f34a73837d422f7aba2740d8409ac60df3441a8120eca3b83");

   Network::MessageHeader<Network::CoreMessageType> header = Network::readHeader<Network::CoreMessageType>(data);
   qDebug() << header.toStr();

   QVERIFY(!header.isNull());
   QCOMPARE(header.type, Network::CORE_IM_ALIVE);
   QCOMPARE(header.size, 42u);
   QCOMPARE(header.senderID.toStr(), peerID);

   // We use a larger buffer to check if the last four bytes has been alterate.
   char buffer[Network::HEADER_SIZE + 4];
   memset(buffer, 0, sizeof(buffer));

   Network::writeHeader<Network::CoreMessageType>(buffer, header);
   QVERIFY(qstrncmp(data, buffer, Network::HEADER_SIZE) == 0);
   QVERIFY(qstrncmp(buffer + Network::HEADER_SIZE, "\0\0\0\0", 4) == 0);
}

void Tests::readAndWriteWithZeroCopyStreamQIODevice()
{
   QString filePath(QDir::tempPath().append("/test.bin"));
   QFile file(filePath);
   file.remove();

   Hash hash1 = Hash::fromStr("3178e82063eb9399667a3fd13c5ad856db40189977bf9c40f3e03b42");
   Hash hash2 = Hash::fromStr("c5bacce0dca8c6a48f3b60d758c7bc9ee1ced3b423d6f7835877c65a");

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



