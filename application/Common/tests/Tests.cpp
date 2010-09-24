#include <Tests.h>

#include <QtDebug>
#include <QByteArray>
#include <QFile>
#include <QDir>

#include <Protos/common.pb.h>

#include <Hash.h>
#include <Math.h>
#include <PersistantData.h>
#include <Global.h>
#include <ZeroCopyStreamQIODevice.h>
using namespace Common;

Tests::Tests()
{
}

void Tests::writePersistantData()
{
   //PersistantData::setValue("paul", "42 years old");
}

void Tests::readPersistantData()
{
   /* TODO :
   QByteArray value = PersistantData::getValue("paul");
   qDebug() << "read value : " << value;
   QVERIFY(value == "42 years old");

   try
   {
      PersistantData::getValue("john");
   }
   catch (Common::UnknownValueException)
   {
      qDebug() << "Ok, exception UnknownValueException catched for the value 'john'";
   }
   catch (...)
   {
      QFAIL("Unknown exception occured");
   }*/
}

void Tests::removePersistantData()
{
   /*QVERIFY(PersistantData::rmValue("paul"));
   QVERIFY(!PersistantData::rmValue("john"));*/
}

void Tests::generateAHash()
{
   char array[] = {
      0x2d, 0x73, 0x73, 0x6f,
      0x34, 0xa7, 0x38, 0x37,
      0xd4, 0x22, 0xf7, 0xab,
      0xa2, 0x74, 0x0d, 0x84,
      0x09, 0xac, 0x60, 0xdf
   };
   QByteArray byteArray(array, 20);

   qDebug() << "Reference            : " << byteArray.toHex();;

   Common::Hash h1 = Common::Hash::rand();
   qDebug() << "h1 (Generated hash)  : " << h1.toStr();

   Common::Hash h2(byteArray);
   qDebug() << "h2 (from QByteArray) : " << h2.toStr();
   QVERIFY(memcmp(h2.getData(), array, 20) == 0);

   Common::Hash h3(h2);
   qDebug() << "h3 (copied from h2)  : " << h3.toStr();
   QVERIFY(memcmp(h3.getData(), array, 20) == 0);

   Common::Hash h4(array);
   qDebug() << "h4 (from char[])     : " << h4.toStr();
   QVERIFY(memcmp(h4.getData(), array, 20) == 0);
}

void Tests::buildAnHashFromAString()
{
   QString str("2d73736f34a73837d422f7aba2740d8409ac60df");
   Common::Hash h = Common::Hash::fromStr(str);
   QCOMPARE(h.toStr(), str);
}

void Tests::compareTwoHash()
{
   char array[] = {
      0x2d, 0x73, 0x73, 0x6f,
      0x34, 0xa7, 0x38, 0x37,
      0xd4, 0x22, 0xf7, 0xab,
      0xa2, 0x74, 0x0d, 0x84,
      0x09, 0xac, 0x60, 0xdf
   };
   QByteArray byteArray(array, 20);
   QString str("2d73736f34a73837d422f7aba2740d8409ac60df");

   Common::Hash h1 = Common::Hash::fromStr(str);
   Common::Hash h2(byteArray);
   Common::Hash h3 = h1;
   Common::Hash h4;
   h4 = h1;

   QVERIFY(h1 == h1);
   QVERIFY(h1 == h2);
   QVERIFY(h1 == h3);
   QVERIFY(h1 == h4);
   QVERIFY(h2 == h3);
   QVERIFY(h2 == h4);
}

void Tests::math()
{
   QVERIFY(Common::Math::nCombinations(5, 4) == 5);
   QVERIFY(Common::Math::nCombinations(4, 2) == 6);
   QVERIFY(Common::Math::nCombinations(4, 4) == 1);
   QVERIFY(Common::Math::nCombinations(2, 4) == 0);
}

void Tests::availableDiskSpace()
{
   qDebug() << "Available disk space [Mo] : " << Common::Global::availableDiskSpace(".") / 1024 / 1024;
}


#include <google/protobuf/io/zero_copy_stream_impl.h>

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

      hashMessage1.set_hash(hash1.getData(),  Hash::HASH_SIZE);
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
      hashMessage1.ParseFromBoundedZeroCopyStream(&inputStream, 22);
      hashMessage2.ParseFromBoundedZeroCopyStream(&inputStream, 22);
   }
   file.close();

   QCOMPARE(QByteArray(hashMessage1.hash().data(), Hash::HASH_SIZE), QByteArray(hash1.getData(), Hash::HASH_SIZE));
   QCOMPARE(QByteArray(hashMessage2.hash().data(), Hash::HASH_SIZE), QByteArray(hash2.getData(), Hash::HASH_SIZE));
}



