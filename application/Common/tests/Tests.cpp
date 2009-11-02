#include <Tests.h>

#include <QtDebug>
#include <QByteArray>

#include <Hash.h>
#include <Math.h>
#include <PersistantData.h>
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

void Tests::math()
{
   QVERIFY(Common::Math::nCombinations(5, 4) == 5);
   QVERIFY(Common::Math::nCombinations(4, 2) == 6);
   QVERIFY(Common::Math::nCombinations(4, 4) == 1);
   QVERIFY(Common::Math::nCombinations(2, 4) == 0);
}



