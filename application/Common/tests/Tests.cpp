#include <Tests.h>

#include <QtDebug>

#include <Hash.h>
#include <Math.h>
#include <PersistantData.h>
using namespace Common;

Tests::Tests()
{
}

void Tests::writePersistantData()
{
   PersistantData::setValue("paul", "42 years old");
}

void Tests::readPersistantData()
{
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
   }
}

void Tests::removePersistantData()
{
   QVERIFY(PersistantData::rmValue("paul"));
   QVERIFY(!PersistantData::rmValue("john"));
}

void Tests::generateAHash()
{
   Common::Hash hash = Common::Hash::rand();
   QVERIFY(hash.size() == 20);
   qDebug() << "Generated hash : " << hash.toStr();
}

void Tests::math()
{
   QVERIFY(Common::Math::nCombinations(5, 4) == 5);
   QVERIFY(Common::Math::nCombinations(4, 2) == 6);
   QVERIFY(Common::Math::nCombinations(4, 4) == 1);
   QVERIFY(Common::Math::nCombinations(2, 4) == 0);
}



