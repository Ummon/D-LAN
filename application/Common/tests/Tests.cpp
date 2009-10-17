#include <Tests.h>
using namespace Common;

#include <QtDebug>

#include <Hash.h>

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
}



