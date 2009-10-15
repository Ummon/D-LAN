#include <Tests.h>
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
   qDebug("read value : " + value);
   QVERIFY(value == "42 years old");
}

