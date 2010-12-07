#include <StressTests.h>

#include <QtDebug>

#include <Common/PersistentData.h>
#include <Common/Constants.h>

#include <StressTest.h>

StressTests::StressTests()
{
}

/**
  * Some tasks will be performed concurrently.
  */
void StressTests::stressTest()
{
   qDebug() << "===== stressTest() =====";

   Common::PersistentData::rmValue(Common::FILE_CACHE);
   StressTest test;
}
