#include <StressTests.h>

#include <QtDebug>
#include <QTest>

#include <Protos/core_settings.pb.h>

#include <Common/Settings.h>
#include <Common/PersistentData.h>
#include <Common/Constants.h>
#include <Common/Global.h>
#include <Common/LogManager/Builder.h>

#include <StressTest.h>

StressTests::StressTests()
{
}

void StressTests::initTestCase()
{
   LM::Builder::initMsgHandler();

   qDebug() << "===== initTestCase() =====";

   try
   {
      QString tempFolder = Common::Global::setCurrentDirToTemp("FileManagerStressTests");
      qDebug() << "Application folder path (where is put the persistent data) : " << APPLICATION_FOLDER_PATH;
      qDebug() << "The file created during this test are put in : " << tempFolder;
   }
   catch(Common::Global::UnableToSetTempDirException& e)
   {
      QFAIL(e.what());
   }

   Common::PersistentData::rmValue(Common::FILE_CACHE); // Reset the stored cache.

   SETTINGS.setFilename("core_settings_file_manager_stress_tests.txt");
   SETTINGS.setSettingsMessage(new Protos::Core::Settings());
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
