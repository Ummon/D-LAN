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
      qDebug() << "Application folder path (where the persistent data is put) : " <<  Global::getDataFolder(Common::Global::LOCAL, false);
      qDebug() << "The file created during this test are put in : " << tempFolder;
   }
   catch(Common::Global::UnableToSetTempDirException& e)
   {
      QFAIL(e.errorMessage.toAscii().constData());
   }

   Common::PersistentData::rmValue(Common::Constants::FILE_CACHE, Common::Global::LOCAL); // Reset the stored cache.

   SETTINGS.setFilename("core_settings_file_manager_stress_tests.txt");
   SETTINGS.setSettingsMessage(new Protos::Core::Settings());
   SETTINGS.set("check_received_data_integrity", false);
}

/**
  * Some tasks will be performed concurrently.
  */
void StressTests::stressTest()
{
   qDebug() << "===== stressTest() =====";

   Common::PersistentData::rmValue(Common::Constants::FILE_CACHE, Common::Global::LOCAL);
   StressTest test;
}
