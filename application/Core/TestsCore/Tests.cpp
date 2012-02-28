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
  

#include <string>
using namespace std;

#include <QtDebug>
#include <QRegExp>
#include <QFile>
#include <QTextStream>
#include <QDataStream>
#include <QStringList>
#include <QDirIterator>

#include <Protos/core_settings.pb.h>

#include <Common/Constants.h>
#include <Common/Settings.h>
#include <Common/Global.h>
#include <Common/PersistentData.h>

Tests::Tests() :
   core("core_settings_tests.txt")
{
}

void Tests::initTestCase()
{
   LM::Builder::initMsgHandler();

   qDebug() << "===== initTestCase() =====";

   try
   {
      QString tempFolder = Common::Global::setCurrentDirToTemp("CoreTests");
      qDebug() << "Application folder path (where the persistent data is put) : " <<  Global::getDataFolder(Common::Global::LOCAL, false);
      qDebug() << "The file created during this test are put in : " << tempFolder;
   }
   catch(Common::Global::UnableToSetTempDirException& e)
   {
      QFAIL(e.getMessage().toAscii().constData());
   }

   Common::PersistentData::rmValue(Common::FILE_CACHE, Common::Global::LOCAL); // Reset the stored cache.

   // TODO :
   // Be able to give the local (queue+cache+log) AND roaming (settings) data folders to the core, be careful where are put the logs...
   // remove the ability to set the settings filename in Core constructor.
}

void Tests::cleanupTestCase()
{
   qDebug() << "===== cleanupTestCase() =====";
}

