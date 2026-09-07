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
  
#include <QCoreApplication>
#include <QTest>
#include <QTemporaryDir>
#include <Common/Global.h>
#include <Common/Settings.h>
#include <Protos/core_settings.pb.h>

#include <Tests.h>

int main(int argc, char *argv[])
{
   QCoreApplication a(argc, argv);

   QTemporaryDir settingsDir;
   if (!settingsDir.isValid())
      return 1;
   Common::Global::setDataFolder(Common::Global::DataFolderType::LOCAL, settingsDir.path());
   Common::Global::setDataFolder(Common::Global::DataFolderType::ROAMING, settingsDir.path());
   auto settings = new Protos::Core::Settings();
   settings->set_buffer_size_reading(131072);
   settings->set_check_received_data_integrity(true);
   settings->set_unfinished_suffix_term(".unfinished");
   settings->set_peer_timeout_factor(3.2);
   settings->set_peer_imalive_period(5000);
   SETTINGS.setFilename("core_settings_download_manager_tests.json");
   SETTINGS.setSettingsMessage(settings);

   Tests tests;
   return QTest::qExec(&tests, argc, argv);
}
