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

#include <Protos/core_settings.pb.h>

#include <Common/Settings.h>
#include <Common/Global.h>
#include <Common/LogManager/Builder.h>

#include <Tests.h>

Protos::Core::Settings* createDefaultValuesSettings();

int main(int argc, char *argv[])
{
   QCoreApplication a(argc, argv);

   QTemporaryDir settingsDir;
   if (!settingsDir.isValid())
      return 1;
   Common::Global::setDataFolder(Common::Global::DataFolderType::LOCAL, settingsDir.path());
   Common::Global::setDataFolder(Common::Global::DataFolderType::ROAMING, settingsDir.path());

   SETTINGS.setFilename("core_settings_peer_manager_tests.json");
   SETTINGS.setSettingsMessage(createDefaultValuesSettings());
   SETTINGS.save();

   Tests tests;
   int ret = QTest::qExec(&tests, argc, argv);

   SETTINGS.free();
   google::protobuf::ShutdownProtobufLibrary();

   return ret;
}

Protos::Core::Settings* createDefaultValuesSettings()
{
   auto settings = new Protos::Core::Settings();
   settings->set_buffer_size_reading(131072);
   settings->set_buffer_size_writing(524288);
   settings->set_socket_buffer_size(131072);
   settings->set_socket_timeout(7000);
   settings->set_upload_max_nb_connections(32);
   settings->set_upload_max_nb_connections_per_peer(4);

   ///// FileManager /////
   settings->set_minimum_duration_when_hashing(3000);
   settings->set_scan_period_unwatchable_dirs(30000);
   settings->set_unfinished_suffix_term(".unfinished");
   settings->set_minimum_free_space(1048576);
   settings->set_save_cache_period(60000);
   settings->set_check_received_data_integrity(true);
   settings->set_get_entries_timeout(5000);

   ///// PeerManager /////
   settings->set_pending_socket_timeout(10000);
   settings->set_peer_timeout_factor(3.2);
   settings->set_peer_imalive_period(5000);
   settings->set_idle_socket_timeout(60000);
   settings->set_max_number_idle_socket(6);
   settings->set_get_hashes_timeout(20000);

   return settings;
}
