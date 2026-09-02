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

#include <Protos/core_settings.pb.h>

#include <Common/Settings.h>

#include <Tests.h>

Protos::Core::Settings* createDefaultValuesSettings();

int main(int argc, char *argv[])
{
   QCoreApplication a(argc, argv);

   SETTINGS.setFilename("core_settings_network_listener_tests.json");
   SETTINGS.setSettingsMessage(createDefaultValuesSettings());
   SETTINGS.save();

   Tests tests;
   const int ret = QTest::qExec(&tests, argc, argv);

   SETTINGS.free();
   google::protobuf::ShutdownProtobufLibrary();

   return ret;
}

/**
  * Same values as 'Core::createDefaultValuesSettings()' for the components used by the tests.
  */
Protos::Core::Settings* createDefaultValuesSettings()
{
   auto settings = new Protos::Core::Settings();
   settings->set_nick("test");
   settings->set_buffer_size_reading(131072);
   settings->set_buffer_size_writing(524288);
   settings->set_socket_buffer_size(131072);
   settings->set_socket_timeout(7000);

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

   ///// DownloadManager /////
   settings->set_number_of_downloader(3);
   settings->set_lan_speed(52428800);
   settings->set_time_recheck_chunk_factor(4);
   settings->set_switch_to_another_peer_factor(1.5);
   settings->set_download_rate_valid_time_factor(3000);
   settings->set_save_queue_period(60000);
   settings->set_block_duration_corrupted_data(30000);

   ///// UploadManager /////
   settings->set_upload_lifetime(5000);
   settings->set_upload_min_nb_thread(3);
   settings->set_upload_thread_lifetime(30000);

   ///// NetworkListener /////
   settings->set_unicast_base_port(59487);
   settings->set_multicast_port(59486);
   settings->set_multicast_group(3960285976);
   settings->set_channel("main");
   settings->set_multicast_ttl(31);
   settings->set_max_udp_datagram_size(16356);
   settings->set_max_imalive_throughput(1048576);
   settings->set_udp_buffer_size(163840);
   settings->set_max_number_of_search_result_to_send(300);
   settings->set_max_number_of_result_shown(5000);
   settings->set_listen_address("");
   settings->set_listen_any(Protos::Common::Interface_Address_Protocol_IPv6);

   return settings;
}
