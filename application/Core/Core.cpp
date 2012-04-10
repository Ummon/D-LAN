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
  
#include <Core.h>
using namespace CoreSpace;

#include <Protos/core_settings.pb.h>

#include <Common/PersistentData.h>
#include <Common/Constants.h>
#include <Common/Hash.h>
#include <Common/Languages.h>
#include <FileManager/Builder.h>
#include <PeerManager/Builder.h>
#include <UploadManager/Builder.h>
#include <DownloadManager/Builder.h>
#include <NetworkListener/Builder.h>
#include <RemoteControlManager/Builder.h>

LOG_INIT_CPP(Core);

Core::Core(bool resetSettings, QLocale locale)
{
   GOOGLE_PROTOBUF_VERIFY_VERSION;
   SETTINGS.setFilename(Common::Constants::CORE_SETTINGS_FILENAME);
   SETTINGS.setSettingsMessage(new Protos::Core::Settings());
   SETTINGS.load();

   if (resetSettings || locale != QLocale::system())
   {
      QString originalRoamingFolder = Common::Global::getDataFolder(Common::Global::ROAMING);

      /**
        * Reset the settings for the current application folder (i==0) and for the service application folder (i==1).
        * The service folder may not be accessible depending of the rights of the current user.
        */
      for (int i = 0; i < 2; i++)
      {
         if (i == 1)
         {
            QString roamingSystem = Common::Global::getDataServiceFolder(Common::Global::ROAMING);
            if (!QDir(roamingSystem).exists())
               break;

            Common::Global::setDataFolder(Common::Global::ROAMING, roamingSystem);
         }

         if (SETTINGS.load() && resetSettings)
         {
            const QString nick = SETTINGS.get<QString>("nick");
            const Common::Hash peerID = SETTINGS.get<Common::Hash>("peer_id");
            SETTINGS.saveTo(Common::Constants::CORE_SETTINGS_FILENAME + ".backup");
            SETTINGS.rmAll();
            SETTINGS.set("nick", nick);
            SETTINGS.set("peer_id", peerID);
            SETTINGS.save();
         }

         if (locale != QLocale::system())
         {
            this->setLanguage(locale, false);
         }
      }
      Common::Global::setDataFolder(Common::Global::ROAMING, originalRoamingFolder);
      SETTINGS.load();
   }

   this->checkSettingsIntegrity();

   // To automatically create the file if it doesn't exist.
   if (!SETTINGS.save())
   {
      L_ERRO("Unable to save settings");
   }
}

void Core::start()
{
   QThread::currentThread()->setObjectName("Core");

   QCoreApplication::instance()->installTranslator(&this->translator);
   this->setLanguage(SETTINGS.get<QLocale>("language"), true);

   L_USER(QObject::tr("Starting.."));

   this->fileManager = FM::Builder::newFileManager();
   this->peerManager = PM::Builder::newPeerManager(this->fileManager);
   this->uploadManager = UM::Builder::newUploadManager(this->peerManager);
   this->downloadManager = DM::Builder::newDownloadManager(this->fileManager, this->peerManager);
   this->networkListener = NL::Builder::newNetworkListener(this->fileManager, this->peerManager, this->uploadManager, this->downloadManager);
   this->remoteControlManager = RCM::Builder::newRemoteControlManager(this->fileManager, this->peerManager, this->uploadManager, this->downloadManager, this->networkListener);

   connect(this->remoteControlManager.data(), SIGNAL(languageDefined(QLocale)), this, SLOT(setLanguage(QLocale)));

   L_USER(QObject::tr("Ready to serve"));
}

void Core::setLanguage(QLocale locale, bool load)
{
   if (load)
   {
      Common::Languages languages(QCoreApplication::applicationDirPath() + "/" + Common::Constants::LANGUAGE_DIRECTORY);
      Common::Language lang = languages.getBestMatchLanguage(Common::Languages::CORE, locale);
      SETTINGS.set("language", lang.locale);
      SETTINGS.save();
      this->translator.load(lang.filename, QCoreApplication::applicationDirPath() + "/" + Common::Constants::LANGUAGE_DIRECTORY);
   }
   else
   {
      SETTINGS.set("language", locale);
      SETTINGS.save();
   }
}

/**
  * Check if each value settings is valid, for example buffer_size cannot be one byte or 3 TiB..
  */
void Core::checkSettingsIntegrity()
{
   SETTINGS.rm("chunk_size"); // The size of the chunks must never change.

   if (SETTINGS.get<QString>("nick").isEmpty())
      SETTINGS.set("nick", Common::Global::getCurrenMachineName());

   this->checkSetting("buffer_size_reading", 1024u, 32u * 1024u * 1024u, true);
   this->checkSetting("buffer_size_writing", 1024u, 32u * 1024u * 1024u, true);
   this->checkSetting("socket_buffer_size", 1024u, 32u * 1024u * 1024u, true);
   this->checkSetting("socket_timeout", 1000u, 60u * 1000u);

   this->checkSetting("minimum_duration_when_hashing", 100u, 30u * 1000u);
   this->checkSetting("scan_period_unwatchable_dirs", 1000u, 60u * 60u * 1000u);
   QRegExp unfinishedSuffixExp("^\\.\\S+$");
   if (!unfinishedSuffixExp.exactMatch(SETTINGS.get<QString>("unfinished_suffix_term")))
   {
      L_ERRO("Settings : 'unfinished_suffix_term' must begin with a dot and not contain any space character");
      SETTINGS.rm("unfinished_suffix_term");
   }
   this->checkSetting("minimum_free_space", 0u, 4294967295u);
   this->checkSetting("save_cache_period", 1000u, 4294967295u);

   this->checkSetting("pending_socket_timeout", 10u, 30u * 1000u);
   this->checkSetting("peer_timeout_factor", 1.0, 10.0);
   this->checkSetting("idle_socket_timeout", 1000u, 60u * 60u * 1000u);
   this->checkSetting("max_number_idle_socket", 0u, 10u);
   this->checkSetting("get_hashes_timeout", 1000u, 60u * 1000u);

   this->checkSetting("number_of_downloader", 1u, 10u);
   this->checkSetting("lan_speed", 1024u * 1024u, 1024u * 1024u * 1024u);
   this->checkSetting("time_recheck_chunk_factor", 1.0, 10.0);
   this->checkSetting("switch_to_another_peer_factor", 1.0, 10.0);
   this->checkSetting("download_rate_valid_time_factor", 100u, 100000u);
   this->checkSetting("peer_imalive_period", 1000u, 60u * 1000u);
   this->checkSetting("save_queue_period", 1000u, 4294967295u);
   this->checkSetting("ban_duration_corrupted_data", 0u, 60u * 60u * 1000u);

   this->checkSetting("upload_lifetime", 0u, 30u * 1000u);
   this->checkSetting("upload_min_nb_thread", 1u, 1000u);
   this->checkSetting("upload_thread_lifetime", 0u, 60u * 60u * 1000u);

   this->checkSetting("unicast_base_port", 1u, 65535u);
   this->checkSetting("multicast_port", 1u, 65535u);
   this->checkSetting("multicast_ttl", 1u, 255u);
   this->checkSetting("max_udp_datagram_size", 255u, 65535u);
   this->checkSetting("udp_read_buffer_size", 255u, 6684672u);
   this->checkSetting("max_number_of_search_result_to_send", 1u, 10000u);
   this->checkSetting("max_number_of_result_shown", 1u, 100000u);
   this->checkSetting("max_number_of_chat_message_saved", 1u, 1000000u);

   this->checkSetting("remote_control_port", 1u, 65535u);
   this->checkSetting("remote_refresh_rate", 500u, 10u * 1000u);
   this->checkSetting("remote_max_nb_connection", 1u, 1000u);
   this->checkSetting("search_lifetime", 1000u, 60 * 1000u);
   this->checkSetting("delay_gui_connection_fail", 0u, 10 * 1000u);
}
