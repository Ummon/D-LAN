#include <Core.h>
using namespace CoreSpace;

#include <QObject>
#include <QThread>
#include <QRegExp>

#include <Protos/core_settings.pb.h>

#include <Common/PersistentData.h>
#include <Common/Constants.h>
#include <FileManager/Builder.h>
#include <PeerManager/Builder.h>
#include <UploadManager/Builder.h>
#include <DownloadManager/Builder.h>
#include <NetworkListener/Builder.h>
#include <RemoteControlManager/Builder.h>

Core::Core(int argc, char **argv)
   : QtService<QCoreApplication>(argc, argv, Common::SERVICE_NAME)
{
   GOOGLE_PROTOBUF_VERIFY_VERSION;

   SETTINGS.setFilename("core_settings.txt");
   SETTINGS.setSettingsMessage(new Protos::Core::Settings());
   SETTINGS.load();

   try
   {
      SETTINGS.save(); // To automatically create the file if it doesn't exist.
   }
   catch(Common::PersistentDataIOException& err)
   {
      L_ERRO(err.message);
   }

   this->setServiceDescription("A LAN file sharing system");
   this->setStartupType(QtServiceController::ManualStartup);
   this->setServiceFlags(QtServiceBase::Default);
}

void Core::start()
{
   QThread::currentThread()->setObjectName("Core");

   this->checkSettingsIntegrity();

   L_USER("Starting..");

   this->fileManager = FM::Builder::newFileManager();
   this->peerManager = PM::Builder::newPeerManager(this->fileManager);
   this->uploadManager = UM::Builder::newUploadManager(this->fileManager, this->peerManager);
   this->downloadManager = DM::Builder::newDownloadManager(this->fileManager, this->peerManager);
   this->networkListener = NL::Builder::newNetworkListener(this->fileManager, this->peerManager, this->downloadManager);
   this->remoteControlManager = RCM::Builder::newRemoteControlManager(this->fileManager, this->peerManager, this->uploadManager, this->downloadManager, this->networkListener);

   L_USER("Ready to serve");
}

void Core::stop()
{
   this->application()->quit();
}

/**
  * Check if each value settings is valid, for example buffer_size cannot be one byte or 3 TiB..
  */
void Core::checkSettingsIntegrity()
{
   SETTINGS.rm("chunk_size"); // The size of the chunks must never change.

   this->checkSetting("buffer_size", 1024u, 32u * 1024u * 1024u, true);
   this->checkSetting("socket_buffer_size", 1024u, 32u * 1024u * 1024u, true);
   this->checkSetting("socket_timeout", 1000u, 60u * 1000u);

   this->checkSetting("minimum_duration_when_hashing", 100u, 30u * 1000u);
   this->checkSetting("time_between_rescan", 1000u, 60u * 60u * 1000u);
   QRegExp unfinishedSuffixExp("^\\.\\S+$");
   if (!unfinishedSuffixExp.exactMatch(SETTINGS.get<QString>("unfinished_suffix_term")))
   {
      L_ERRO("Settings : 'unfinished_suffix_term' must begin with a dot and not contain any space character");
      SETTINGS.rm("unfinished_suffix_term");
   }
   this->checkSetting("minimum_free_space", 0u, 4294967295u);

   this->checkSetting("pending_socket_timeout", 10u, 30u * 1000u);
   this->checkSetting("peer_timeout_factor", 1u, 10u);
   this->checkSetting("idle_socket_timeout", 1000u, 60u * 60u * 1000u);
   this->checkSetting("max_number_idle_socket", 0u, 10u);

   this->checkSetting("number_of_downloader", 1u, 10u);
   this->checkSetting("lan_speed", 1024u * 1024u, 1024u * 1024u * 1024u);
   this->checkSetting("time_recheck_chunk_factor", 1.0, 10.0);
   this->checkSetting("switch_to_another_peer_factor", 1.0, 10.0);
   this->checkSetting("download_rate_valid_time_factor", 100u, 100000u);
   this->checkSetting("peer_imalive_period", 1000u, 60u * 1000u);

   this->checkSetting("upload_live_time", 0u, 30u * 1000u);

   this->checkSetting("unicast_base_port", 1u, 65535u);
   this->checkSetting("multicast_port", 1u, 65535u);
   this->checkSetting("multicast_ttl", 1u, 255u);
   this->checkSetting("max_udp_datagram_size", 255u, 65535u);
   this->checkSetting("number_of_hashes_sent_imalive", 1u, 1000u);
   this->checkSetting("protocol_version", 1u, 10000u);
   this->checkSetting("max_number_of_search_result_to_send", 1u, 10000u);
}




