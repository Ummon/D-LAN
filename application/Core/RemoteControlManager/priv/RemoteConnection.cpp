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
  
#include <priv/RemoteConnection.h>
using namespace RCM;

#include <limits>

#include <QSet>
#include <QCoreApplication>
#include <QDateTime>
#include <QNetworkInterface>

#include <Common/Settings.h>
#include <Common/ProtoHelper.h>
#include <Common/Constants.h>
#include <Common/Hash.h>
#include <Common/SharedDir.h>
#include <Common/Global.h>
#include <Common/StringUtils.h>
#include <Core/FileManager/IChunk.h>
#include <Core/FileManager/Exceptions.h>
#include <Core/PeerManager/IPeer.h>
#include <Core/DownloadManager/IDownload.h>
#include <Core/UploadManager/IChunkUploader.h>

#include <priv/Log.h>

void RemoteConnection::Logger::logDebug(const QString& message)
{
   L_DEBU(message);
}

void RemoteConnection::Logger::logError(const QString& message)
{
   L_WARN(message);
}

RemoteConnection::RemoteConnection(
   QSharedPointer<FM::IFileManager> fileManager,
   QSharedPointer<PM::IPeerManager> peerManager,
   QSharedPointer<UM::IUploadManager> uploadManager,
   QSharedPointer<DM::IDownloadManager> downloadManager,
   QSharedPointer<NL::INetworkListener> networkListener,
   QSharedPointer<CS::IChatSystem> chatSystem,
   QTcpSocket* socket
) :
   MessageSocket(new RemoteConnection::Logger(), socket, peerManager->getSelf()->getID()),
   fileManager(fileManager),
   peerManager(peerManager),
   uploadManager(uploadManager),
   downloadManager(downloadManager),
   networkListener(networkListener),
   chatSystem(chatSystem),
   waitForStateResult(false),
   authenticated(false),
   saltChallenge(0)
 #if DEBUG
   ,loggerRefreshState(LM::Builder::newLogger("RemoteConnection (State)"))
 #endif
{
   L_DEBU(QString("New RemoteConnection from %1").arg(socket->peerAddress().toString()));

   this->authenticated = this->isLocal();

   this->startListening();
   if (!socket->isValid())
   {
      delete this;
      return;
   }

   this->refreshAllInterfaces();

   this->sendLogMessagesTimer.setInterval(SETTINGS.get<quint32>("delay_before_sending_log_messages"));
   this->sendLogMessagesTimer.setSingleShot(true);
   connect(&this->sendLogMessagesTimer, SIGNAL(timeout()), this, SLOT(sendLogMessages()));

   this->timerRefresh.setInterval(SETTINGS.get<quint32>("remote_refresh_rate"));
   this->timerRefresh.setSingleShot(true);
   connect(&this->timerRefresh, SIGNAL(timeout()), this, SLOT(refresh()));

   this->timerCloseSocket.setInterval(MAX_DELAY_WAITING_AUTH_RES);
   this->timerCloseSocket.setSingleShot(true);
   connect(&this->timerCloseSocket, SIGNAL(timeout()), this, SLOT(closeSocket()));

   connect(this->chatSystem.data(), SIGNAL(newMessages(Protos::Common::ChatMessages)), this, SLOT(newChatMessages(Protos::Common::ChatMessages)));

   this->loggerHook = LM::Builder::newLoggerHook(LM::Severity(LM::SV_FATAL_ERROR | LM::SV_ERROR | LM::SV_END_USER | LM::SV_WARNING));

   qRegisterMetaType<QSharedPointer<LM::IEntry>>("QSharedPointer<LM::IEntry>");
   connect(this->loggerHook.data(), SIGNAL(newLogEntry(QSharedPointer<LM::IEntry>)), this, SLOT(newLogEntry(QSharedPointer<LM::IEntry>)), Qt::QueuedConnection);

   this->askForAuthentication();
}

RemoteConnection::~RemoteConnection()
{
   L_DEBU(QString("RemoteConnection[%1] deleted").arg(this->num));
   emit deleted(this);
}

void RemoteConnection::send(Common::MessageHeader::MessageType type, const google::protobuf::Message& message)
{
   // When not authenticated we can only send messages of type 'GUI_AUTHENTICATION_RESULT' or 'GUI_ASK_FOR_AUTHENTICATION'.
   if (!this->authenticated && type != Common::MessageHeader::GUI_ASK_FOR_AUTHENTICATION && type != Common::MessageHeader::GUI_AUTHENTICATION_RESULT)
      return;

   Common::MessageSocket::send(type, message);
}

void RemoteConnection::refresh()
{
   if (this->waitForStateResult)
      return;

   const int downloadRate = this->downloadManager->getDownloadRate();
   const int uploadRate = this->uploadManager->getUploadRate();

   Protos::GUI::State state;

   state.set_integrity_check_enabled(SETTINGS.get<bool>("check_received_data_integrity"));
   state.set_password_defined(!SETTINGS.get<Common::Hash>("remote_password").isNull());

   // Ourself
   Protos::GUI::State::Peer* self = state.add_peer();
   self->mutable_peer_id()->set_hash(this->peerManager->getSelf()->getID().getData(), Common::Hash::HASH_SIZE);
   self->set_sharing_amount(this->fileManager->getAmount());
   self->set_download_rate(downloadRate);
   self->set_upload_rate(uploadRate);
   Common::ProtoHelper::setStr(*self, &Protos::GUI::State::Peer::set_nick, this->peerManager->getSelf()->getNick());
   Common::ProtoHelper::setStr(*self, &Protos::GUI::State::Peer::set_core_version, Common::Global::getVersionFull());

   // Peers.
   const QList<PM::IPeer*>& peers = this->peerManager->getPeers();
   for (QListIterator<PM::IPeer*> i(peers); i.hasNext();)
   {
      PM::IPeer* peer = i.next();
      Protos::GUI::State::Peer* protoPeer = state.add_peer();
      protoPeer->mutable_peer_id()->set_hash(peer->getID().getData(), Common::Hash::HASH_SIZE);
      Common::ProtoHelper::setStr(*protoPeer, &Protos::GUI::State::Peer::set_nick, peer->getNick());

      const QString coreVersion = peer->getCoreVersion();
      if (!coreVersion.isNull())
         Common::ProtoHelper::setStr(*protoPeer, &Protos::GUI::State::Peer::set_core_version, coreVersion);

      protoPeer->set_sharing_amount(peer->getSharingAmount());
      protoPeer->set_download_rate(peer->getDownloadRate());
      protoPeer->set_upload_rate(peer->getUploadRate());
      Common::ProtoHelper::setIP(*protoPeer->mutable_ip(), peer->getIP());
      protoPeer->set_status(
         peer->getProtocolVersion() == Common::Constants::PROTOCOL_VERSION ? Protos::GUI::State::Peer::OK :
         (peer->getProtocolVersion() < Common::Constants::PROTOCOL_VERSION ? Protos::GUI::State::Peer::VERSION_OUTDATED : Protos::GUI::State::Peer::MORE_RECENT_VERSION)
      );
   }

   // Downloads.
   const QList<DM::IDownload*>& downloads = this->downloadManager->getDownloads();
   for (QListIterator<DM::IDownload*> i(downloads); i.hasNext();)
   {
      DM::IDownload* download = i.next();
      Protos::GUI::State_Download* protoDownload = state.add_download();
      protoDownload->set_id(download->getID());
      protoDownload->mutable_local_entry()->CopyFrom(download->getLocalEntry());
      protoDownload->mutable_local_entry()->mutable_chunk()->Clear(); // We don't need to send the hashes.
      protoDownload->set_status(static_cast<Protos::GUI::State::Download::Status>(download->getStatus())); // Warning, enums must be compatible.
      protoDownload->set_downloaded_bytes(download->getDownloadedBytes());

      PM::IPeer* peerSource = download->getPeerSource();
      protoDownload->add_peer_id()->set_hash(peerSource->getID().getData(), Common::Hash::HASH_SIZE); // The first hash must be the source.
      QSet<PM::IPeer*> peers = download->getPeers();
      peers.remove(peerSource);
      for (QSetIterator<PM::IPeer*> j(peers); j.hasNext();)
         protoDownload->add_peer_id()->set_hash(j.next()->getID().getData(), Common::Hash::HASH_SIZE);

      if (!peerSource->getNick().isNull())
         Common::ProtoHelper::setStr(*protoDownload, &Protos::GUI::State::Download::set_peer_source_nick, peerSource->getNick());
   }

   // Uploads.
   QList<UM::IChunkUploader*> chunkUploaders = this->uploadManager->getChunkUploaders();
   for (QListIterator<UM::IChunkUploader*> i(chunkUploaders); i.hasNext();)
   {
      UM::IChunkUploader* chunkUploader = i.next();
      Protos::GUI::State_Upload* protoUpload = state.add_upload();
      if (chunkUploader->getChunk()->populateEntry(protoUpload->mutable_file()))
      {
         protoUpload->mutable_file()->mutable_chunk()->Clear();
         protoUpload->set_id(chunkUploader->getID());
         protoUpload->set_current_part(chunkUploader->getChunk()->getNum() + 1); // "+ 1" to begin at 1 and not 0.
         protoUpload->set_nb_part(chunkUploader->getChunk()->getNbTotalChunk());
         protoUpload->set_progress(chunkUploader->getProgress());
         protoUpload->mutable_peer_id()->set_hash(chunkUploader->getPeerID().getData(), Common::Hash::HASH_SIZE);
      }
      else
         state.mutable_upload()->RemoveLast();
   }

   // Shared Dirs.
   for (QListIterator<Common::SharedDir> i(this->fileManager->getSharedDirs()); i.hasNext();)
   {
      Common::SharedDir sharedDir = i.next();
      Protos::GUI::State::SharedDir* sharedDirProto = state.add_shared_directory();
      Common::ProtoHelper::setStr(*sharedDirProto, &Protos::GUI::State::SharedDir::set_path, sharedDir.path);
      sharedDirProto->set_size(sharedDir.size);
      sharedDirProto->set_free_space(sharedDir.freeSpace);
      sharedDirProto->mutable_id()->set_hash(sharedDir.ID.getData(), Common::Hash::HASH_SIZE);
   }

   // Stats.
   Protos::GUI::State_Stats* stats = state.mutable_stats();
   stats->set_cache_status(static_cast<Protos::GUI::State::Stats::CacheStatus>(this->fileManager->getCacheStatus())); // Warning: IFileManager::CacheStatus and Protos::GUI::State_Stats_CacheStatus must be compatible.
   stats->set_progress(this->fileManager->getProgress());
   stats->set_download_rate(downloadRate);
   stats->set_upload_rate(uploadRate);

   // Network interfaces.
   const QString& adressToListenStr = SETTINGS.get<QString>("listen_address");
   const QHostAddress adressToListen(adressToListenStr);
   if (adressToListenStr.isEmpty())
      state.set_listenany(static_cast<Protos::Common::Interface::Address::Protocol>(SETTINGS.get<quint32>("listen_any")));
   for (QListIterator<QNetworkInterface> i(this->interfaces); i.hasNext();)
   {
      const QNetworkInterface& interface = i.next();
      if (
         interface.flags().testFlag(QNetworkInterface::CanMulticast) &&
         !interface.flags().testFlag(QNetworkInterface::IsLoopBack) &&
         interface.isValid()
      )
      {
         const QList<QNetworkAddressEntry>& addresses = interface.addressEntries();
         if (!addresses.isEmpty())
         {
            Protos::Common::Interface* interfaceMess = state.add_interface();
            interfaceMess->set_id(interface.index() == 0 ? Common::StringUtils::hashStringToInt(interface.name()) : interface.index());
            Common::ProtoHelper::setStr(*interfaceMess, &Protos::Common::Interface::set_name, interface.humanReadableName());
            interfaceMess->set_isup(interface.flags().testFlag(QNetworkInterface::IsUp) && interface.flags().testFlag(QNetworkInterface::IsRunning));
            for (QListIterator<QNetworkAddressEntry> j(addresses); j.hasNext();)
            {
               QHostAddress address = j.next().ip();
               Protos::Common::Interface::Address* addressMess = interfaceMess->add_address();
               Common::ProtoHelper::setStr(*addressMess, &Protos::Common::Interface::Address::set_address, address.toString());
               addressMess->set_protocol(address.protocol() == QAbstractSocket::IPv6Protocol ? Protos::Common::Interface::Address::IPv6 : Protos::Common::Interface::Address::IPv4);
               addressMess->set_listened(address == adressToListen);
            }
         }
      }
   }

   // Chat rooms.
   for (QListIterator<CS::IChatSystem::ChatRoom> i(this->chatSystem->getRooms()); i.hasNext();)
   {
      const CS::IChatSystem::ChatRoom& room = i.next();
      Protos::GUI::State::Room* roomMess = state.add_rooms();

      Common::ProtoHelper::setStr(*roomMess, &Protos::GUI::State::Room::set_name, room.name);
      for (QSetIterator<PM::IPeer*> j(room.peers); j.hasNext();)
         roomMess->add_peer_id()->set_hash(j.next()->getID().getData(), Common::Hash::HASH_SIZE);
      roomMess->set_joined(room.joined);
   }

   this->waitForStateResult = true;
   this->send(Common::MessageHeader::GUI_STATE, state);
}

void RemoteConnection::closeSocket()
{
   this->close();
}

void RemoteConnection::newChatMessages(const Protos::Common::ChatMessages& messages)
{
   this->send(Common::MessageHeader::GUI_EVENT_CHAT_MESSAGES, messages);
}

void RemoteConnection::searchFound(const Protos::Common::FindResult& result)
{
   this->send(Common::MessageHeader::GUI_SEARCH_RESULT, result);
}

void RemoteConnection::getEntriesResult(const Protos::Core::GetEntriesResult& entries)
{
   PM::IGetEntriesResult* getEntriesResult = static_cast<PM::IGetEntriesResult*>(this->sender());

   Protos::GUI::BrowseResult result;
   for (int i = 0; i < entries.result_size(); i++)
   {
      Protos::Common::Entries* entriesResult = result.add_entries();
      if (entries.result(i).has_entries())
         entriesResult->CopyFrom(entries.result(i).entries());
   }

   result.set_tag(getEntriesResult->property("tag").toULongLong());
   this->send(Common::MessageHeader::GUI_BROWSE_RESULT, result);

   this->removeGetEntriesResult(getEntriesResult);
}

void RemoteConnection::getEntriesTimeout()
{
   PM::IGetEntriesResult* getEntriesResult = static_cast<PM::IGetEntriesResult*>(this->sender());
   this->removeGetEntriesResult(getEntriesResult);
}

void RemoteConnection::newLogEntry(QSharedPointer<LM::IEntry> entry)
{
   Protos::GUI::EventLogMessages::EventLogMessage* eventLogMessage = this->eventLogMessages.add_message();
   eventLogMessage->set_time(entry->getDate().currentMSecsSinceEpoch());
   Common::ProtoHelper::setStr(*eventLogMessage, &Protos::GUI::EventLogMessages::EventLogMessage::set_message, entry->getMessage());
   eventLogMessage->set_severity(static_cast<Protos::GUI::EventLogMessages::EventLogMessage::Severity>(entry->getSeverity()));

   if (!this->sendLogMessagesTimer.isActive())
      this->sendLogMessagesTimer.start();
}

void RemoteConnection::sendLogMessages()
{
   this->send(Common::MessageHeader::GUI_EVENT_LOG_MESSAGES, this->eventLogMessages);
   this->eventLogMessages.Clear();
}

void RemoteConnection::sendNoPasswordDefinedResult()
{
   Protos::GUI::AuthenticationResult authResultMessage;
   authResultMessage.set_status(Protos::GUI::AuthenticationResult::AUTH_PASSWORD_NOT_DEFINED);
   this->send(Common::MessageHeader::GUI_AUTHENTICATION_RESULT, authResultMessage);
   this->socket->close();
}

void RemoteConnection::sendBadPasswordResult()
{
   Protos::GUI::AuthenticationResult authResultMessage;
   authResultMessage.set_status(Protos::GUI::AuthenticationResult::AUTH_BAD_PASSWORD);
   this->send(Common::MessageHeader::GUI_AUTHENTICATION_RESULT, authResultMessage);
   this->socket->close();
}

void RemoteConnection::askForAuthentication()
{
   Protos::GUI::AskForAuthentication askForAuthenticationMessage;
   askForAuthenticationMessage.set_salt(SETTINGS.get<quint64>("salt"));

   this->saltChallenge = this->mtrand.randInt64();
   askForAuthenticationMessage.set_salt_challenge(this->saltChallenge);

   this->timerCloseSocket.start();
   this->send(Common::MessageHeader::GUI_ASK_FOR_AUTHENTICATION, askForAuthenticationMessage);
}

void RemoteConnection::removeGetEntriesResult(const PM::IGetEntriesResult* getEntriesResult)
{
   for (QMutableListIterator<QSharedPointer<PM::IGetEntriesResult>> i(this->getEntriesResults); i.hasNext();)
      if (i.next().data() == getEntriesResult)
         i.remove();
}

/**
  * We send all the last received messages to the GUI (history).
  */
void RemoteConnection::sendLastChatMessages()
{
   {
      Protos::Common::ChatMessages chatMessages;
      this->chatSystem->getLastChatMessages(chatMessages);
      this->send(Common::MessageHeader::GUI_EVENT_CHAT_MESSAGES, chatMessages);
   }

   foreach (CS::IChatSystem::ChatRoom room, this->chatSystem->getRooms())
      if (room.joined)
      {
         Protos::Common::ChatMessages chatMessages;
         this->chatSystem->getLastChatMessages(chatMessages,  std::numeric_limits<int>::max(), room.name);
         this->send(Common::MessageHeader::GUI_EVENT_CHAT_MESSAGES, chatMessages);
      }
}

void RemoteConnection::refreshAllInterfaces()
{
   this->interfaces = QNetworkInterface::allInterfaces();
}

void RemoteConnection::onNewMessage(const Common::Message& message)
{
   if (!this->authenticated && message.getHeader().getType() != Common::MessageHeader::GUI_AUTHENTICATION)
      return;

   switch (message.getHeader().getType())
   {
   case Common::MessageHeader::GUI_STATE_RESULT:
      this->waitForStateResult = false;
      this->timerRefresh.start();
      break;

   case Common::MessageHeader::GUI_AUTHENTICATION:
      {
         const Protos::GUI::Authentication& authenticationMessage = message.getMessage<Protos::GUI::Authentication>();

         this->timerCloseSocket.stop();

         if (!this->isLocal())
         {
            Common::Hash passwordReceived(authenticationMessage.password_challenge().hash());
            Common::Hash currentPassword = SETTINGS.get<Common::Hash>("remote_password");

            if (currentPassword.isNull())
            {
               QTimer::singleShot(SETTINGS.get<quint32>("delay_gui_connection_fail"), this, SLOT(sendNoPasswordDefinedResult()));
               break;
            }
            else if (passwordReceived != Common::Hasher::hashWithSalt(currentPassword, this->saltChallenge))
            {
               QTimer::singleShot(SETTINGS.get<quint32>("delay_gui_connection_fail"), this, SLOT(sendBadPasswordResult()));
               break;
            }
         }

         Protos::GUI::AuthenticationResult authResultMessage;
         authResultMessage.set_status(Protos::GUI::AuthenticationResult::AUTH_OK);
         this->send(Common::MessageHeader::GUI_AUTHENTICATION_RESULT, authResultMessage);
         this->authenticated = true;
         this->refresh();
         this->sendLastChatMessages();
      }
      break;

   case Common::MessageHeader::GUI_LANGUAGE:
      {
         const Protos::GUI::Language& langMessage = message.getMessage<Protos::GUI::Language>();
         emit languageDefined(Common::ProtoHelper::getLang(langMessage.language()));
      }
      break;

   case Common::MessageHeader::GUI_CHANGE_PASSWORD:
      {
         const Protos::GUI::ChangePassword& passMessage = message.getMessage<Protos::GUI::ChangePassword>();

         Common::Hash newPassword(passMessage.new_password().hash());
         Common::Hash currentPassword = SETTINGS.get<Common::Hash>("remote_password");

         if (newPassword.isNull()) // If the new password is null, the password is reset.
         {
            SETTINGS.set("remote_password", Common::Hash());
            SETTINGS.rm("salt");
            SETTINGS.save();
            this->refresh();
         }
         else if (currentPassword.isNull() || currentPassword == Common::Hash(passMessage.old_password().hash()))
         {
            SETTINGS.set("remote_password", newPassword);
            SETTINGS.set("salt", static_cast<quint64>(passMessage.new_salt()));
            SETTINGS.save();
            this->refresh();
         }
      }
      break;

   case Common::MessageHeader::GUI_SETTINGS:
      {
         const Protos::GUI::CoreSettings& coreSettingsMessage = message.getMessage<Protos::GUI::CoreSettings>();

         if (coreSettingsMessage.has_nick())
            this->peerManager->setNick(Common::ProtoHelper::getStr(coreSettingsMessage, &Protos::GUI::CoreSettings::nick));

         if (coreSettingsMessage.has_enable_integrity_check())
            SETTINGS.set("check_received_data_integrity", coreSettingsMessage.enable_integrity_check());

         try
         {
            QStringList sharedDirs;
            for (int i = 0; i < coreSettingsMessage.shared_directories().dir_size(); i++)
               sharedDirs << Common::ProtoHelper::getRepeatedStr(coreSettingsMessage.shared_directories(), &Protos::GUI::CoreSettings::SharedDirectories::dir, i);
            this->fileManager->setSharedDirs(sharedDirs);
         }
         catch(FM::DirsNotFoundException& e)
         {
            foreach (QString path, e.paths)
               L_WARN(QString("Directory not found : %1").arg(path));
         }

         QString currentAddressToListenTo = SETTINGS.get<QString>("listen_address");
         Protos::Common::Interface::Address::Protocol currentProtocol = static_cast<Protos::Common::Interface::Address::Protocol>(SETTINGS.get<quint32>("listen_any"));
         const QString& newAddressToListenTo = Common::ProtoHelper::getStr(coreSettingsMessage, &Protos::GUI::CoreSettings::listen_address);
         Protos::Common::Interface::Address::Protocol newProtocol = coreSettingsMessage.listen_any();
         SETTINGS.set("listen_address", newAddressToListenTo);
         SETTINGS.set("listen_any", static_cast<quint32>(newProtocol));
         if (currentAddressToListenTo != newAddressToListenTo || currentProtocol != newProtocol)
            this->networkListener->rebindSockets();

         SETTINGS.save();
         this->refresh();
      }
      break;

   case Common::MessageHeader::GUI_SEARCH:
      {
         // Remove old searches.
         for (QMutableListIterator<QSharedPointer<NL::ISearch>> i(this->currentSearches); i.hasNext();)
            if (i.next()->elapsed() > SETTINGS.get<quint32>("search_lifetime"))
               i.remove();

         const Protos::GUI::Search& searchMessage = message.getMessage<Protos::GUI::Search>();
         const Protos::Common::FindPattern& findPattern = searchMessage.pattern();

         // Special syntax to search in your own files.
         if (searchMessage.local())
         {
            QList<QString> extensions;
            extensions.reserve(findPattern.extension_filter_size());
            for (int i = 0; i < findPattern.extension_filter_size(); ++i)
               extensions << Common::ProtoHelper::getRepeatedStr(findPattern, &Protos::Common::FindPattern::extension_filter, i);

            const QList<Protos::Common::FindResult>& results = this->fileManager->find(
               Common::ProtoHelper::getStr(findPattern, &Protos::Common::FindPattern::pattern),
               extensions,
               findPattern.min_size() == 0 ? std::numeric_limits<qint64>::min() : (qint64)findPattern.min_size(),
               findPattern.max_size() == 0 ? std::numeric_limits<qint64>::max() : (qint64)findPattern.max_size(),
               SETTINGS.get<quint32>("max_number_of_result_shown"),
               std::numeric_limits<int>::max()
            );

            const quint64 tag = this->mtrand.randInt64();
            Protos::GUI::Tag tagMess;
            tagMess.set_tag(tag);
            this->send(Common::MessageHeader::GUI_SEARCH_TAG, tagMess);

            if (!results.isEmpty())
            {
               Protos::Common::FindResult result = results.first();
               result.mutable_peer_id()->set_hash(this->peerManager->getSelf()->getID().getData(), Common::Hash::HASH_SIZE);
               result.set_tag(tag);
               this->searchFound(result);
            }
         }
         else
         {
            QSharedPointer<NL::ISearch> search = this->networkListener->newSearch();
            connect(search.data(), SIGNAL(found(const Protos::Common::FindResult&)), this, SLOT(searchFound(const Protos::Common::FindResult&)));
            this->currentSearches << search;
            const quint64 tag = search->search(findPattern);

            Protos::GUI::Tag tagMess;
            tagMess.set_tag(tag);
            this->send(Common::MessageHeader::GUI_SEARCH_TAG, tagMess);
         }
      }
      break;

   case Common::MessageHeader::GUI_BROWSE:
      {
         const Protos::GUI::Browse& browseMessage = message.getMessage<Protos::GUI::Browse>();

         Common::Hash peerID(browseMessage.peer_id().hash());
         PM::IPeer* peer = this->peerManager->getPeer(peerID);

         quint64 tag = this->mtrand.randInt64();
         Protos::GUI::Tag tagMess;
         tagMess.set_tag(tag);
         this->send(Common::MessageHeader::GUI_BROWSE_TAG, tagMess);

         if (peer && peer != this->peerManager->getSelf())
         {
            Protos::Core::GetEntries getEntries;
            getEntries.mutable_dirs()->CopyFrom(browseMessage.dirs());
            getEntries.set_get_roots(browseMessage.get_roots());
            QSharedPointer<PM::IGetEntriesResult> entries = peer->getEntries(getEntries);
            if (entries.isNull())
            {
               Protos::GUI::BrowseResult result;
               result.set_tag(tag);
               this->send(Common::MessageHeader::GUI_BROWSE_RESULT, result);
               break;
            }

            entries->setProperty("tag", tag);
            connect(entries.data(), SIGNAL(result(const Protos::Core::GetEntriesResult&)), this, SLOT(getEntriesResult(const Protos::Core::GetEntriesResult&)));
            connect(entries.data(), SIGNAL(timeout()), this, SLOT(getEntriesTimeout()));
            entries->start();
            this->getEntriesResults << entries;
         }
         else
         {
            Protos::GUI::BrowseResult result;

            // If we want to browse our files.
            if (peerID == this->peerManager->getSelf()->getID())
            {
               for (int i = 0; i < browseMessage.dirs().entry_size(); i++)
                  result.add_entries()->CopyFrom(this->fileManager->getEntries(browseMessage.dirs().entry(i)));

               // Add the root directories if asked. Populate shared dirs with their base path.
               if (browseMessage.dirs().entry_size() == 0 || browseMessage.get_roots())
                  result.add_entries()->CopyFrom(this->fileManager->getEntries());
            }

            result.set_tag(tag);
            this->send(Common::MessageHeader::GUI_BROWSE_RESULT, result);
         }
      }
      break;

   case Common::MessageHeader::GUI_CANCEL_DOWNLOADS:
      {
         const Protos::GUI::CancelDownloads& cancelDownloadsMessage = message.getMessage<Protos::GUI::CancelDownloads>();

         if (cancelDownloadsMessage.complete())
            this->downloadManager->removeAllCompleteDownloads();

         QList<quint64> IDs;
         for (int i = 0; i < cancelDownloadsMessage.id_size(); i++)
            IDs << cancelDownloadsMessage.id(i);

         this->downloadManager->removeDownloads(IDs);

         this->refresh();
      }
      break;

   case Common::MessageHeader::GUI_PAUSE_DOWNLOADS:
      {
         const Protos::GUI::PauseDownloads& pauseDownloadsMessage = message.getMessage<Protos::GUI::PauseDownloads>();

         QList<quint64> IDs;
         for (int i = 0; i < pauseDownloadsMessage.id_size(); i++)
            IDs << pauseDownloadsMessage.id(i);

         this->downloadManager->pauseDownloads(IDs, pauseDownloadsMessage.pause());

         this->refresh();
      }
      break;

   case Common::MessageHeader::GUI_MOVE_DOWNLOADS:
      {
         const Protos::GUI::MoveDownloads& moveDownloadsMessage = message.getMessage<Protos::GUI::MoveDownloads>();

         QList<quint64> downloadIDRefs;
         for (int i = 0; i < moveDownloadsMessage.id_ref_size(); i++)
            downloadIDRefs << moveDownloadsMessage.id_ref(i);

         QList<quint64> downloadIDs;
         for (int i = 0; i < moveDownloadsMessage.id_to_move_size(); i++)
            downloadIDs << moveDownloadsMessage.id_to_move(i);

         this->downloadManager->moveDownloads(downloadIDRefs, downloadIDs, moveDownloadsMessage.position());

         this->refresh();
      }
      break;

   case Common::MessageHeader::GUI_DOWNLOAD:
      {
         const Protos::GUI::Download& downloadMessage = message.getMessage<Protos::GUI::Download>();

         PM::IPeer* peer = this->peerManager->getPeer(downloadMessage.peer_id().hash());

         if (peer)
         {
            if (downloadMessage.has_destination_directory_id())
               this->downloadManager->addDownload(downloadMessage.entry(), peer, downloadMessage.destination_directory_id().hash(), Common::ProtoHelper::getStr(downloadMessage, &Protos::GUI::Download::destination_path));
            else if (downloadMessage.has_destination_path())
               this->downloadManager->addDownload(downloadMessage.entry(), peer, Common::ProtoHelper::getStr(downloadMessage, &Protos::GUI::Download::destination_path));
            else
               this->downloadManager->addDownload(downloadMessage.entry(), peer);
         }

         this->refresh();
      }
      break;

   case Common::MessageHeader::GUI_CHAT_MESSAGE:
      {
         const Protos::GUI::ChatMessage& chatMessage = message.getMessage<Protos::GUI::ChatMessage>();

         CS::IChatSystem::SendStatus status =
            chatMessage.has_room() ?
                 this->chatSystem->send(Common::ProtoHelper::getStr(chatMessage, &Protos::GUI::ChatMessage::message), Common::ProtoHelper::getStr(chatMessage, &Protos::GUI::ChatMessage::room))
               : this->chatSystem->send(Common::ProtoHelper::getStr(chatMessage, &Protos::GUI::ChatMessage::message));

         Protos::GUI::ChatMessageResult result;
         result.set_status(status == CS::IChatSystem::OK ? Protos::GUI::ChatMessageResult::OK : (status == CS::IChatSystem::MESSAGE_TOO_LARGE ? Protos::GUI::ChatMessageResult::MESSAGE_TOO_LARGE : Protos::GUI::ChatMessageResult::ERROR_UNKNOWN));
         this->send(Common::MessageHeader::GUI_CHAT_MESSAGE_RESULT, result);
      }
      break;

   case Common::MessageHeader::GUI_JOIN_ROOM:
      {
         const Protos::GUI::JoinRoom joinRoomMessage = message.getMessage<Protos::GUI::JoinRoom>();

         this->chatSystem->joinRoom(Common::ProtoHelper::getStr(joinRoomMessage, &Protos::GUI::JoinRoom::name));
      }
      break;

   case Common::MessageHeader::GUI_LEAVE_ROOM:
      {
         const Protos::GUI::LeaveRoom leaveRoomMessage = message.getMessage<Protos::GUI::LeaveRoom>();

         this->chatSystem->leaveRoom(Common::ProtoHelper::getStr(leaveRoomMessage, &Protos::GUI::LeaveRoom::name));
      }
      break;

   case Common::MessageHeader::GUI_REFRESH:
      this->refresh();
      break;

   case Common::MessageHeader::GUI_REFRESH_NETWORK_INTERFACES:
      this->refreshAllInterfaces();
      this->refresh();
      break;

   default:;
   }
}

void RemoteConnection::onDisconnected()
{
   delete this;
}
