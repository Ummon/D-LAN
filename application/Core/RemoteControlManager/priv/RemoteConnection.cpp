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

#include <Common/ZeroCopyStreamQIODevice.h>
#include <Common/Settings.h>
#include <Common/ProtoHelper.h>
#include <Common/Hash.h>
#include <Common/SharedDir.h>
#include <Core/FileManager/IChunk.h>
#include <Core/FileManager/Exceptions.h>
#include <Core/PeerManager/IPeer.h>
#include <Core/NetworkListener/IChat.h>
#include <Core/DownloadManager/IDownload.h>
#include <Core/UploadManager/IUpload.h>

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
   QTcpSocket* socket
) :
   MessageSocket(new RemoteConnection::Logger(), socket, peerManager->getID()),
   fileManager(fileManager),
   peerManager(peerManager),
   uploadManager(uploadManager),
   downloadManager(downloadManager),
   networkListener(networkListener),
   authenticated(false)
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

   this->timerRefresh.setInterval(SETTINGS.get<quint32>("remote_refresh_rate"));
   this->timerRefresh.setSingleShot(true);
   connect(&this->timerRefresh, SIGNAL(timeout()), this, SLOT(refresh()));
   this->refresh();

   connect(&this->networkListener->getChat(), SIGNAL(newMessage(const Protos::GUI::EventChatMessages_Message&)), this, SLOT(newChatMessage(const Protos::GUI::EventChatMessages_Message&)));
   // We send all the last received messages to the GUI (history).
   this->send(Common::MessageHeader::GUI_EVENT_CHAT_MESSAGES, this->networkListener->getChat().getLastMessages());

   this->loggerHook = LM::Builder::newLoggerHook(LM::Severity(LM::SV_FATAL_ERROR | LM::SV_ERROR | LM::SV_END_USER | LM::SV_WARNING));

   qRegisterMetaType< QSharedPointer<const LM::IEntry> >("QSharedPointer<const LM::IEntry>");
   connect(this->loggerHook.data(), SIGNAL(newLogEntry(QSharedPointer<const LM::IEntry>)), this, SLOT(newLogEntry(QSharedPointer<const LM::IEntry>)), Qt::QueuedConnection);
}

RemoteConnection::~RemoteConnection()
{
   L_DEBU(QString("RemoteConnection[%1] deleted").arg(this->num));
   emit deleted(this);
}

void RemoteConnection::send(Common::MessageHeader::MessageType type, const google::protobuf::Message& message)
{
   // When not authenticated we can only send messages of type 'GUI_AUTHENTICATION_RESULT'.
   if (!this->authenticated && type != Common::MessageHeader::GUI_AUTHENTICATION_RESULT)
      return;

   Common::MessageSocket::send(type, message);
}

/**
  * @see RemoteControlManager::chatMessageSent
  */
void RemoteConnection::sendMessageToItself(const QString& message)
{
   Protos::GUI::EventChatMessages eventChatMessages;

   Protos::GUI::EventChatMessages_Message* eventChatMessage = eventChatMessages.add_message();
   eventChatMessage->mutable_peer_id()->set_hash(this->peerManager->getID().getData(), Common::Hash::HASH_SIZE);
   eventChatMessage->set_time(QDateTime::currentMSecsSinceEpoch());
   Common::ProtoHelper::setStr(*eventChatMessage, &Protos::GUI::EventChatMessages_Message::set_message, message);

   this->send(Common::MessageHeader::GUI_EVENT_CHAT_MESSAGES, eventChatMessages);
}

void RemoteConnection::onNewMessage(Common::MessageHeader::MessageType type, const google::protobuf::Message& message)
{
   if (type != Common::MessageHeader::GUI_AUTHENTICATION && !this->authenticated)
      return;

   switch (type)
   {
   case Common::MessageHeader::GUI_AUTHENTICATION:
      {
         const Protos::GUI::Authentication& authenticationMessage = static_cast<const Protos::GUI::Authentication&>(message);

         if (this->isLocal())
            this->authenticated = true;
         else
         {
            Common::Hash passwordHashReceived(authenticationMessage.password().hash());
            Common::Hash actualPasswordHash = SETTINGS.get<Common::Hash>("remote_password");

            if (!actualPasswordHash.isNull() && passwordHashReceived == actualPasswordHash)
            {
               Protos::GUI::AuthenticationResult authResultMessage;
               authResultMessage.set_status(Protos::GUI::AuthenticationResult_Status_OK);
               this->send(Common::MessageHeader::GUI_AUTHENTICATION_RESULT, authResultMessage);
               this->authenticated = true;
            }
            else
            {
               QTimer::singleShot(1000, this, SLOT(sendBadPasswordResult()));
            }
         }
      }
      break;

   case Common::MessageHeader::GUI_SETTINGS:
      {
         const Protos::GUI::CoreSettings& coreSettingsMessage = static_cast<const Protos::GUI::CoreSettings&>(message);

         this->peerManager->setNick(Common::ProtoHelper::getStr(coreSettingsMessage, &Protos::GUI::CoreSettings::nick));
         SETTINGS.set("check_received_data_integrity", coreSettingsMessage.enable_integrity_check());

         try
         {
            QStringList sharedDirs;
            for (int i = 0; i < coreSettingsMessage.shared_directory_size(); i++)
               sharedDirs << Common::ProtoHelper::getRepeatedStr(coreSettingsMessage, &Protos::GUI::CoreSettings::shared_directory, i);
            this->fileManager->setSharedDirs(sharedDirs);
         }
         catch(FM::DirsNotFoundException& e)
         {
            foreach (QString path, e.paths)
               L_WARN(QString("Directory not found : %1").arg(path));
         }

         this->refresh();
      }
      break;

   case Common::MessageHeader::GUI_SEARCH:
      {
         // Remove old searches.
         for (QMutableListIterator< QSharedPointer<NL::ISearch> > i(this->currentSearches); i.hasNext();)
            if (i.next()->elapsed() > SETTINGS.get<quint32>("search_lifetime"))
               i.remove();

         const Protos::GUI::Search& searchMessage = static_cast<const Protos::GUI::Search&>(message);
         const QString pattern = Common::ProtoHelper::getStr(searchMessage, &Protos::GUI::Search::pattern);

         // Special syntax to search in your own files.
         if (pattern.startsWith('<'))
         {
            QList<Protos::Common::FindResult> results = this->fileManager->find(pattern, SETTINGS.get<quint32>("max_number_of_result_shown"), std::numeric_limits<int>::max());

            if (!results.isEmpty())
            {
               const quint64 tag = (static_cast<quint64>(this->mtrand.randInt()) << 32) | this->mtrand.randInt();
               Protos::GUI::Tag tagMess;
               tagMess.set_tag(tag);
               this->send(Common::MessageHeader::GUI_SEARCH_TAG, tagMess);

               Protos::Common::FindResult& result = results.first();
               result.mutable_peer_id()->set_hash(this->peerManager->getID().getData(), Common::Hash::HASH_SIZE);
               result.set_tag(tag);
               this->searchFound(result);
            }
         }
         else
         {
            QSharedPointer<NL::ISearch> search = this->networkListener->newSearch();
            connect(search.data(), SIGNAL(found(const Protos::Common::FindResult&)), this, SLOT(searchFound(const Protos::Common::FindResult&)));
            this->currentSearches << search;
            const quint64 tag = search->search(pattern);

            Protos::GUI::Tag tagMess;
            tagMess.set_tag(tag);
            this->send(Common::MessageHeader::GUI_SEARCH_TAG, tagMess);
         }
      }
      break;

   case Common::MessageHeader::GUI_BROWSE:
      {
         const Protos::GUI::Browse& browseMessage = static_cast<const Protos::GUI::Browse&>(message);

         Common::Hash peerID(browseMessage.peer_id().hash());
         PM::IPeer* peer = this->peerManager->getPeer(peerID);

         quint64 tag = (static_cast<quint64>(this->mtrand.randInt()) << 32) | this->mtrand.randInt();
         Protos::GUI::Tag tagMess;
         tagMess.set_tag(tag);
         this->send(Common::MessageHeader::GUI_BROWSE_TAG, tagMess);

         if (peer)
         {
            Protos::Core::GetEntries getEntries;
            getEntries.mutable_dirs()->CopyFrom(browseMessage.dirs());
            getEntries.set_get_roots(browseMessage.get_roots());
            QSharedPointer<PM::IGetEntriesResult> entries = peer->getEntries(getEntries);
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
            if (peerID == this->peerManager->getID())
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
         const Protos::GUI::CancelDownloads& cancelDownloadsMessage = static_cast<const Protos::GUI::CancelDownloads&>(message);

         if (cancelDownloadsMessage.complete())
            this->downloadManager->removeAllCompleteDownloads();

         QList<quint64> IDs;
         for (int i = 0; i < cancelDownloadsMessage.id_size(); i++)
            IDs << cancelDownloadsMessage.id(i);

         this->downloadManager->removeDownloads(IDs);

         this->refresh();
      }
      break;

   case Common::MessageHeader::GUI_MOVE_DOWNLOADS:
      {
         const Protos::GUI::MoveDownloads& moveDownloadsMessage = static_cast<const Protos::GUI::MoveDownloads&>(message);

         QList<quint64> downloadIDs;
         for (int i = 0; i < moveDownloadsMessage.id_to_move_size(); i++)
            downloadIDs << moveDownloadsMessage.id_to_move(i);
         this->downloadManager->moveDownloads(moveDownloadsMessage.id_ref(), moveDownloadsMessage.move_before(), downloadIDs);

         this->refresh();
      }
      break;

   case Common::MessageHeader::GUI_DOWNLOAD:
      {
         const Protos::GUI::Download& downloadMessage = static_cast<const Protos::GUI::Download&>(message);

         Common::Hash peerID(downloadMessage.peer_id().hash());
         if (downloadMessage.has_destination_directory_id())
            this->downloadManager->addDownload(downloadMessage.entry(), peerID, downloadMessage.destination_directory_id().hash(), Common::ProtoHelper::getStr(downloadMessage, &Protos::GUI::Download::destination_path));
         else if (downloadMessage.has_destination_path())
            this->downloadManager->addDownload(downloadMessage.entry(), peerID, Common::ProtoHelper::getStr(downloadMessage, &Protos::GUI::Download::destination_path));
         else
            this->downloadManager->addDownload(downloadMessage.entry(), peerID);

         this->refresh();
      }
      break;

   case Common::MessageHeader::GUI_CHAT_MESSAGE:
      {
         const Protos::GUI::ChatMessage& chatMessage = static_cast<const Protos::GUI::ChatMessage&>(message);

         const QString message = Common::ProtoHelper::getStr(chatMessage, &Protos::GUI::ChatMessage::message);
         emit chatMessageSent(message);
         this->networkListener->getChat().send(message);
      }
      break;

   case Common::MessageHeader::GUI_REFRESH:
      {
         this->refresh();
      }
      break;

   default:;
   }
}

void RemoteConnection::disconnected()
{
   delete this;
}

void RemoteConnection::refresh()
{
   Protos::GUI::State state;

   state.mutable_myself()->mutable_peer_id()->set_hash(this->peerManager->getID().getData(), Common::Hash::HASH_SIZE);
   Common::ProtoHelper::setStr(*state.mutable_myself(), &Protos::GUI::State_Peer::set_nick, this->peerManager->getNick());   
   state.set_integrity_check_enabled(SETTINGS.get<bool>("check_received_data_integrity"));
   state.mutable_myself()->set_sharing_amount(this->fileManager->getAmount());

   // Peers.
   QList<PM::IPeer*> peers = this->peerManager->getPeers();
   for (QListIterator<PM::IPeer*> i(peers); i.hasNext();)
   {
      PM::IPeer* peer = i.next();
      Protos::GUI::State_Peer* protoPeer = state.add_peer();
      protoPeer->mutable_peer_id()->set_hash(peer->getID().getData(), Common::Hash::HASH_SIZE);
      Common::ProtoHelper::setStr(*protoPeer, &Protos::GUI::State_Peer::set_nick, peer->getNick());
      protoPeer->set_sharing_amount(peer->getSharingAmount());
   }

   // Downloads.
   QList<DM::IDownload*> downloads = this->downloadManager->getDownloads();
   for (QListIterator<DM::IDownload*> i(downloads); i.hasNext();)
   {
      DM::IDownload* download = i.next();
      Protos::GUI::State_Download* protoDownload = state.add_download();
      protoDownload->set_id(download->getID());
      protoDownload->mutable_local_entry()->CopyFrom(download->getLocalEntry());
      protoDownload->mutable_local_entry()->mutable_chunk()->Clear(); // We don't need to send the hashes.
      protoDownload->set_status(static_cast<Protos::GUI::State_Download_Status>(download->getStatus())); // Warning, enums must be compatible.
      protoDownload->set_progress(download->getProgress());
      for (QSetIterator<Common::Hash> j(download->getPeers()); j.hasNext();)
         protoDownload->add_peer_id()->set_hash(j.next().getData(), Common::Hash::HASH_SIZE);
   }

   // Uploads.
   QList<UM::IUpload*> uploads = this->uploadManager->getUploads();
   for (QListIterator<UM::IUpload*> i(uploads); i.hasNext();)
   {
      UM::IUpload* upload = i.next();
      Protos::GUI::State_Upload* protoUpload = state.add_upload();
      if (upload->getChunk()->populateEntry(protoUpload->mutable_file()))
      {
         protoUpload->set_id(upload->getID());
         protoUpload->set_current_part(upload->getChunk()->getNum() + 1); // "+ 1" to begin at 1 and not 0.
         protoUpload->set_nb_part(upload->getChunk()->getNbTotalChunk());
         protoUpload->set_progress(upload->getProgress());
         protoUpload->mutable_peer_id()->set_hash(upload->getPeerID().getData(), Common::Hash::HASH_SIZE);
      }
      else
         state.mutable_upload()->RemoveLast();
   }

   // Shared Dirs.
   for (QListIterator<Common::SharedDir> i(this->fileManager->getSharedDirs()); i.hasNext();)
   {
      Common::SharedDir sharedDir = i.next();
      Protos::GUI::State_SharedDir* sharedDirProto = state.add_shared_directory();
      Common::ProtoHelper::setStr(*sharedDirProto, &Protos::GUI::State_SharedDir::set_path, sharedDir.path);
      sharedDirProto->set_size(sharedDir.size);
      sharedDirProto->set_free_space(sharedDir.freeSpace);
      sharedDirProto->mutable_id()->set_hash(sharedDir.ID.getData(), Common::Hash::HASH_SIZE);
   }

   // Stats.
   Protos::GUI::State_Stats* stats = state.mutable_stats();
   stats->set_cache_status(static_cast<Protos::GUI::State_Stats_CacheStatus>(this->fileManager->getCacheStatus())); // Warning: IFileManager::CacheStatus and Protos::GUI::State_Stats_CacheStatus must be compatible.
   stats->set_progress(this->fileManager->getProgress());
   stats->set_download_rate(this->downloadManager->getDownloadRate());
   stats->set_upload_rate(this->uploadManager->getUploadRate());

   this->send(Common::MessageHeader::GUI_STATE, state);

   this->timerRefresh.start();
}

void RemoteConnection::newChatMessage(const Protos::GUI::EventChatMessages_Message& message)
{
   Protos::GUI::EventChatMessages eventChatMessages;
   eventChatMessages.add_message()->CopyFrom(message);

   this->send(Common::MessageHeader::GUI_EVENT_CHAT_MESSAGES, eventChatMessages);
}

void RemoteConnection::searchFound(const Protos::Common::FindResult& result)
{
   this->send(Common::MessageHeader::GUI_SEARCH_RESULT, result);
}

void RemoteConnection::getEntriesResult(const Protos::Core::GetEntriesResult& entries)
{
   PM::IGetEntriesResult* getEntriesResult = dynamic_cast<PM::IGetEntriesResult*>(this->sender());

   Protos::GUI::BrowseResult result;
   result.mutable_entries()->MergeFrom(entries.entries());
   result.set_tag(getEntriesResult->property("tag").toULongLong());
   this->send(Common::MessageHeader::GUI_BROWSE_RESULT, result);

   this->removeGetEntriesResult(getEntriesResult);
}

void RemoteConnection::getEntriesTimeout()
{
   PM::IGetEntriesResult* getEntriesResult = dynamic_cast<PM::IGetEntriesResult*>(this->sender());
   this->removeGetEntriesResult(getEntriesResult);
}

void RemoteConnection::newLogEntry(QSharedPointer<const LM::IEntry> entry)
{
   Protos::GUI::EventLogMessage eventLogMessage;
   eventLogMessage.set_time(entry->getDate().currentMSecsSinceEpoch());
   Common::ProtoHelper::setStr(eventLogMessage, &Protos::GUI::EventLogMessage::set_message, entry->getMessage());
   eventLogMessage.set_severity(static_cast<Protos::GUI::EventLogMessage_Severity>(entry->getSeverity()));

   this->send(Common::MessageHeader::GUI_EVENT_LOG_MESSAGE, eventLogMessage);
}

void RemoteConnection::sendBadPasswordResult()
{
   Protos::GUI::AuthenticationResult authResultMessage;
   authResultMessage.set_status(Protos::GUI::AuthenticationResult_Status_BAD_PASSWORD);
   this->send(Common::MessageHeader::GUI_AUTHENTICATION_RESULT, authResultMessage);
   this->socket->close();
}

void RemoteConnection::removeGetEntriesResult(const PM::IGetEntriesResult* getEntriesResult)
{
   for (QMutableListIterator< QSharedPointer<PM::IGetEntriesResult> > i(this->getEntriesResults); i.hasNext();)
      if (i.next().data() == getEntriesResult)
         i.remove();
}
