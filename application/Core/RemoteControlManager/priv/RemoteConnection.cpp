#include <priv/RemoteConnection.h>
using namespace RCM;

#include <QSet>
#include <QCoreApplication>

#include <Protos/gui_protocol.pb.h>

#include <Common/ZeroCopyStreamQIODevice.h>
#include <Common/Settings.h>
#include <Common/ProtoHelper.h>
#include <Common/Hash.h>
#include <Core/FileManager/IChunk.h>
#include <Core/FileManager/Exceptions.h>
#include <Core/PeerManager/IPeer.h>
#include <Core/NetworkListener/IChat.h>
#include <Core/DownloadManager/IDownload.h>
#include <Core/UploadManager/IUpload.h>

#include <priv/Log.h>

RemoteConnection::RemoteConnection(
   QSharedPointer<FM::IFileManager> fileManager,
   QSharedPointer<PM::IPeerManager> peerManager,
   QSharedPointer<UM::IUploadManager> uploadManager,
   QSharedPointer<DM::IDownloadManager> downloadManager,
   QSharedPointer<NL::INetworkListener> networkListener,
   QTcpSocket* socket
) :
   fileManager(fileManager),
   peerManager(peerManager),
   uploadManager(uploadManager),
   downloadManager(downloadManager),
   networkListener(networkListener),
   socket(socket),
   loggerRefreshState(LM::Builder::newLogger("RemoteConnection (State)"))
{
   L_DEBU(QString("New RemoteConnection from %1").arg(socket->peerAddress().toString()));

   this->timerRefresh.setInterval(SETTINGS.get<quint32>("remote_refresh_rate"));
   connect(&this->timerRefresh, SIGNAL(timeout()), this, SLOT(refresh()));
   this->timerRefresh.start();
   this->refresh();

   connect(this->socket, SIGNAL(readyRead()), this, SLOT(dataReceived()));
   connect(this->socket, SIGNAL(disconnected()), this, SLOT(disconnected()));

   if (!this->socket->isValid())
      this->disconnect();
   else
      this->dataReceived(); // The case where some data arrived before the 'connect' above.

   connect(&this->networkListener->getChat(), SIGNAL(newMessage(const Common::Hash&, const Protos::Core::ChatMessage&)), this, SLOT(newChatMessage(const Common::Hash&, const Protos::Core::ChatMessage&)));

   this->loggerHook = LM::Builder::newLoggerHook(LM::Severity(LM::SV_FATAL_ERROR | LM::SV_ERROR | LM::SV_END_USER | LM::SV_WARNING));

   qRegisterMetaType< QSharedPointer<const LM::IEntry> >("QSharedPointer<const LM::IEntry>");
   connect(this->loggerHook.data(), SIGNAL(newLogEntry(QSharedPointer<const LM::IEntry>)), this, SLOT(newLogEntry(QSharedPointer<const LM::IEntry>)), Qt::QueuedConnection);
}

RemoteConnection::~RemoteConnection()
{
   L_DEBU(QString("RemoteConnection deleted, %1").arg(socket->peerAddress().toString()));
   emit deleted(this);
   delete this->socket;
}

void RemoteConnection::refresh()
{
   Protos::GUI::State state;

   // Peers.
   QList<PM::IPeer*> peers = this->peerManager->getPeers();
   for (QListIterator<PM::IPeer*> i(peers); i.hasNext();)
   {
      PM::IPeer* peer = i.next();
      Protos::GUI::Peer* protoPeer = state.add_peer();
      protoPeer->mutable_peer_id()->set_hash(peer->getID().getData(), Common::Hash::HASH_SIZE);
      Common::ProtoHelper::setStr(*protoPeer, &Protos::GUI::Peer::set_nick, peer->getNick());
      protoPeer->set_sharing_amount(peer->getSharingAmount());
   }

   // Downloads.
   QList<DM::IDownload*> downloads = this->downloadManager->getDownloads();
   for (QListIterator<DM::IDownload*> i(downloads); i.hasNext();)
   {
      DM::IDownload* download = i.next();
      Protos::GUI::State_Download* protoDownload = state.add_download();
      protoDownload->set_id(download->getID());
      protoDownload->mutable_entry()->CopyFrom(download->getEntry());
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
      upload->getChunk()->populateEntry(protoUpload->mutable_file());
      protoUpload->set_id(upload->getID());
      protoUpload->set_current_part(upload->getChunk()->getNum() + 1); // "+ 1" to begin ar 1 and not 0.
      protoUpload->set_nb_part(upload->getChunk()->getNbTotalChunk());
      protoUpload->set_progress(upload->getProgress());
      protoUpload->mutable_peer_id()->set_hash(upload->getPeerID().getData(), Common::Hash::HASH_SIZE);
   }

   // Core settings.
   Protos::GUI::CoreSettings* coreSettings = state.mutable_settings();
   coreSettings->mutable_myself()->mutable_peer_id()->set_hash(this->peerManager->getID().getData(), Common::Hash::HASH_SIZE);
   Common::ProtoHelper::setStr(*coreSettings->mutable_myself(), &Protos::GUI::Peer::set_nick, this->peerManager->getNick());
   coreSettings->mutable_myself()->set_sharing_amount(this->fileManager->getAmount());
   for (QStringListIterator i(this->fileManager->getSharedDirsReadOnly()); i.hasNext();)
      Common::ProtoHelper::setStr(*coreSettings, &Protos::GUI::CoreSettings::add_shared_directory, i.next());
   for (QStringListIterator i(this->fileManager->getSharedDirsReadWrite()); i.hasNext();)
      Common::ProtoHelper::setStr(*coreSettings, &Protos::GUI::CoreSettings::add_destination_directory, i.next());

   // Stats.
   Protos::GUI::State_Stats* stats = state.mutable_stats();
   stats->set_cache_status(Protos::GUI::State_Stats_CacheStatus_UP_TO_DATE); // TODO : not implemented.
   stats->set_progress(100); // TODO : not implemented.
   stats->set_download_rate(this->downloadManager->getDownloadRate());
   stats->set_upload_rate(this->uploadManager->getUploadRate());

   this->send(Common::Network::GUI_STATE, state);
}

void RemoteConnection::dataReceived()
{
   while (!this->socket->atEnd())
   {
      QCoreApplication::processEvents(); // To read from the native socket to the internal QTcpSocket buffer. TODO : more elegant way?
      if (this->currentHeader.isNull() && this->socket->bytesAvailable() >= Common::Network::HEADER_SIZE)
      {
         this->currentHeader = Common::Network::readHeader<Common::Network::GUIMessageType>(*this->socket);

         L_DEBU(QString("RemoteConnection::dataReceived from %1, %2")
            .arg(this->socket->peerAddress().toString())
            .arg(this->currentHeader.toStr())
         );
      }

      if (!this->currentHeader.isNull() && this->socket->bytesAvailable() >= this->currentHeader.size)
      {
         this->readMessage();
         this->currentHeader.setNull();
      }
   }
}

void RemoteConnection::disconnected()
{
   L_DEBU("Connection dropped");

   this->deleteLater();
}

void RemoteConnection::newChatMessage(const Common::Hash& peerID, const Protos::Core::ChatMessage& message)
{
   Protos::GUI::EventChatMessage eventChatMessage;
   eventChatMessage.mutable_peer_id()->set_hash(peerID.getData(), Common::Hash::HASH_SIZE);
   eventChatMessage.set_message(message.message());

   this->send(Common::Network::GUI_EVENT_CHAT_MESSAGE, eventChatMessage);
}

void RemoteConnection::searchFound(const Protos::Common::FindResult& result)
{
   this->send(Common::Network::GUI_SEARCH_RESULT, result);
}

void RemoteConnection::getEntriesResult(const Protos::Common::Entries& entries)
{
   PM::IGetEntriesResult* getEntriesResult = dynamic_cast<PM::IGetEntriesResult*>(this->sender());

   Protos::GUI::BrowseResult result;
   result.mutable_entries()->CopyFrom(entries);
   result.set_tag(getEntriesResult->property("tag").toULongLong());
   this->send(Common::Network::GUI_BROWSE_RESULT, result);

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

   this->send(Common::Network::GUI_EVENT_LOG_MESSAGE, eventLogMessage);
}

bool RemoteConnection::readMessage()
{
   bool readOK = false;

   switch (this->currentHeader.type)
   {
   case Common::Network::GUI_SETTINGS:
      {
         Protos::GUI::CoreSettings coreSettingsMessage;
         {
            Common::ZeroCopyInputStreamQIODevice inputStream(this->socket);
            readOK = coreSettingsMessage.ParseFromBoundedZeroCopyStream(&inputStream, this->currentHeader.size);
         }

         if (readOK)
         {
            this->peerManager->setNick(Common::ProtoHelper::getStr(coreSettingsMessage.myself(), &Protos::GUI::Peer::nick));

            try
            {
               QStringList sharedDirsReadOnly;
               for (int i = 0; i < coreSettingsMessage.shared_directory_size(); i++)
                  sharedDirsReadOnly << Common::ProtoHelper::getRepeatedStr(coreSettingsMessage, &Protos::GUI::CoreSettings::shared_directory, i);
               this->fileManager->setSharedDirsReadOnly(sharedDirsReadOnly);

               QStringList sharedDirsReadWrite;
               for (int i = 0; i < coreSettingsMessage.destination_directory_size(); i++)
                  sharedDirsReadWrite << Common::ProtoHelper::getRepeatedStr(coreSettingsMessage, &Protos::GUI::CoreSettings::destination_directory, i);
               this->fileManager->setSharedDirsReadWrite(sharedDirsReadWrite);
            }
            catch(FM::SuperDirectoryExistsException& e)
            {
               L_WARN(QString("There is already a super directory : %1 for this directory : %2").arg(e.superDirectory).arg(e.subDirectory));
            }
         }
      }
      break;

   case Common::Network::GUI_SEARCH:
      {
         // Remove old searches.
         for (QMutableListIterator< QSharedPointer<NL::ISearch> > i(this->currentSearches); i.hasNext();)
            if (i.next()->elapsed() > SETTINGS.get<quint32>("search_lifetime"))
               i.remove();

         Protos::GUI::Search searchMessage;
         {
            Common::ZeroCopyInputStreamQIODevice inputStream(this->socket);
            readOK = searchMessage.ParseFromBoundedZeroCopyStream(&inputStream, this->currentHeader.size);
         }

         if (readOK)
         {
            QSharedPointer<NL::ISearch> search = this->networkListener->newSearch();
            connect(search.data(), SIGNAL(found(const Protos::Common::FindResult&)), this, SLOT(searchFound(const Protos::Common::FindResult&)));
            this->currentSearches << search;
            quint64 tag = search->search(Common::ProtoHelper::getStr(searchMessage, &Protos::GUI::Search::pattern));

            Protos::GUI::Tag tagMess;
            tagMess.set_tag(tag);
            this->send(Common::Network::GUI_SEARCH_TAG, tagMess);
         }
      }
      break;

   case Common::Network::GUI_BROWSE:
      {
         Protos::GUI::Browse browseMessage;
         {
            Common::ZeroCopyInputStreamQIODevice inputStream(this->socket);
            readOK = browseMessage.ParseFromBoundedZeroCopyStream(&inputStream, this->currentHeader.size);
         }

         if (readOK)
         {
            Common::Hash peerID(browseMessage.peer_id().hash().data());
            PM::IPeer* peer = this->peerManager->getPeer(peerID);

            quint64 tag = (static_cast<quint64>(this->mtrand.randInt()) << 32) | this->mtrand.randInt();
            Protos::GUI::Tag tagMess;
            tagMess.set_tag(tag);
            this->send(Common::Network::GUI_BROWSE_TAG, tagMess);

            if (peer)
            {
               Protos::Core::GetEntries getEntries;
               if (browseMessage.has_dir())
                  getEntries.mutable_dir()->CopyFrom(browseMessage.dir());
               QSharedPointer<PM::IGetEntriesResult> entries = peer->getEntries(getEntries);
               entries->setProperty("tag", tag);
               connect(entries.data(), SIGNAL(result(const Protos::Common::Entries&)), this, SLOT(getEntriesResult(const Protos::Common::Entries&)));
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
                  result.mutable_entries()->CopyFrom(browseMessage.has_dir() ? this->fileManager->getEntries(browseMessage.dir()) : this->fileManager->getEntries());
               }
               else
                  result.mutable_entries(); // To create an empty 'entries' field.

               result.set_tag(tag);
               this->send(Common::Network::GUI_BROWSE_RESULT, result);
            }
         }
      }
      break;

   case Common::Network::GUI_CANCEL_DOWNLOADS:
      {
         Protos::GUI::CancelDownloads cancelDownloadsMessage;
         {
            Common::ZeroCopyInputStreamQIODevice inputStream(this->socket);
            readOK = cancelDownloadsMessage.ParseFromBoundedZeroCopyStream(&inputStream, this->currentHeader.size);
         }

         if (readOK)
         {
            // To avoid O(n^2).
            QSet<quint64> IDs;
            for (int i = 0; i < cancelDownloadsMessage.id_size(); i++)
               IDs.insert(cancelDownloadsMessage.id(i));

            QList<DM::IDownload*> downloads = this->downloadManager->getDownloads();
            for (QListIterator<DM::IDownload*> i(downloads); i.hasNext();)
            {
               DM::IDownload* download = i.next();
               if (IDs.contains(download->getID()))
                  download->remove();
            }

            this->refresh();
            this->timerRefresh.start();
         }
      }
      break;

   case Common::Network::GUI_MOVE_DOWNLOADS:
      {
         Protos::GUI::MoveDownloads moveDownloadsMessage;
         {
            Common::ZeroCopyInputStreamQIODevice inputStream(this->socket);
            readOK = moveDownloadsMessage.ParseFromBoundedZeroCopyStream(&inputStream, this->currentHeader.size);
         }

         if (readOK)
         {
            QList<quint64> downloadIDs;
            for (int i = 0; i < moveDownloadsMessage.id_to_move_size(); i++)
               downloadIDs << moveDownloadsMessage.id_to_move(i);
            this->downloadManager->moveDownloads(moveDownloadsMessage.id_ref(), moveDownloadsMessage.move_before(), downloadIDs);

            this->refresh();
            this->timerRefresh.start();
         }
      }
      break;

   case Common::Network::GUI_DOWNLOAD:
      {
         Protos::GUI::Download downloadMessage;
         {
            Common::ZeroCopyInputStreamQIODevice inputStream(this->socket);
            readOK = downloadMessage.ParseFromBoundedZeroCopyStream(&inputStream, this->currentHeader.size);
         }

         if (readOK)
         {
            Common::Hash peerID(downloadMessage.peer_id().hash().data());
            this->downloadManager->addDownload(downloadMessage.entry(), peerID);

            this->refresh();
            this->timerRefresh.start();
         }
      }
      break;

   case Common::Network::GUI_CHAT_MESSAGE:
      {
         Protos::GUI::ChatMessage chatMessage;
         {
            Common::ZeroCopyInputStreamQIODevice inputStream(this->socket);
            readOK = chatMessage.ParseFromBoundedZeroCopyStream(&inputStream, this->currentHeader.size);
         }

         if (readOK)
            this->networkListener->getChat().send(Common::ProtoHelper::getStr(chatMessage, &Protos::GUI::ChatMessage::message));
      }
      break;

   case Common::Network::GUI_REFRESH:
      {
         this->refresh();
         this->timerRefresh.start();
      }
      break;

   default:
      readOK = false;
   }

   return readOK;
}

void RemoteConnection::send(Common::Network::GUIMessageType type, const google::protobuf::Message& message) const
{
   const Common::Network::MessageHeader<Common::Network::GUIMessageType> header(type, message.ByteSize(), this->peerManager->getID());

#if DEBUG
   QString logMess = QString("RemoteConnection::send : %2\n%3").arg(header.toStr()).arg(Common::ProtoHelper::getDebugStr(message));
   if (type == Common::Network::GUI_STATE)
      ; // LOG_DEBU(this->loggerRefreshState, logMess); // Commented -> too heavy...
   else
      L_DEBU(logMess);
#endif

   Common::Network::writeHeader(*this->socket, header);
   Common::ZeroCopyOutputStreamQIODevice outputStream(this->socket);
   if (!message.SerializeToZeroCopyStream(&outputStream))
      L_WARN(QString("Unable to send %1").arg(Common::ProtoHelper::getDebugStr(message)));
}

void RemoteConnection::removeGetEntriesResult(const PM::IGetEntriesResult* getEntriesResult)
{
   for (QMutableListIterator< QSharedPointer<PM::IGetEntriesResult> > i(this->getEntriesResults); i.hasNext();)
      if (i.next().data() == getEntriesResult)
         i.remove();
}


