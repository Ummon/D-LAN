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
  
#include <priv/CoreConnection.h>
using namespace RCC;

#include <QHostAddress>
#include <QCoreApplication>

#include <Common/ProtoHelper.h>
#include <Common/Constants.h>
#include <Common/Global.h>

#include <LogManager/Builder.h>

#include <priv/Log.h>
#include <priv/SendChatMessageResult.h>
#include <priv/BrowseResult.h>
#include <priv/SearchResult.h>

// The behavior under Windows and Linux are not the same when connecting a socket to a port.
// On Linux 'connectToHost(..)' will immediately fail if there is no service behind the port,
// on Windows there is a delay before 'stateChanged' is called with a 'UnconnectedState' type.
#ifdef Q_OS_WIN32
   const int InternalCoreConnection::NB_RETRIES_MAX(1);
   const int InternalCoreConnection::TIME_BETWEEN_RETRIES(100);
#else
   const int InternalCoreConnection::NB_RETRIES_MAX(8);
   const int InternalCoreConnection::TIME_BETWEEN_RETRIES(250);
#endif

void InternalCoreConnection::Logger::logDebug(const QString& message)
{
   L_DEBU(message);
}

void InternalCoreConnection::Logger::logError(const QString& message)
{
   L_WARN(message);
}

InternalCoreConnection::InternalCoreConnection(CoreController& coreController) :
   Common::MessageSocket(new InternalCoreConnection::Logger()),
   coreController(coreController),
   currentHostLookupID(-1),
   nbRetries(0),
   authenticated(false),
   forcedToClose(false),
   salt(0)
{
   this->startListening();
}

InternalCoreConnection::~InternalCoreConnection()
{
   if (this->currentHostLookupID != -1)
      QHostInfo::abortHostLookup(this->currentHostLookupID);

   this->addressesToTry.clear();
}

void InternalCoreConnection::connectToCore(const QString& address, quint16 port, Common::Hash password)
{
   this->connectionInfo.address = address;
   this->connectionInfo.port = port;
   this->connectionInfo.password = password;

   if (this->isConnected() && this->connectionInfo.address == address)
   {
      emit connectingError(ICoreConnection::RCC_ERROR_ALREADY_CONNECTED_TO_THIS_CORE);
      return;
   }

   if (this->currentHostLookupID != -1)
      QHostInfo::abortHostLookup(this->currentHostLookupID);

   this->currentHostLookupID = QHostInfo::lookupHost(this->connectionInfo.address, this, SLOT(adressResolved(QHostInfo)));
}

void InternalCoreConnection::connectToCore(const QString& address, quint16 port, const QString& password)
{
   this->password = password;
   this->connectToCore(address, port, Common::Hash());
}

bool InternalCoreConnection::isLocal() const
{
   if (this->socket->peerAddress().isNull())
      return Common::Global::isLocal(QHostAddress(this->connectionInfo.address));
   else
      return MessageSocket::isLocal();
}

bool InternalCoreConnection::isConnected() const
{
   return MessageSocket::isConnected() && this->authenticated;
}

void InternalCoreConnection::disconnectFromCore()
{
   this->forcedToClose = true;
   this->close();
   this->forcedToClose = false;
   this->addressesToTry.clear();
   this->connectionInfo.clear();
}

QSharedPointer<ISendChatMessageResult> InternalCoreConnection::sendChatMessage(int socketTimeout, const QString& message, const QString& roomName, const QList<Common::Hash>& peerIDsAnswered)
{
   QSharedPointer<SendChatMessageResult> sendChatMessageResult = QSharedPointer<SendChatMessageResult>(new SendChatMessageResult(this, socketTimeout, message, roomName, peerIDsAnswered));
   this->sendChatMessageResultWithoutReply << sendChatMessageResult.toWeakRef();
   return sendChatMessageResult;
}

void InternalCoreConnection::joinRoom(const QString& room)
{
   if (!room.isEmpty())
   {
      Protos::GUI::JoinRoom joinRoomMessage;
      Common::ProtoHelper::setStr(joinRoomMessage, &Protos::GUI::JoinRoom::set_name, room);
      this->send(Common::MessageHeader::GUI_JOIN_ROOM, joinRoomMessage);
   }
}

void InternalCoreConnection::leaveRoom(const QString& room)
{
   if (!room.isEmpty())
   {
      Protos::GUI::LeaveRoom leaveRoomMessage;
      Common::ProtoHelper::setStr(leaveRoomMessage, &Protos::GUI::LeaveRoom::set_name, room);
      this->send(Common::MessageHeader::GUI_LEAVE_ROOM, leaveRoomMessage);
   }
}

void InternalCoreConnection::setCoreSettings(const Protos::GUI::CoreSettings settings)
{
   this->send(Common::MessageHeader::GUI_SETTINGS, settings);
}

void InternalCoreConnection::setCoreLanguage(const QLocale& locale)
{
   this->currentLanguage = locale;
   this->sendCurrentLanguage();
}

bool InternalCoreConnection::setCorePassword(const QString& newPassword, const QString& oldPassword)
{
   Protos::GUI::ChangePassword passMess;

   const quint64 newSalt = mtrand.randInt64();
   Common::Hash newPasswordHashed = Common::Hasher::hashWithSalt(newPassword, newSalt);

   passMess.mutable_new_password()->set_hash(newPasswordHashed.getData(), Common::Hash::HASH_SIZE);
   passMess.set_new_salt(newSalt);

   if (!oldPassword.isNull())
   {
      Common::Hash oldPasswordHashed = Common::Hasher::hashWithSalt(oldPassword, this->salt);
      if (!this->connectionInfo.password.isNull() && this->connectionInfo.password != oldPasswordHashed)
         return false;

      passMess.mutable_old_password()->set_hash(oldPasswordHashed.getData(), Common::Hash::HASH_SIZE);
   }

   this->connectionInfo.password = newPasswordHashed;
   this->salt = newSalt;

   this->send(Common::MessageHeader::GUI_CHANGE_PASSWORD, passMess);
   return true;
}

void InternalCoreConnection::resetCorePassword()
{
   Protos::GUI::ChangePassword passMess;
   passMess.mutable_new_password()->set_hash(Common::Hash().getData(), Common::Hash::HASH_SIZE);
   passMess.set_new_salt(0);
   this->send(Common::MessageHeader::GUI_CHANGE_PASSWORD, passMess);
}

QSharedPointer<IBrowseResult> InternalCoreConnection::browse(const Common::Hash& peerID, int socketTimeout)
{
   QSharedPointer<BrowseResult> browseResult = QSharedPointer<BrowseResult>(new BrowseResult(this, peerID, socketTimeout));
   this->browseResultsWithoutTag << browseResult.toWeakRef();
   return browseResult;
}

QSharedPointer<IBrowseResult> InternalCoreConnection::browse(const Common::Hash& peerID, const Protos::Common::Entry& entry, int socketTimeout)
{
   QSharedPointer<BrowseResult> browseResult = QSharedPointer<BrowseResult>(new BrowseResult(this, peerID, entry, socketTimeout));
   this->browseResultsWithoutTag << browseResult.toWeakRef();
   return browseResult;
}

QSharedPointer<IBrowseResult> InternalCoreConnection::browse(const Common::Hash& peerID, const Protos::Common::Entries& entries, bool withRoots, int socketTimeout)
{
   QSharedPointer<BrowseResult> browseResult = QSharedPointer<BrowseResult>(new BrowseResult(this, peerID, entries, withRoots, socketTimeout));
   this->browseResultsWithoutTag << browseResult.toWeakRef();
   return browseResult;
}

QSharedPointer<ISearchResult> InternalCoreConnection::search(const Protos::Common::FindPattern& findPattern, bool local, int socketTimeout)
{
   QSharedPointer<SearchResult> searchResult = QSharedPointer<SearchResult>(new SearchResult(this, findPattern, local, socketTimeout));
   this->searchResultsWithoutTag << searchResult.toWeakRef();
   return searchResult;
}

void InternalCoreConnection::download(const Common::Hash& peerID, const Protos::Common::Entry& entry)
{
   // We cannot download our entries.
   if (peerID == this->getLocalID())
      return;

   Protos::GUI::Download downloadMessage;
   downloadMessage.mutable_peer_id()->set_hash(peerID.getData(), Common::Hash::HASH_SIZE);
   downloadMessage.mutable_entry()->CopyFrom(entry);
   this->send(Common::MessageHeader::GUI_DOWNLOAD, downloadMessage);
}

void InternalCoreConnection::download(const Common::Hash& peerID, const Protos::Common::Entry& entry, const Common::Hash& sharedFolderID, const QString& path)
{
   // We cannot download our entries.
   if (peerID == this->getLocalID())
      return;

   Protos::GUI::Download downloadMessage;
   downloadMessage.mutable_peer_id()->set_hash(peerID.getData(), Common::Hash::HASH_SIZE);
   downloadMessage.mutable_entry()->CopyFrom(entry);
   if (!sharedFolderID.isNull())
      downloadMessage.mutable_destination_directory_id()->set_hash(sharedFolderID.getData(), Common::Hash::HASH_SIZE);
   Common::ProtoHelper::setStr(downloadMessage, &Protos::GUI::Download::set_destination_path, path);
   this->send(Common::MessageHeader::GUI_DOWNLOAD, downloadMessage);
}

void InternalCoreConnection::cancelDownloads(const QList<quint64>& downloadIDs, bool complete)
{
   Protos::GUI::CancelDownloads cancelDownloadsMessage;
   for (QListIterator<quint64> i(downloadIDs); i.hasNext();)
      cancelDownloadsMessage.add_id(i.next());
   cancelDownloadsMessage.set_complete(complete);
   this->send(Common::MessageHeader::GUI_CANCEL_DOWNLOADS, cancelDownloadsMessage);
}

void InternalCoreConnection::pauseDownloads(const QList<quint64>& downloadIDs, bool pause)
{
   Protos::GUI::PauseDownloads pauseDownloadsMessage;
   for (QListIterator<quint64> i(downloadIDs); i.hasNext();)
      pauseDownloadsMessage.add_id(i.next());
   pauseDownloadsMessage.set_pause(pause);
   this->send(Common::MessageHeader::GUI_PAUSE_DOWNLOADS, pauseDownloadsMessage);
}

void InternalCoreConnection::moveDownloads(const QList<quint64>& downloadIDRefs, const QList<quint64>& downloadIDs, Protos::GUI::MoveDownloads::Position position)
{
   if (downloadIDRefs.isEmpty() || downloadIDs.isEmpty()) // Nothing to do in this case.
      return;

   Protos::GUI::MoveDownloads moveDownloadsMessage;
   for (QListIterator<quint64> i(downloadIDRefs); i.hasNext();)
      moveDownloadsMessage.add_id_ref(i.next());
   moveDownloadsMessage.set_position(position);
   for (QListIterator<quint64> i(downloadIDs); i.hasNext();)
      moveDownloadsMessage.add_id_to_move(i.next());
   this->send(Common::MessageHeader::GUI_MOVE_DOWNLOADS, moveDownloadsMessage);
}

void InternalCoreConnection::refresh()
{
   this->send(Common::MessageHeader::GUI_REFRESH);
}

void InternalCoreConnection::refreshNetworkInterfaces()
{
   this->send(Common::MessageHeader::GUI_REFRESH_NETWORK_INTERFACES);
}

bool InternalCoreConnection::isRunningAsSubProcess() const
{
   return this->coreController.getStatus() == RUNNING_AS_SUB_PROCESS;
}

ICoreConnection::ConnectionInfo InternalCoreConnection::getConnectionInfo() const
{
   return this->connectionInfo;
}

void InternalCoreConnection::adressResolved(QHostInfo hostInfo)
{
   this->currentHostLookupID = -1;

   if (hostInfo.addresses().isEmpty())
   {
      emit connectingError(ICoreConnection::RCC_ERROR_HOST_UNKOWN);
      return;
   }

   this->addressesToTry = hostInfo.addresses();
   this->addressesToRetry.clear();
   this->nbRetries = 0;
   this->tryToConnectToTheNextAddress();
}

void InternalCoreConnection::tryToConnectToTheNextAddress()
{
   if (this->addressesToTry.isEmpty())
      return;

   QHostAddress address;

   // Search for an IPv6 address first.
   for (QMutableListIterator<QHostAddress> i(this->addressesToTry); i.hasNext();)
   {
      QHostAddress currentAddress = i.next();
      if (currentAddress.protocol() == QAbstractSocket::IPv6Protocol)
      {
         address = currentAddress;
         i.remove();
         break;
      }
   }

   if (address.isNull())
      address = this->addressesToTry.takeFirst();

   L_DEBU(QString("Trying to connect to %1 (nb retry: %2) ...").arg(address.toString()).arg(this->nbRetries));

   // If the address is local then check if the core is launched, if not try to launch it.
   if (Common::Global::isLocal(address))
   {
      this->coreController.startCore(this->connectionInfo.port);
      if (this->coreController.getStatus() == NOT_RUNNING)
         L_WARN("Unable to start the Core");
   }

   connect(this->socket, &QAbstractSocket::stateChanged, this, &InternalCoreConnection::stateChanged);
   this->socket->connectToHost(address, this->connectionInfo.port);
   this->addressesToRetry << address;
}

void InternalCoreConnection::stateChanged(QAbstractSocket::SocketState socketState)
{
   switch (socketState)
   {
   case QAbstractSocket::UnconnectedState:
      disconnect(this->socket, &QAbstractSocket::stateChanged, this, &InternalCoreConnection::stateChanged);
      if (!this->addressesToTry.isEmpty())
      {
         this->tryToConnectToTheNextAddress();
      }
      else if (this->nbRetries++ < NB_RETRIES_MAX)
      {
         this->addressesToTry = this->addressesToRetry;
         this->addressesToRetry.clear();
         QTimer::singleShot(TIME_BETWEEN_RETRIES, this, SLOT(tryToConnectToTheNextAddress()));
      }
      else
      {
         emit connectingError(ICoreConnection::RCC_ERROR_HOST_TIMEOUT);
      }
      break;

   case QAbstractSocket::ConnectedState:
      disconnect(this->socket, &QAbstractSocket::stateChanged, this, &InternalCoreConnection::stateChanged);
      // Now we wait a message 'Protos.GUI.AskForAuthentication' from the Core before being authenticated.

   default:;
   }
}

void InternalCoreConnection::connectedAndAuthenticated()
{
   // If we were previously connected we announce it.
   if (this->authenticated)
      emit disconnected(this->forcedToClose);

   this->authenticated = true;

   this->sendCurrentLanguage();
   emit connected();
}

void InternalCoreConnection::sendCurrentLanguage()
{
   if (this->authenticated)
   {
      Protos::GUI::Language langMess;
      Common::ProtoHelper::setLang(*langMess.mutable_language(), this->currentLanguage);
      this->send(Common::MessageHeader::GUI_LANGUAGE, langMess);
   }
}

void InternalCoreConnection::onNewMessage(const Common::Message& message)
{
   // While we are not authenticated we accept only two message types.
   if (!this->authenticated && message.getHeader().getType() != Common::MessageHeader::GUI_ASK_FOR_AUTHENTICATION && message.getHeader().getType() != Common::MessageHeader::GUI_AUTHENTICATION_RESULT)
      return;

   switch (message.getHeader().getType())
   {
   case Common::MessageHeader::GUI_ASK_FOR_AUTHENTICATION:
      {
         const Protos::GUI::AskForAuthentication& askForAuthentication = message.getMessage<Protos::GUI::AskForAuthentication>();

         Protos::GUI::Authentication authentication;

         this->salt = askForAuthentication.salt();

         if (!this->password.isEmpty())
            this->connectionInfo.password = Common::Hasher::hashWithSalt(this->password, this->salt);

         authentication.mutable_password_challenge()->set_hash(Common::Hasher::hashWithSalt(this->connectionInfo.password, askForAuthentication.salt_challenge()).getData(), Common::Hash::HASH_SIZE);
         this->password.clear();
         this->send(Common::MessageHeader::GUI_AUTHENTICATION, authentication);
      }
      break;

   case Common::MessageHeader::GUI_AUTHENTICATION_RESULT:
      {
         const Protos::GUI::AuthenticationResult& authenticationResult = message.getMessage<Protos::GUI::AuthenticationResult>();

         if (authenticationResult.status() == Protos::GUI::AuthenticationResult::AUTH_OK)
         {
            this->connectedAndAuthenticated();
         }
         else
         {
            switch (authenticationResult.status())
            {
            case Protos::GUI::AuthenticationResult::AUTH_PASSWORD_NOT_DEFINED:
               emit connectingError(ICoreConnection::RCC_ERROR_NO_REMOTE_PASSWORD_DEFINED);
               break;

            case Protos::GUI::AuthenticationResult::AUTH_BAD_PASSWORD:
                emit connectingError(ICoreConnection::RCC_ERROR_WRONG_PASSWORD);
               break;

            case Protos::GUI::AuthenticationResult::AUTH_ERROR:
               emit connectingError(ICoreConnection::RCC_ERROR_UNKNOWN);
               break;

            default:;
            }
         }
      }
      break;

   case Common::MessageHeader::GUI_STATE:
      {
         const Protos::GUI::State& state = message.getMessage<Protos::GUI::State>();

         emit newState(state);
         this->send(Common::MessageHeader::GUI_STATE_RESULT);
      }
      break;

   case Common::MessageHeader::GUI_EVENT_CHAT_MESSAGES:
      {
         const Protos::Common::ChatMessages& chatMessages = message.getMessage<Protos::Common::ChatMessages>();
         if (chatMessages.message_size() > 0)
            emit newChatMessages(chatMessages);
      }
      break;

   case Common::MessageHeader::GUI_EVENT_LOG_MESSAGES:
      {
         const Protos::GUI::EventLogMessages& eventLogMessages = message.getMessage<Protos::GUI::EventLogMessages>();

         QList<QSharedPointer<LM::IEntry>> entries;

         for (int i = 0; i < eventLogMessages.message_size(); i++)
         {
            const QDateTime dateTime = QDateTime::fromMSecsSinceEpoch(eventLogMessages.message(i).time());
            const QString& message = Common::ProtoHelper::getStr(eventLogMessages.message(i), &Protos::GUI::EventLogMessages::EventLogMessage::message);
            const LM::Severity severity = LM::Severity(eventLogMessages.message(i).severity());
            entries << LM::Builder::newEntry(dateTime, severity, message);
         }

         emit newLogMessages(entries);
      }
      break;

   case Common::MessageHeader::GUI_CHAT_MESSAGE_RESULT:
      while (!this->sendChatMessageResultWithoutReply.isEmpty())
      {
         const Protos::GUI::ChatMessageResult result = message.getMessage<Protos::GUI::ChatMessageResult>();
         QWeakPointer<SendChatMessageResult> sendChatMessageResult = this->sendChatMessageResultWithoutReply.takeFirst();
         if (!sendChatMessageResult.isNull())
         {
            sendChatMessageResult.data()->setResult(result);
            break;
         }
      }
      break;

   case Common::MessageHeader::GUI_SEARCH_TAG:
      {
         const Protos::GUI::Tag& tagMessage = message.getMessage<Protos::GUI::Tag>();

         while (!this->searchResultsWithoutTag.isEmpty())
         {
            QWeakPointer<SearchResult> searchResult = this->searchResultsWithoutTag.takeFirst();
            if (!searchResult.isNull())
            {
               searchResult.data()->setTag(tagMessage.tag());
               break;
            }
         }
      }
      break;

   case Common::MessageHeader::GUI_SEARCH_RESULT:
      {
         const Protos::Common::FindResult& findResultMessage = message.getMessage<Protos::Common::FindResult>();
         emit searchResult(findResultMessage);
      }
      break;

   case Common::MessageHeader::GUI_BROWSE_TAG:
      {
         const Protos::GUI::Tag& tagMessage = message.getMessage<Protos::GUI::Tag>();

         while (!this->browseResultsWithoutTag.isEmpty())
         {
            QWeakPointer<BrowseResult> browseResult = this->browseResultsWithoutTag.takeFirst();
            if (!browseResult.isNull())
            {
               browseResult.data()->setTag(tagMessage.tag());
               break;
            }
         }
      }
      break;

   case Common::MessageHeader::GUI_BROWSE_RESULT:
      {
         const Protos::GUI::BrowseResult& browseResultMessage = message.getMessage<Protos::GUI::BrowseResult>();

         emit browseResult(browseResultMessage);
      }
      break;

   default:;
   }
}

void InternalCoreConnection::onDisconnected()
{
   this->authenticated = false;
   emit disconnected(this->forcedToClose);
   this->forcedToClose = false;
}
