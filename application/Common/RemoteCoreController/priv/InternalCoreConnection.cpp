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

#include <LogManager/Builder.h>
#include <ZeroCopyStreamQIODevice.h>
#include <ProtoHelper.h>
#include <Constants.h>

#include <priv/CoreController.h>
#include <priv/Log.h>
#include <priv/BrowseResult.h>
#include <priv/SearchResult.h>

void InternalCoreConnection::Logger::logDebug(const QString& message)
{
   L_DEBU(message);
}

void InternalCoreConnection::Logger::logError(const QString& message)
{
   L_WARN(message);
}

InternalCoreConnection::InternalCoreConnection() :
   Common::MessageSocket(new InternalCoreConnection::Logger()),
   coreStatus(NOT_RUNNING),
   currentHostLookupID(-1),
   authenticated(false)
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
      emit connectingError(ICoreConnection::ERROR_ALREADY_CONNECTED_TO_THIS_CORE);
      return;
   }

   if (this->currentHostLookupID != -1)
      QHostInfo::abortHostLookup(this->currentHostLookupID);

   this->currentHostLookupID = QHostInfo::lookupHost(this->connectionInfo.address, this, SLOT(adressResolved(QHostInfo)));
}

bool InternalCoreConnection::isConnected() const
{
   return MessageSocket::isConnected() && this->authenticated;
}

void InternalCoreConnection::disconnectFromCore()
{
   this->addressesToTry.clear();
   this->connectionInfo.clear();
   this->socket->close();
}

void InternalCoreConnection::sendChatMessage(const QString& message)
{
   Protos::GUI::ChatMessage chatMessage;
   Common::ProtoHelper::setStr(chatMessage, &Protos::GUI::ChatMessage::set_message, message);
   this->send(Common::MessageHeader::GUI_CHAT_MESSAGE, chatMessage);
}

void InternalCoreConnection::setCoreSettings(const Protos::GUI::CoreSettings settings)
{
   this->send(Common::MessageHeader::GUI_SETTINGS, settings);
}

void InternalCoreConnection::setCoreLanguage(const QLocale locale)
{
   this->currentLanguage = locale;
   this->sendCurrentLanguage();
}

void InternalCoreConnection::setCorePassword(Hash newPassword, Hash oldPassword)
{
   Protos::GUI::ChangePassword passMess;
   passMess.mutable_new_password()->set_hash(newPassword.getData(), Common::Hash::HASH_SIZE);

   if (!oldPassword.isNull())
      passMess.mutable_old_password()->set_hash(oldPassword.getData(), Common::Hash::HASH_SIZE);

   this->send(Common::MessageHeader::GUI_CHANGE_PASSWORD, passMess);
}

QSharedPointer<IBrowseResult> InternalCoreConnection::browse(const Common::Hash& peerID)
{
   QSharedPointer<BrowseResult> browseResult = QSharedPointer<BrowseResult>(new BrowseResult(this, peerID));
   this->browseResultsWithoutTag << browseResult.toWeakRef();
   return browseResult;
}

QSharedPointer<IBrowseResult> InternalCoreConnection::browse(const Common::Hash& peerID, const Protos::Common::Entry& entry)
{
   QSharedPointer<BrowseResult> browseResult = QSharedPointer<BrowseResult>(new BrowseResult(this, peerID, entry));
   this->browseResultsWithoutTag << browseResult.toWeakRef();
   return browseResult;
}

QSharedPointer<IBrowseResult> InternalCoreConnection::browse(const Common::Hash& peerID, const Protos::Common::Entries& entries, bool withRoots)
{
   QSharedPointer<BrowseResult> browseResult = QSharedPointer<BrowseResult>(new BrowseResult(this, peerID, entries, withRoots));
   this->browseResultsWithoutTag << browseResult.toWeakRef();
   return browseResult;
}

QSharedPointer<ISearchResult> InternalCoreConnection::search(const QString& terms)
{
   QSharedPointer<SearchResult> searchResult = QSharedPointer<SearchResult>(new SearchResult(this, terms));
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

bool InternalCoreConnection::isRunningAsSubProcess() const
{
   return this->coreStatus == RUNNING_AS_SUB_PROCESS;
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
      emit connectingError(ICoreConnection::ERROR_HOST_UNKOWN);
      return;
   }

   this->addressesToTry = hostInfo.addresses();

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

   // If the address is local check if the core is launched, if not try to launch it.
#ifndef DEBUG
   if (address == QHostAddress::LocalHost || address == QHostAddress::LocalHostIPv6)
         this->coreStatus = CoreController::StartCore();
#endif

   connect(this->socket, SIGNAL(stateChanged(QAbstractSocket::SocketState)), this, SLOT(stateChanged(QAbstractSocket::SocketState)));
   this->socket->connectToHost(address, this->connectionInfo.port);
}

void InternalCoreConnection::stateChanged(QAbstractSocket::SocketState socketState)
{
   switch (socketState)
   {
   case QAbstractSocket::UnconnectedState:
      if (!this->addressesToTry.isEmpty())
      {
         this->tryToConnectToTheNextAddress();
      }
      else
      {
         emit connectingError(ICoreConnection::ERROR_HOST_TIMEOUT);
      }
      break;

   case QAbstractSocket::ConnectedState:
      disconnect(this->socket, SIGNAL(stateChanged(QAbstractSocket::SocketState)), this, SLOT(stateChanged(QAbstractSocket::SocketState)));

      if (this->isLocal())
      {
         this->connectedAndAuthenticated();
      }
      else
      {
         Protos::GUI::Authentication authMessage;
         authMessage.mutable_password()->set_hash(this->connectionInfo.password.getData(), Common::Hash::HASH_SIZE);
         this->send(Common::MessageHeader::GUI_AUTHENTICATION, authMessage);
      }

   default:;
   }
}

void InternalCoreConnection::connectedAndAuthenticated()
{
   // If we were previously connected we announce it.
   if (this->authenticated)
      emit disconnected();

   this->authenticated = true;

   this->sendCurrentLanguage();
   emit connected();
}

void InternalCoreConnection::sendCurrentLanguage()
{
   Protos::GUI::Language langMess;
   ProtoHelper::setLang(*langMess.mutable_language(), this->currentLanguage);
   this->send(Common::MessageHeader::GUI_LANGUAGE, langMess);
}

void InternalCoreConnection::onNewMessage(Common::MessageHeader::MessageType type, const google::protobuf::Message& message)
{
   if (type != Common::MessageHeader::GUI_AUTHENTICATION_RESULT && !this->authenticated)
      return;

   switch (type)
   {
   case Common::MessageHeader::GUI_AUTHENTICATION_RESULT:
      {
         const Protos::GUI::AuthenticationResult& authenticationResult = static_cast<const Protos::GUI::AuthenticationResult&>(message);

         if (authenticationResult.status() == Protos::GUI::AuthenticationResult_Status_OK)
         {
            this->connectedAndAuthenticated();
         }
         else
         {
            switch (authenticationResult.status())
            {
            case Protos::GUI::AuthenticationResult_Status_PASSWORD_NOT_DEFINED:
               emit connectingError(ICoreConnection::ERROR_NO_REMOTE_PASSWORD_DEFINED);
               break;

            case Protos::GUI::AuthenticationResult_Status_BAD_PASSWORD:
                emit connectingError(ICoreConnection::ERROR_WRONG_PASSWORD);
               break;

            case Protos::GUI::AuthenticationResult_Status_ERROR:
               emit connectingError(ICoreConnection::ERROR_UNKNOWN);
               break;

            default:;
            }
         }
      }
      break;

   case Common::MessageHeader::GUI_STATE:
      {
         const Protos::GUI::State& state = static_cast<const Protos::GUI::State&>(message);

         emit newState(state);
         this->send(Common::MessageHeader::GUI_STATE_RESULT);
      }
      break;

   case Common::MessageHeader::GUI_EVENT_CHAT_MESSAGES:
      {
         const Protos::GUI::EventChatMessages& eventChatMessages = static_cast<const Protos::GUI::EventChatMessages&>(message);
         if (eventChatMessages.message_size() > 0)
            emit newChatMessages(eventChatMessages);
      }
      break;

   case Common::MessageHeader::GUI_EVENT_LOG_MESSAGE:
      {
         const Protos::GUI::EventLogMessage& eventLogMessage = static_cast<const Protos::GUI::EventLogMessage&>(message);

         QDateTime dateTime = QDateTime::fromMSecsSinceEpoch(eventLogMessage.time());
         QString message = Common::ProtoHelper::getStr(eventLogMessage, &Protos::GUI::EventLogMessage::message);
         LM::Severity severity = LM::Severity(eventLogMessage.severity());
         emit newLogMessage(LM::Builder::newEntry(dateTime, severity, message));
      }
      break;

   case Common::MessageHeader::GUI_SEARCH_TAG:
      {
         const Protos::GUI::Tag& tagMessage = static_cast<const Protos::GUI::Tag&>(message);

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
         const Protos::Common::FindResult& findResultMessage = static_cast<const Protos::Common::FindResult&>(message);

         emit searchResult(findResultMessage);
      }
      break;

   case Common::MessageHeader::GUI_BROWSE_TAG:
      {
         const Protos::GUI::Tag& tagMessage = static_cast<const Protos::GUI::Tag&>(message);

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
         const Protos::GUI::BrowseResult& browseResultMessage = static_cast<const Protos::GUI::BrowseResult&>(message);

         emit browseResult(browseResultMessage);
      }
      break;

   default:;
   }
}

void InternalCoreConnection::onDisconnected()
{
   this->authenticated = false;
   emit disconnected();
}
