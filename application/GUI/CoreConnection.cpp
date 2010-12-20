/**
  * Aybabtu - A decentralized LAN file sharing software.
  * Copyright (C) 2010-2011 Greg Burri <greg.burri@gmail.com>
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
  
#include "CoreConnection.h"
using namespace GUI;

#include <QHostAddress>
#include <QCoreApplication>

#include <Common/LogManager/Builder.h>
#include <Common/ZeroCopyStreamQIODevice.h>
#include <Common/Settings.h>
#include <Common/ProtoHelper.h>
#include <Common/Constants.h>

#include <CoreController.h>
#include <Log.h>

BrowseResult::BrowseResult(CoreConnection* coreConnection, const Common::Hash& peerID)
   : IBrowseResult(SETTINGS.get<quint32>("socket_timeout")), peerID(peerID), tag(0)
{
   this->init(coreConnection);
}

BrowseResult::BrowseResult(CoreConnection* coreConnection, const Common::Hash& peerID, const Protos::Common::Entry& entry)
   : IBrowseResult(SETTINGS.get<quint32>("socket_timeout")), peerID(peerID), entry(entry), tag(0)
{
   this->init(coreConnection);
}

void BrowseResult::start()
{
   Protos::GUI::Browse browseMessage;
   browseMessage.mutable_peer_id()->set_hash(this->peerID.getData(), Common::Hash::HASH_SIZE);
   if (this->entry.IsInitialized())
      browseMessage.mutable_dir()->CopyFrom(this->entry);

   this->coreConnection->send(Common::Network::GUI_BROWSE, browseMessage);
}

void BrowseResult::setTag(quint64 tag)
{
   this->tag = tag;
}

void BrowseResult::browseResult(quint64 tag, const Protos::Common::Entries& entries)
{
   if (tag == this->tag) // Is this message for us?
   {
      this->tag = 0; // To avoid multi emit (should not occurs).
      emit result(entries);
   }
}

void BrowseResult::init(CoreConnection* coreConnection)
{
   this->coreConnection = coreConnection;
   connect(this->coreConnection, SIGNAL(browseResult(quint64, const Protos::Common::Entries&)), this, SLOT(browseResult(quint64, const Protos::Common::Entries&)));
}

/////

SearchResult::SearchResult(CoreConnection* coreConnection, const QString& terms)
   : ISearchResult(SETTINGS.get<quint32>("socket_timeout")), coreConnection(coreConnection), terms(terms)
{
   connect(this->coreConnection, SIGNAL(searchResult(const Protos::Common::FindResult&)), this, SLOT(searchResult(const Protos::Common::FindResult&)));
}

void SearchResult::start()
{
   Protos::GUI::Search search;
   Common::ProtoHelper::setStr(search, &Protos::GUI::Search::set_pattern, this->terms);
   this->coreConnection->send(Common::Network::GUI_SEARCH, search);
}

void SearchResult::setTag(quint64 tag)
{
   this->tag = tag;
}

void SearchResult::searchResult(const Protos::Common::FindResult& findResult)
{
   if (findResult.tag() == this->tag) // Is this message for us?
      emit result(findResult);
}

/////

CoreConnection::CoreConnection()
   : currentHostLookupID(-1), authenticated(false)
{
   connect(&this->socket, SIGNAL(readyRead()), this, SLOT(dataReceived()));
   connect(&this->socket, SIGNAL(connected()), this, SLOT(connected()));
   connect(&this->socket, SIGNAL(disconnected()), this, SLOT(disconnected()));
   connect(&this->socket, SIGNAL(stateChanged(QAbstractSocket::SocketState)), this, SLOT(stateChanged(QAbstractSocket::SocketState)));
}

CoreConnection::~CoreConnection()
{
   if (this->currentHostLookupID != -1)
      QHostInfo::abortHostLookup(this->currentHostLookupID);

   this->addressesToTry.clear();
}

Common::Hash CoreConnection::getOurID() const
{
   return this->ourID;
}

void CoreConnection::sendChatMessage(const QString& message)
{
   Protos::GUI::ChatMessage chatMessage;
   Common::ProtoHelper::setStr(chatMessage, &Protos::GUI::ChatMessage::set_message, message);
   this->send(Common::Network::GUI_CHAT_MESSAGE, chatMessage);
}

void CoreConnection::setCoreSettings(const Protos::GUI::CoreSettings settings)
{
   this->send(Common::Network::GUI_SETTINGS, settings);
}

QSharedPointer<IBrowseResult> CoreConnection::browse(const Common::Hash& peerID)
{
   QSharedPointer<BrowseResult> browseResult = QSharedPointer<BrowseResult>(new BrowseResult(this, peerID));
   this->browseResultsWithoutTag << browseResult;
   return browseResult;
}

QSharedPointer<IBrowseResult> CoreConnection::browse(const Common::Hash& peerID, const Protos::Common::Entry& entry)
{
   QSharedPointer<BrowseResult> browseResult = QSharedPointer<BrowseResult>(new BrowseResult(this, peerID, entry));
   this->browseResultsWithoutTag << browseResult;
   return browseResult;
}

QSharedPointer<ISearchResult> CoreConnection::search(const QString& terms)
{
   QSharedPointer<SearchResult> searchResult = QSharedPointer<SearchResult>(new SearchResult(this, terms));
   this->searchResultsWithoutTag << searchResult;
   return searchResult;
}

void CoreConnection::download(const Common::Hash& peerID, const Protos::Common::Entry& entry)
{
   Protos::GUI::Download downloadMessage;
   downloadMessage.mutable_peer_id()->set_hash(peerID.getData(), Common::Hash::HASH_SIZE);
   downloadMessage.mutable_entry()->CopyFrom(entry);
   this->send(Common::Network::GUI_DOWNLOAD, downloadMessage);
}

void CoreConnection::cancelDownloads(const QList<quint64>& downloadIDs)
{
   Protos::GUI::CancelDownloads cancelDownloadsMessage;
   for(QListIterator<quint64> i(downloadIDs); i.hasNext();)
      cancelDownloadsMessage.add_id(i.next());
   this->send(Common::Network::GUI_CANCEL_DOWNLOADS, cancelDownloadsMessage);
}

void CoreConnection::moveDownloads(quint64 downloadIDRef, const QList<quint64>& downloadIDs, bool moveBefore)
{
   Protos::GUI::MoveDownloads moveDownloadsMessage;
   moveDownloadsMessage.set_id_ref(downloadIDRef);
   moveDownloadsMessage.set_move_before(moveBefore);
   for(QListIterator<quint64> i(downloadIDs); i.hasNext();)
      moveDownloadsMessage.add_id_to_move(i.next());
   this->send(Common::Network::GUI_MOVE_DOWNLOADS, moveDownloadsMessage);
}

void CoreConnection::refresh()
{
   this->send(Common::Network::GUI_REFRESH);
}

bool CoreConnection::isConnected()
{
   return this->socket.state() == QAbstractSocket::ConnectedState;
}

void CoreConnection::connectToCore()
{
   this->socket.close();

   if (this->currentHostLookupID != -1)
      QHostInfo::abortHostLookup(this->currentHostLookupID);

   this->currentHostLookupID = QHostInfo::lookupHost(SETTINGS.get<QString>("core_address"), this, SLOT(adressResolved(QHostInfo)));
}

void CoreConnection::stateChanged(QAbstractSocket::SocketState socketState)
{
   switch(socketState)
   {
   case QAbstractSocket::UnconnectedState:
      if (!this->addressesToTry.isEmpty())
      {
         this->tryToConnectToTheNextAddress();
      }
      else
      {
         L_USER("Unable to connect to the core");
         this->connectToCore();
      }
      break;

   default:;
   }
}

void CoreConnection::dataReceived()
{
   // TODO : it will loop infinetly if not enough data is provided.
   while (!this->socket.atEnd())
   {
      if (this->currentHeader.isNull() && this->socket.bytesAvailable() >= Common::Network::HEADER_SIZE)
      {
         this->currentHeader = Common::Network::readHeader<Common::Network::GUIMessageType>(this->socket);
         this->ourID = this->currentHeader.senderID;

         L_DEBU(QString("Data received : %1").arg(this->currentHeader.toStr()));
      }

      if (!this->currentHeader.isNull() && this->socket.bytesAvailable() >= this->currentHeader.size)
      {
         if (!this->readMessage())
            L_WARN(QString("Unable to read message : %1").arg(this->currentHeader.toStr()));
         this->currentHeader.setNull();
      }
      else
         return;
   }
}

void CoreConnection::adressResolved(QHostInfo hostInfo)
{
   this->currentHostLookupID = -1;

   if (hostInfo.addresses().isEmpty())
   {      
      L_USER(QString("Unable to resolve the address : %1").arg(hostInfo.hostName()));
      return;
   }

   this->addressesToTry = hostInfo.addresses();

   this->tryToConnectToTheNextAddress();

   // Search an IPv4 address. (Old code).
   /*for (QListIterator<QHostAddress> i(hostInfo.addresses()); i.hasNext();)
   {
      QHostAddress currentAddress = i.next();
      if (currentAddress.protocol() == QAbstractSocket::IPv4Protocol)
      {
         address = currentAddress;
         break;
      }
   }*/
}

void CoreConnection::connected()
{
   if (this->socket.peerAddress() == QHostAddress::LocalHost || this->socket.peerAddress() == QHostAddress::LocalHostIPv6)
   {
      this->authenticated = true;
      L_USER("Connected to the core");
      L_DEBU(QString("Core address : %1").arg(this->socket.peerAddress().toString()));
      emit coreConnected();      
   }
   else
   {
      Common::Hash password = SETTINGS.get<Common::Hash>("password");
      Protos::GUI::Authentication authMessage;
      authMessage.mutable_password()->set_hash(password.getData(), Common::Hash::HASH_SIZE);
      this->send(Common::Network::GUI_AUTHENTICATION, authMessage);
   }
}

void CoreConnection::disconnected()
{
   this->authenticated = false;
   emit coreDisconnected();
}

void CoreConnection::tryToConnectToTheNextAddress()
{
   if (this->addressesToTry.isEmpty())
      return;

   QHostAddress address;

   // Search for an IPv4 address first.
   for (QMutableListIterator<QHostAddress> i(this->addressesToTry); i.hasNext();)
   {
      QHostAddress currentAddress = i.next();
      if (currentAddress.protocol() == QAbstractSocket::IPv4Protocol)
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
      CoreController::StartCore();
#endif

   this->socket.connectToHost(address, SETTINGS.get<quint32>("core_port"));
}

void CoreConnection::send(Common::Network::GUIMessageType type)
{
   const Common::Network::MessageHeader<Common::Network::GUIMessageType> header(type, 0, this->ourID);
   L_DEBU(QString("CoreConnection::send : %1").arg(header.toStr()));
   Common::Network::writeHeader(this->socket, header);

   if (this->socket.state() == QAbstractSocket::ConnectedState)
      this->socket.flush();
}

void CoreConnection::send(Common::Network::GUIMessageType type, const google::protobuf::Message& message)
{
   const Common::Network::MessageHeader<Common::Network::GUIMessageType> header(type, message.ByteSize(), this->ourID);

   L_DEBU(QString("CoreConnection::send : %1\n%2").arg(header.toStr()).arg(Common::ProtoHelper::getDebugStr(message)));

   {
      Common::Network::writeHeader(this->socket, header);
      Common::ZeroCopyOutputStreamQIODevice outputStream(&this->socket);
      message.SerializeToZeroCopyStream(&outputStream);
   }

   if (this->socket.state() == QAbstractSocket::ConnectedState)
      this->socket.flush();
}

bool CoreConnection::readMessage()
{
   bool readOK = false;

   switch (this->currentHeader.type)
   {
   case Common::Network::GUI_AUTHENTICATION_RESULT:
      {
         Protos::GUI::AuthenticationResult authenticationResult;

         // This scope (and the others ones below) is here to force the input stream to read all the bytes.
         // See Common::ZeroCopyInputStreamQIODevice::~ZeroCopyInputStreamQIODevice.
         {
            Common::ZeroCopyInputStreamQIODevice inputStream(&this->socket);
            readOK = authenticationResult.ParseFromBoundedZeroCopyStream(&inputStream, this->currentHeader.size);
         }

         if (readOK)
         {
            switch (authenticationResult.status())
            {
            case Protos::GUI::AuthenticationResult_Status_BAD_PASSWORD:
               L_USER("Authentication failed, bad password");
               break;

            case Protos::GUI::AuthenticationResult_Status_ERROR:
               L_USER("Authentication failed");
               break;

            case Protos::GUI::AuthenticationResult_Status_OK:
               this->authenticated = true;
               L_USER("Connected to the core");
               emit coreConnected();
               break;
            }
         }
      }
      break;


   case Common::Network::GUI_STATE:
      {
         Protos::GUI::State state;

         {
            Common::ZeroCopyInputStreamQIODevice inputStream(&this->socket);
            readOK = state.ParseFromBoundedZeroCopyStream(&inputStream, this->currentHeader.size);
         }

         if (readOK && this->authenticated)
            emit newState(state);
      }
      break;

   case Common::Network::GUI_EVENT_CHAT_MESSAGE:
      {
         Protos::GUI::EventChatMessage eventChatMessage;

         {
            Common::ZeroCopyInputStreamQIODevice inputStream(&this->socket);
            readOK = eventChatMessage.ParseFromBoundedZeroCopyStream(&inputStream, this->currentHeader.size);
         }

         if (readOK && this->authenticated)
         {
            Common::Hash peerID(eventChatMessage.peer_id().hash().data());
            emit newChatMessage(peerID, Common::ProtoHelper::getStr(eventChatMessage, &Protos::GUI::EventChatMessage::message));
         }
      }
      break;

   case Common::Network::GUI_EVENT_LOG_MESSAGE:
      {
         Protos::GUI::EventLogMessage eventLogMessage;

         {
            Common::ZeroCopyInputStreamQIODevice inputStream(&this->socket);
            readOK = eventLogMessage.ParseFromBoundedZeroCopyStream(&inputStream, this->currentHeader.size);
         }

         if (readOK && this->authenticated)
         {
            QDateTime dateTime = QDateTime::fromMSecsSinceEpoch(eventLogMessage.time());
            QString message = Common::ProtoHelper::getStr(eventLogMessage, &Protos::GUI::EventLogMessage::message);
            LM::Severity severity = LM::Severity(eventLogMessage.severity());
            emit newLogMessage(LM::Builder::newEntry(dateTime, severity, message));
         }
      }
      break;

   case Common::Network::GUI_SEARCH_TAG:
      {
         Protos::GUI::Tag tagMessage;
         {
            Common::ZeroCopyInputStreamQIODevice inputStream(&this->socket);
            readOK = tagMessage.ParseFromBoundedZeroCopyStream(&inputStream, this->currentHeader.size);
         }

         if (readOK && !this->searchResultsWithoutTag.isEmpty())
            this->searchResultsWithoutTag.takeFirst()->setTag(tagMessage.tag());
      }
      break;

   case Common::Network::GUI_SEARCH_RESULT:
      {
         Protos::Common::FindResult findResultMessage;
         {
            Common::ZeroCopyInputStreamQIODevice inputStream(&this->socket);
            readOK = findResultMessage.ParseFromBoundedZeroCopyStream(&inputStream, this->currentHeader.size);
         }

         if (readOK && this->authenticated)
            emit searchResult(findResultMessage);
      }
      break;

   case Common::Network::GUI_BROWSE_TAG:
      {
         Protos::GUI::Tag tagMessage;
         {
            Common::ZeroCopyInputStreamQIODevice inputStream(&this->socket);
            readOK = tagMessage.ParseFromBoundedZeroCopyStream(&inputStream, this->currentHeader.size);
         }

         if (readOK && this->authenticated && !this->browseResultsWithoutTag.isEmpty())
            this->browseResultsWithoutTag.takeFirst()->setTag(tagMessage.tag());
      }
      break;

   case Common::Network::GUI_BROWSE_RESULT:
      {
         Protos::GUI::BrowseResult browseResultMessage;
         {
            Common::ZeroCopyInputStreamQIODevice inputStream(&this->socket);
            readOK = browseResultMessage.ParseFromBoundedZeroCopyStream(&inputStream, this->currentHeader.size);
         }

         if (readOK && this->authenticated)
            emit browseResult(browseResultMessage.tag(), browseResultMessage.entries());
      }
      break;

   default:
      readOK = false;
   }

   return readOK;
}
