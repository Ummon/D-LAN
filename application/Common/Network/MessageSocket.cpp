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
  
#include <Common/Network/MessageSocket.h>
using namespace Common;

#include <Protos/common.pb.h>
#include <Protos/core_protocol.pb.h>
#include <Protos/gui_protocol.pb.h>

#include <ZeroCopyStreamQIODevice.h>
#include <ProtoHelper.h>

/**
  * @class Common::MessageSocket
  *
  * An abstract class which is able to send and receive protocol buffer messages over a QAbstractSocket.
  */

/**
  * Take ownership of 'logger'.
  */
MessageSocket::MessageSocket(MessageSocket::ILogger* logger, const Hash& ID, const Hash& remoteID) :
   logger(logger), socket(new QTcpSocket()), ID(ID), remoteID(remoteID), IDDefined(!ID.isNull()), remoteIDDefined(!remoteID.isNull()), listening(false)
{
#ifdef DEBUG
   this->num = ++MessageSocket::currentNum;
   MESSAGE_SOCKET_LOG_DEBUG(QString("New Socket[%1] (not connected)").arg(this->num));
#endif
}

/**
  * Take ownership of 'socket'.
  * If remoteID isn't given, it will be initialized by the ID of the first received message.
  * If ID isn't given, it will be set to the remoteID when the first message is received.
  */
MessageSocket::MessageSocket(MessageSocket::ILogger* logger, QAbstractSocket* socket, const Hash& ID, const Hash& remoteID) :
   logger(logger), socket(socket), ID(ID), remoteID(remoteID), IDDefined(!ID.isNull()), remoteIDDefined(!remoteID.isNull()), listening(false)
{
#ifdef DEBUG
   this->num = ++MessageSocket::currentNum;
   MESSAGE_SOCKET_LOG_DEBUG(QString("New Socket[%1] (connection from %2:%3)").arg(this->num).arg(socket->peerAddress().toString()).arg(socket->peerPort()));
#endif
}

/**
  * Will automatically create a connection to the given address and port.
  */
MessageSocket::MessageSocket(MessageSocket::ILogger* logger, const QHostAddress& address, quint16 port, const Hash& ID, const Hash& remoteID) :
   logger(logger), socket(new QTcpSocket()), ID(ID), remoteID(remoteID), IDDefined(!ID.isNull()), remoteIDDefined(!remoteID.isNull()), listening(false)
{
#ifdef DEBUG
   this->num = ++MessageSocket::currentNum;
   MESSAGE_SOCKET_LOG_DEBUG(QString("New Socket[%1] (connection to %2:%3)").arg(this->num).arg(address.toString()).arg(port));
#endif

   this->socket->connectToHost(address, port);
}

MessageSocket::~MessageSocket()
{
   this->stopListening();

   this->socket->close();
   this->socket->deleteLater();

   delete this->logger;
}

Hash MessageSocket::getID() const
{
   return this->ID;
}

/**
  * Return the remote peer ID. Cannot be modified.
  */
Hash MessageSocket::getRemoteID() const
{
   return this->remoteID;
}

/**
  * Send a given message, the message type must match the 'type' parameter.
  */
void MessageSocket::send(MessageHeader::MessageType type, const google::protobuf::Message& message)
{
   this->send(type, &message);
}

void MessageSocket::send(MessageHeader::MessageType type)
{
   this->send(type, 0);
}

void MessageSocket::send(MessageHeader::MessageType type, const google::protobuf::Message* message)
{
   if (!this->listening)
      return;

   MessageHeader header(type, message ? message->ByteSize() : 0, this->ID);

   MESSAGE_SOCKET_LOG_DEBUG(QString("Socket[%1]::send : %2 to %3\n%4").arg(this->num).arg(header.toStr()).arg(this->remoteID.toStr()).arg(message ? ProtoHelper::getDebugStr(*message) : "<empty message>"));

   if (message)
   {
      MessageHeader::writeHeader(*this->socket, header);
      ZeroCopyOutputStreamQIODevice outputStream(this->socket);
      if (!message->SerializeToZeroCopyStream(&outputStream))
         MESSAGE_SOCKET_LOG_ERROR(QString("Unable to send\n%1").arg(ProtoHelper::getDebugStr(*message)));
   }

   if (this->socket->state() == QAbstractSocket::ConnectedState)
      this->socket->flush();
}

/**
  * Start listening the socket and reading new messages.
  */
void MessageSocket::startListening()
{
   // To prevent multi listening.
   if (this->listening)
      return;

   MESSAGE_SOCKET_LOG_DEBUG(QString("Socket[%1] starting to listen").arg(this->num));

   this->listening = true;

   connect(this->socket, SIGNAL(readyRead()), this, SLOT(dataReceived()), Qt::DirectConnection);
   connect(this->socket, SIGNAL(disconnected()), this, SLOT(disconnected()), Qt::DirectConnection);
   this->dataReceived();
}

/**
  * Stop listening the socket.
  */
void MessageSocket::stopListening()
{
   MESSAGE_SOCKET_LOG_DEBUG(QString("Socket[%1] stopping to listen").arg(this->num));

   disconnect(this->socket, SIGNAL(readyRead()), this, SLOT(dataReceived()));
   disconnect(this->socket, SIGNAL(disconnected()), this, SLOT(disconnected()));

   this->listening = false;
}

bool MessageSocket::isLocal() const
{
   return this->socket->peerAddress() == QHostAddress::LocalHost || this->socket->peerAddress() == QHostAddress::LocalHostIPv6;
}

bool MessageSocket::isConnected() const
{
   return this->socket->state() == QAbstractSocket::ConnectedState;
}

bool MessageSocket::isListening() const
{
   return this->listening;
}

void MessageSocket::dataReceived()
{
   while (!this->socket->atEnd() && this->listening)
   {
      this->onNewDataReceived();
      if (this->currentHeader.isNull() && this->socket->bytesAvailable() >= MessageHeader::HEADER_SIZE)
      {
         this->currentHeader = MessageHeader::readHeader(*this->socket);

         if (this->remoteID.isNull())
            this->remoteID = this->currentHeader.getSenderID();
         if (this->ID.isNull())
            this->ID = this->remoteID;

         if (this->currentHeader.getSenderID() != this->remoteID)
         {
            MESSAGE_SOCKET_LOG_DEBUG(QString("Socket[%1]: Peer ID from message (%2) doesn't match the known peer ID (%3)").arg(this->num).arg(this->currentHeader.getSenderID().toStr()).arg(this->ID.toStr()));
            this->currentHeader.setNull();
            this->socket->close();
            return;
         }
      }

      if (!this->currentHeader.isNull() && this->socket->bytesAvailable() >= this->currentHeader.getSize())
      {
         if (!this->readMessage())
         {
            MESSAGE_SOCKET_LOG_DEBUG("Can't read the message -> finished");
            this->socket->close();
         }
         this->currentHeader.setNull();
      }
      else
         return;
   }
}

void MessageSocket::disconnected()
{
   if (!this->IDDefined)
      this->ID = Common::Hash();
   if (!this->remoteIDDefined)
      this->remoteID = Common::Hash();

   this->currentHeader.setNull();
   MESSAGE_SOCKET_LOG_DEBUG(QString("Socket[%1] disconnected").arg(this->num));
   this->onDisconnected();
}

/**
  * Read the next message corresponding to the header type.
  */
bool MessageSocket::readMessage()
{
   switch (this->currentHeader.getType())
   {
   case MessageHeader::CORE_GET_ENTRIES: return this->readMessage<Protos::Core::GetEntries>();
   case MessageHeader::CORE_GET_ENTRIES_RESULT: return this->readMessage<Protos::Core::GetEntriesResult>();
   case MessageHeader::CORE_GET_HASHES: return this->readMessage<Protos::Core::GetHashes>();
   case MessageHeader::CORE_GET_HASHES_RESULT: return this->readMessage<Protos::Core::GetHashesResult>();
   case MessageHeader::CORE_HASH: return this->readMessage<Protos::Common::Hash>();
   case MessageHeader::CORE_GET_CHUNK: return this->readMessage<Protos::Core::GetChunk>();
   case MessageHeader::CORE_GET_CHUNK_RESULT: return this->readMessage<Protos::Core::GetChunkResult>();

   case MessageHeader::GUI_STATE: return this->readMessage<Protos::GUI::State>();
   case MessageHeader::GUI_EVENT_CHAT_MESSAGES: return this->readMessage<Protos::GUI::EventChatMessages>();
   case MessageHeader::GUI_EVENT_LOG_MESSAGE: return this->readMessage<Protos::GUI::EventLogMessage>();
   case MessageHeader::GUI_AUTHENTICATION: return this->readMessage<Protos::GUI::Authentication>();
   case MessageHeader::GUI_AUTHENTICATION_RESULT: return this->readMessage<Protos::GUI::AuthenticationResult>();
   case MessageHeader::GUI_LANGUAGE: return this->readMessage<Protos::GUI::Language>();
   case MessageHeader::GUI_SETTINGS: return this->readMessage<Protos::GUI::CoreSettings>();
   case MessageHeader::GUI_SEARCH: return this->readMessage<Protos::GUI::Search>();
   case MessageHeader::GUI_SEARCH_TAG: return this->readMessage<Protos::GUI::Tag>();
   case MessageHeader::GUI_SEARCH_RESULT: return this->readMessage<Protos::Common::FindResult>();
   case MessageHeader::GUI_BROWSE: return this->readMessage<Protos::GUI::Browse>();
   case MessageHeader::GUI_BROWSE_TAG: return this->readMessage<Protos::GUI::Tag>();
   case MessageHeader::GUI_BROWSE_RESULT: return this->readMessage<Protos::GUI::BrowseResult>();
   case MessageHeader::GUI_CANCEL_DOWNLOADS: return this->readMessage<Protos::GUI::CancelDownloads>();
   case MessageHeader::GUI_PAUSE_DOWNLOADS: return this->readMessage<Protos::GUI::PauseDownloads>();
   case MessageHeader::GUI_MOVE_DOWNLOADS: return this->readMessage<Protos::GUI::MoveDownloads>();
   case MessageHeader::GUI_DOWNLOAD: return this->readMessage<Protos::GUI::Download>();
   case MessageHeader::GUI_CHAT_MESSAGE: return this->readMessage<Protos::GUI::ChatMessage>();
   case MessageHeader::GUI_REFRESH: return this->readMessage<Protos::Common::Null>();

   default:
      return false;
   }
}

bool MessageSocket::readProtoMessage(google::protobuf::Message& message)
{
   bool readOK = false;
   {
      ZeroCopyInputStreamQIODevice inputStream(this->socket);
      readOK = message.ParseFromBoundedZeroCopyStream(&inputStream, this->currentHeader.getSize());
   }

   if (readOK)
   {
      MESSAGE_SOCKET_LOG_DEBUG(QString("Socket[%1]: Data received from %2, %3\n%4")
         .arg(this->num)
         .arg(this->socket->peerAddress().toString())
         .arg(this->currentHeader.toStr())
         .arg(Common::ProtoHelper::getDebugStr(message))
      );
   }
   return readOK;
}

#ifdef DEBUG
   int MessageSocket::currentNum(0);
#endif
