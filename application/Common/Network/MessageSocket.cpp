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

#include <QNetworkInterface>

#include <Protos/common.pb.h>
#include <Protos/core_protocol.pb.h>
#include <Protos/gui_protocol.pb.h>

#include <ProtoHelper.h>
#include <Global.h>

/**
  * @class Common::MessageSocket
  *
  * An abstract class which is able to send and receive protocol buffer messages over a QAbstractSocket.
  * It is designed to be sublcassed,
  */

/**
  * Build a non-connected message socket.
  * Take ownership of 'logger'.
  */
MessageSocket::MessageSocket(MessageSocket::ILogger* logger, const Hash& localID, const Hash& remoteID) :
   logger(logger),
   socket(new QTcpSocket()),
   localID(localID),
   remoteID(remoteID),
   localIDDefined(!localID.isNull()),
   remoteIDDefined(!remoteID.isNull()),
   listening(false)
{
#ifdef DEBUG
   this->num = ++MessageSocket::currentNum;
   MESSAGE_SOCKET_LOG_DEBUG(QString("New MessageSocket[%1] (not connected)").arg(this->num));
#endif
}

/**
  * Takes the ownership of 'socket'.
  * If remoteID isn't given, it will be initialized by the ID of the first received message.
  * If localID isn't given, it will be set to the remoteID when the first message is received.
  */
MessageSocket::MessageSocket(MessageSocket::ILogger* logger, QAbstractSocket* socket, const Hash& localID, const Hash& remoteID) :
   logger(logger),
   socket(socket),
   localID(localID),
   remoteID(remoteID),
   localIDDefined(!localID.isNull()),
   remoteIDDefined(!remoteID.isNull()),
   listening(false)
{
#ifdef DEBUG
   this->num = ++MessageSocket::currentNum;
   MESSAGE_SOCKET_LOG_DEBUG(QString("New MessageSocket[%1] (connection from %2:%3)").arg(this->num).arg(socket->peerAddress().toString()).arg(socket->peerPort()));
#endif
}

/**
  * Takes the ownership of 'socket'.
  * Will automatically create a connection to the given address and port.
  */
MessageSocket::MessageSocket(MessageSocket::ILogger* logger, const QHostAddress& address, quint16 port, const Hash& localID, const Hash& remoteID) :
   logger(logger),
   socket(new QTcpSocket()),
   localID(localID), remoteID(remoteID),
   localIDDefined(!localID.isNull()),
   remoteIDDefined(!remoteID.isNull()),
   listening(false)
{
#ifdef DEBUG
   this->num = ++MessageSocket::currentNum;
   MESSAGE_SOCKET_LOG_DEBUG(QString("New MessageSocket[%1] (connection to %2:%3)").arg(this->num).arg(address.toString()).arg(port));
#endif

   this->socket->connectToHost(address, port);
}

MessageSocket::~MessageSocket()
{
   this->stopListening();

   this->close();
   this->socket->deleteLater();
   delete this->logger;
}

Hash MessageSocket::getLocalID() const
{
   return this->localID;
}

/**
  * Return the remote peer ID. Cannot be modified.
  */
Hash MessageSocket::getRemoteID() const
{
   return this->remoteID;
}

void MessageSocket::send(MessageHeader::MessageType type, const google::protobuf::Message& message)
{
   this->send(type, &message);
}

/**
  * Send a message without body.
  */
void MessageSocket::send(MessageHeader::MessageType type)
{
   this->send(type, nullptr);
}

void MessageSocket::send(MessageHeader::MessageType type, const google::protobuf::Message* message)
{
   if (!this->listening)
      return;

   MessageHeader header(type, message ? message->ByteSize() : 0, this->localID);

   MESSAGE_SOCKET_LOG_DEBUG(QString("Socket[%1]::send : %2 to %3\n%4").arg(this->num).arg(header.toStr()).arg(this->remoteID.toStr()).arg(message ? ProtoHelper::getDebugStr(*message) : "<empty message>"));

   Message::writeMessageToDevice(this->socket, header, message);
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

   connect(this->socket, SIGNAL(readyRead()), this, SLOT(dataReceivedSlot()), Qt::DirectConnection);
   connect(this->socket, SIGNAL(disconnected()), this, SLOT(disconnectedSlot()), Qt::DirectConnection);

   this->dataReceivedSlot();
}

/**
  * Stop listening the socket.
  * It's useful when some non-message data has to be sent, like a stream of data.
  */
void MessageSocket::stopListening()
{
   MESSAGE_SOCKET_LOG_DEBUG(QString("Socket[%1] stopping to listen").arg(this->num));

   disconnect(this->socket, SIGNAL(readyRead()), this, SLOT(dataReceivedSlot()));
   disconnect(this->socket, SIGNAL(disconnected()), this, SLOT(disconnectedSlot()));

   this->listening = false;
}

bool MessageSocket::isLocal() const
{
   return Global::isLocal(this->socket->peerAddress());
}

bool MessageSocket::isConnected() const
{
   return this->socket->state() == QAbstractSocket::ConnectedState;
}

void MessageSocket::close()
{
   this->socket->close();
}

bool MessageSocket::isListening() const
{
   return this->listening;
}

/**
  * Called when new data has arrived.
  */
void MessageSocket::dataReceivedSlot()
{
   while (!this->socket->atEnd() && this->listening)
   {
      this->onNewDataReceived();
      if (this->currentHeader.isNull() && this->socket->bytesAvailable() >= MessageHeader::HEADER_SIZE)
      {
         this->currentHeader = MessageHeader::readHeader(*this->socket);

         if (this->remoteID.isNull())
            this->remoteID = this->currentHeader.getSenderID();
         if (this->localID.isNull())
            this->localID = this->remoteID;

         if (this->currentHeader.getSenderID() != this->remoteID)
         {
            MESSAGE_SOCKET_LOG_DEBUG(QString("Socket[%1]: Peer ID from message (%2) doesn't match the known peer ID (%3)").arg(this->num).arg(this->currentHeader.getSenderID().toStr()).arg(this->localID.toStr()));
            this->currentHeader.setNull();
            this->socket->close();
            return;
         }
      }

      if (!this->currentHeader.isNull() && this->socket->bytesAvailable() >= this->currentHeader.getSize())
      {
         if (!this->readMessage())
         {
            MESSAGE_SOCKET_LOG_DEBUG(QString("Socket[%1]: Unable to read the received message, closing the socket. Message type: %2").arg(this->num).arg(this->currentHeader.getType()));
            this->socket->close();
            return;
         }
         this->currentHeader.setNull();
      }
      else
         return;
   }
}

void MessageSocket::disconnectedSlot()
{
   if (!this->localIDDefined)
      this->localID = Common::Hash();
   if (!this->remoteIDDefined)
      this->remoteID = Common::Hash();

   this->currentHeader.setNull();
   MESSAGE_SOCKET_LOG_DEBUG(QString("Socket[%1] disconnected").arg(this->num));
   this->onDisconnected();
}

/**
  * Read the next message corresponding to the current header type.
  */
bool MessageSocket::readMessage()
{
   try
   {
      const Message& message = Message::readMessageBodyFromDevice(this->currentHeader, this->socket);

      MESSAGE_SOCKET_LOG_DEBUG(QString("Socket[%1]: Data received from %2, %3\n%4")
         .arg(this->num)
         .arg(this->socket->peerAddress().toString())
         .arg(message.getHeader().toStr())
         .arg(Common::ProtoHelper::getDebugStr(message.getMessage()))
      );

      this->onNewMessage(message);
      emit newMessage(message);
      return true;
   }
   catch (ReadErrorException& e)
   {
      return false;
   }
}

#ifdef DEBUG
   int MessageSocket::currentNum(0);
#endif
