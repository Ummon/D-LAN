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

#include <QPointer>
#include <QScopeGuard>

#include <ProtoHelper.h>
#include <Global.h>

#ifdef DEBUG
namespace
{
   constexpr quint32 MAX_DEBUG_PAYLOAD_SIZE = 4 * 1024;
   constexpr qsizetype MAX_DEBUG_TEXT_SIZE = 8 * 1024;

   QString messageDebugStr(const google::protobuf::Message& message, quint32 payloadSize)
   {
      // Check before JSON conversion so large messages do not allocate huge log strings.
      if (payloadSize > MAX_DEBUG_PAYLOAD_SIZE)
         return QString("[message body omitted: %1 bytes]").arg(payloadSize);

      const QString text = ProtoHelper::getDebugStr(message);
      if (text.size() > MAX_DEBUG_TEXT_SIZE)
         return QString("[message body omitted: debug text exceeds %1 characters]").arg(MAX_DEBUG_TEXT_SIZE);
      return text;
   }
}
#endif

/**
  * @class Common::MessageSocket
  *
  * An abstract class which is able to send and receive protocol buffer messages over a QAbstractSocket.
  * It is designed to be subclassed.
  */

/**
  * Build a non-connected message socket.
  * Take ownership of 'logger'.
  */
MessageSocket::MessageSocket(MessageSocket::ILogger* logger, const Hash& localID, const Hash& remoteID) :
   MessageSocket(logger, new QTcpSocket(), localID, remoteID)
{
}

/**
  * Takes ownership of 'logger' and 'socket'.
  * If remoteID isn't given, it will be initialized by the ID of the first received message.
  * If localID isn't given, it will be set to the remoteID when the first message is received.
  */
MessageSocket::MessageSocket(
   MessageSocket::ILogger* logger,
   QAbstractSocket* socket,
   const Hash& localID,
   const Hash& remoteID
) :
   logger(logger),
   socket(socket),
   localID(localID),
   remoteID(remoteID),
   localIDDefined(!localID.isNull()),
   remoteIDDefined(!remoteID.isNull())
{
   // Own the socket independently of its former parent, while allowing raw
   // transfers to move it to another thread without moving MessageSocket.
   this->socket->setParent(nullptr);

   // The native socket must exist before setting options. Apply this to
   // accepted connections now, and to outgoing connections on every reconnect.
   const auto enableLowDelay = [socket] {
      socket->setSocketOption(QAbstractSocket::LowDelayOption, true);
   };
   connect(this->socket, &QAbstractSocket::connected, this->socket, enableLowDelay);
   if (this->socket->state() == QAbstractSocket::ConnectedState)
      enableLowDelay();

#ifdef DEBUG
   this->num = ++MessageSocket::currentNum;
   MESSAGE_SOCKET_LOG_DEBUG(socket->state() == QAbstractSocket::ConnectedState
      ? QString("New MessageSocket[%1] (connection from %2:%3)").arg(this->num).arg(socket->peerAddress().toString()).arg(socket->peerPort())
      : QString("New MessageSocket[%1] (not connected)").arg(this->num));
#endif
   // Keep tracking the connection while message reads are paused. Only record
   // the event on a transfer thread; session state and callbacks belong here.
   connect(this->socket, &QAbstractSocket::disconnected, this, [this] {
      this->disconnectPending.store(true);
      QMetaObject::invokeMethod(this, &MessageSocket::disconnectedSlot, Qt::AutoConnection);
   }, Qt::DirectConnection);
}

/**
  * Will automatically create a connection to the given address and port.
  */
MessageSocket::MessageSocket(
   MessageSocket::ILogger* logger,
   const QHostAddress& address,
   quint16 port,
   const Hash& localID,
   const Hash& remoteID
) :
   MessageSocket(logger, localID, remoteID)
{
   MESSAGE_SOCKET_LOG_DEBUG(QString("Socket[%1] connecting to %2:%3").arg(this->num).arg(address.toString()).arg(port));

   this->socket->connectToHost(address, port);
}

MessageSocket::~MessageSocket()
{
   this->stopListening();
   // Closing during destruction must not dispatch another lifecycle callback.
   disconnect(this->socket, nullptr, this, nullptr);

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
   if (!this->listening || !this->socket->isOpen())
      return;

   if (type == MessageHeader::NULL_MESS)
   {
      MESSAGE_SOCKET_LOG_ERROR("Cannot send NULL_MESS: invalid wire type; closing the socket");
      this->socket->close();
      return;
   }

   const auto payloadSize = message ? message->ByteSizeLong() : 0;
   if (payloadSize > MAX_MESSAGE_PAYLOAD_SIZE)
   {
      MESSAGE_SOCKET_LOG_ERROR(QString("Outgoing message size too big (%1), size limit is %2 bytes; closing the socket")
         .arg(static_cast<qulonglong>(payloadSize)).arg(MAX_MESSAGE_PAYLOAD_SIZE));
      this->socket->close();
      return;
   }

   MessageHeader header(type, static_cast<quint32>(payloadSize), this->localID);

   MESSAGE_SOCKET_LOG_DEBUG(
      QString("Socket[%1]::send: %2 to %3\n%4")
         .arg(this->num)
         .arg(
            header.toStr(),
            this->remoteID.toStrShort(),
            message ? messageDebugStr(*message, header.getSize()) : "<empty message>"
         )
   );

   // A write error can synchronously disconnect the socket and delete this object.
   const QPointer<MessageSocket> self(this);
   if (Message::writeMessageToDeviceWithCachedSizes(this->socket, header, message) == 0 && self)
   {
      MESSAGE_SOCKET_LOG_ERROR(QString("Unable to write message (type %1); closing the socket").arg(type));
      // A partial frame may already have been queued. Never append another message to it.
      this->socket->close();
   }
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
   connect(this->socket, &QAbstractSocket::readyRead, this, &MessageSocket::dataReceivedSlot, Qt::DirectConnection);

   // A transfer thread may have disconnected and returned the socket before
   // its queued notification runs. Clear that session before reading again.
   const QPointer<MessageSocket> self(this);
   this->disconnectedSlot();
   if (self.isNull() || !this->listening)
      return;

   this->onStartListening();
   if (!self.isNull() && this->listening)
      this->dataReceivedSlot();
}

/**
  * Stop listening the socket.
  * It's useful when some non-message data has to be sent, like a stream of data.
  */
void MessageSocket::stopListening()
{
   MESSAGE_SOCKET_LOG_DEBUG(QString("Socket[%1] stopping to listen").arg(this->num));

   disconnect(this->socket, &QAbstractSocket::readyRead, this, &MessageSocket::dataReceivedSlot);

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
   // A callback can restart listening or trigger readyRead synchronously. Let the
   // active loop finish dispatching this message before reading the next frame.
   if (this->processingData)
      return;

   this->processingData = true;
   // 'onNewDataReceived()', 'onNewMessage(..)' and the signal 'newMessage' may delete this object,
   // for instance by closing the connection. Once it happens no member may be accessed anymore.
   const QPointer<MessageSocket> self(this);
   const auto resetProcessing = qScopeGuard([self] {
      if (self)
         self->processingData = false;
   });

   // A callback may stop listening and hand the socket to another thread.
   while (this->listening && !this->socket->atEnd())
   {
      this->onNewDataReceived();
      if (self.isNull() || !this->listening)
         return;

      if (this->currentHeader.isNull() && this->socket->bytesAvailable() >= MessageHeader::HEADER_SIZE)
      {
         this->currentHeader = MessageHeader::readHeader(*this->socket);

         // NULL_MESS is an internal sentinel, never a valid wire type. Reject it
         // before its payload can be mistaken for the next message header.
         if (this->currentHeader.getType() == MessageHeader::NULL_MESS)
         {
            MESSAGE_SOCKET_LOG_DEBUG(QString("Socket[%1]: Invalid NULL_MESS wire type, closing the socket").arg(this->num));
            this->socket->close();
            return;
         }

         if (this->remoteID.isNull())
            this->remoteID = this->currentHeader.getSenderID();
         if (this->localID.isNull())
            this->localID = this->remoteID;

         if (this->currentHeader.getSenderID() != this->remoteID)
         {
            MESSAGE_SOCKET_LOG_DEBUG(
               QString("Socket[%1]: Peer ID from message (%2) doesn't match the known peer ID (%3)")
                  .arg(this->num)
                  .arg(this->currentHeader.getSenderID().toStrShort(), this->remoteID.toStrShort())
            );
            this->currentHeader.setNull();
            this->socket->close();
            return;
         }

         if (this->currentHeader.getSize() > MAX_MESSAGE_PAYLOAD_SIZE)
         {
            MESSAGE_SOCKET_LOG_DEBUG(
               QString("Socket[%1]: Message size too big (%2), size limit is (%3) bytes")
                  .arg(this->num)
                  .arg(this->currentHeader.getSize())
                  .arg(MAX_MESSAGE_PAYLOAD_SIZE)
            );
            this->currentHeader.setNull();
            this->socket->close();
            return;
         }
      }

      if (!this->currentHeader.isNull() && this->socket->bytesAvailable() >= this->currentHeader.getSize())
      {
         if (!this->readMessage())
         {
            MESSAGE_SOCKET_LOG_DEBUG(
               QString("Socket[%1]: Unable to read the received message, closing the socket. Message type: %2")
                  .arg(this->num)
                  .arg(this->currentHeader.getType())
            );
            this->socket->close();
            return;
         }

         if (self.isNull())
            return;
      }
      else
         return;
   }
}

void MessageSocket::disconnectedSlot()
{
   // startListening() may already have consumed a queued notification.
   if (!this->disconnectPending.exchange(false))
      return;

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
      // The frame has been consumed. Commit that state before invoking callbacks.
      this->currentHeader.setNull();

      MESSAGE_SOCKET_LOG_DEBUG(QString("Socket[%1]: Data received from %2, %3\n%4").arg(
         QString::number(this->num),
         this->socket->peerAddress().toString(),
         message.getHeader().toStr(),
         messageDebugStr(message.getMessage(), message.getHeader().getSize())
      ));

      // 'onNewMessage(..)' may delete this object, see 'dataReceivedSlot()'.
      const QPointer<MessageSocket> self(this);
      if (!this->acceptsMessage(message) || self.isNull())
         return true;
      this->onNewMessage(message);
      if (self.isNull())
         return true;

      emit newMessage(message);
      return true;
   }
   catch (ReadErrorException&)
   {
      return false;
   }
}

#ifdef DEBUG
   int MessageSocket::currentNum(0);
#endif
