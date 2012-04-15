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
  
#ifndef COMMON_MESSAGE_SOCKET_H
#define COMMON_MESSAGE_SOCKET_H

#include <QString>
#include <QTcpSocket>
#include <QAbstractSocket>
#include <QHostAddress>
#include <QTimer>

#include <google/protobuf/message.h>

#include <Common/Network/MessageHeader.h>
#include <Common/Hash.h>
#include <Common/Uncopyable.h>

#ifdef DEBUG
   #define MESSAGE_SOCKET_LOG_DEBUG(mess) this->logger->logDebug(mess)
#else
   #define MESSAGE_SOCKET_LOG_DEBUG(mess)
#endif
#define MESSAGE_SOCKET_LOG_ERROR(mess) this->logger->logError(mess)

namespace Common
{
   class MessageSocket : public QObject, Uncopyable
   {
      Q_OBJECT
   protected:
      class ILogger
      {
      public:
         virtual ~ILogger() {}
         virtual void logDebug(const QString& message) = 0;
         virtual void logError(const QString& message) = 0;
      };

      MessageSocket(ILogger* logger, const Hash& localID = Hash(), const Hash& remoteID = Hash());
      MessageSocket(ILogger* logger, QAbstractSocket* socket, const Hash& localID = Hash(), const Hash& remoteID = Hash());
      MessageSocket(ILogger* logger, const QHostAddress& address, quint16 port, const Hash& localID = Hash(), const Hash& remoteID = Hash());

   public:
      virtual ~MessageSocket();

   public:
      virtual Hash getLocalID() const;
      virtual Hash getRemoteID() const;

      virtual void send(MessageHeader::MessageType type, const google::protobuf::Message& message);
      virtual void send(MessageHeader::MessageType type);
   private:
      virtual void send(MessageHeader::MessageType type, const google::protobuf::Message* message);

   public:
      virtual void startListening();
      virtual void stopListening();

      virtual bool isLocal() const;
      virtual bool isConnected() const;

      virtual void close();   signals:

   signals:
      /**
        * Emitted after a message is received. The method 'onNewMessage()' is called previously.
        */
      void newMessage(Common::MessageHeader::MessageType type, const google::protobuf::Message& message);

   protected:
      bool isListening() const;

   private slots:
      void dataReceivedSlot();
      void disconnectedSlot();

   private:
      /**
        * Call when a new message arrived. Do nothing by default.
        * Can be inherited by a subclass of 'MessageSocket'.
        * The signal 'newMessage' is also emitted after this called.
        */
      virtual void onNewMessage(MessageHeader::MessageType type, const google::protobuf::Message& message) {}
      virtual void onNewDataReceived() {}
      virtual void onDisconnected() {}

      bool readMessage();
      template<typename MessT> bool readMessage();
      bool readProtoMessage(google::protobuf::Message& message);

      ILogger* logger;

   protected:
      QAbstractSocket* socket;

   private:
      Hash localID; // ID of the local peer.
      Hash remoteID; // ID of the remote peer.

      const bool localIDDefined;
      const bool remoteIDDefined;

      bool listening;

      MessageHeader currentHeader;

#ifdef DEBUG
      // To identify the sockets in debug mode.
   protected:
      int num;
   private:
      static int currentNum;
#endif
   };
}

/***** Definitions *****/
using namespace Common;

template<typename MessT> bool MessageSocket::readMessage()
{
   MessT mess;
   if (this->readProtoMessage(mess))
   {
      this->onNewMessage(this->currentHeader.getType(), mess);
      emit newMessage(this->currentHeader.getType(), mess);
      return true;
   }
   return false;
}

#endif
