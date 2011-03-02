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
   #define MESSAGE_SOCKET_LOG_DEBUG(mess) this->logDebug(mess)
#else
   #define MESSAGE_SOCKET_LOG_DEBUG(mess)
#endif
#define MESSAGE_SOCKET_LOG_ERROR(mess) this->logError(mess)

namespace Common
{
   class MessageSocket : public QObject, Uncopyable
   {
      Q_OBJECT
   protected:
      MessageSocket();

   public:
      virtual ~MessageSocket();

   protected:
      void init(const Hash& ID = Hash(), const Hash& remoteID = Hash());
      void init(QAbstractSocket* socket, const Hash& ID = Hash(), const Hash& remoteID = Hash());
      void init(const QHostAddress& address, quint16 port, const Hash& ID = Hash(), const Hash& remoteID = Hash());

   public:

      virtual QAbstractSocket* getQSocket() const;
      virtual Hash getID() const;
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

   signals:
      /**
        * Emitted after a message is received. The method 'onNewMessage()' is called previously.
        */
      void newMessage(Common::MessageHeader::MessageType type, const google::protobuf::Message& message);

   protected:

      /**
        * Call when a new message arrived. Do nothing by default.
        * The signal 'newMessage' is also emitted after this called.
        */
      virtual void onNewMessage(MessageHeader::MessageType type, const google::protobuf::Message& message) {};

      virtual void onNewDataReceived() {};

      /**
        * Do nothing if not redefined.
        */
      virtual void logDebug(const QString& message) {};

      /**
        * Do nothing if not redefined.
        */
      virtual void logError(const QString& message) {};

      bool isListening() const;

   protected slots:
      virtual void dataReceived();
      virtual void disconnected();

   private:
      bool readMessage();
      template<typename MessT> bool readMessage();
      bool readProtoMessage(google::protobuf::Message& message);

      QAbstractSocket* socket;

      Hash ID;
      Hash remoteID;

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
