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

#pragma once

#include <QSharedPointer>

#include <Common/Uncopyable.h>
#include <Common/Network/Message.h>
#include <Common/Network/MessageHeader.h>

#include <priv/PeerMessageSocket.h>

namespace PM
{
   /**
     * The part common to the results of the requests sent to a peer, see 'IPeer'.
     * 'Interface' is the public interface of the result and 'Request' the type of the message sent by 'start()'.
     *
     * Once started, the socket belongs to the result until the whole answer has been received. A result deleted
     * before that closes its socket: what remains of the answer couldn't be told apart from the answer to the
     * next request.
     */
   template <typename Interface, typename Request>
   class Result : public Interface, Common::Uncopyable
   {
   protected:
      /**
        * @param socket May be null if the connection pool was unable to give one, the request will then simply time out.
        */
      Result(
         int timeout,
         Common::MessageHeader::MessageType type,
         const Request& request,
         const QSharedPointer<PeerMessageSocket>& socket
      ) :
         Interface(timeout), socket(socket), type(type), request(request)
      {
      }

   public:
      void start() override
      {
         if (this->started)
            return;
         this->started = true;

         this->startTimer();
         if (this->socket.isNull())
            return;

         QObject::connect(
            this->socket.data(),
            &PeerMessageSocket::newMessage,
            this,
            [this](const Common::Message& message) { this->newMessage(message); },
            Qt::DirectConnection
         );
         // Queued: the socket may be closed from within 'send(..)' and the caller doesn't expect a timeout from 'start()'.
         QObject::connect(
            this->socket.data(), &PeerMessageSocket::closed, this, [this] { this->socketClosed(); }, Qt::QueuedConnection
         );
         this->socket->send(this->type, this->request);
      }

      void doDeleteLater() override
      {
         this->stopTimer();
         if (!this->socket.isNull())
         {
            // We must disconnect because 'finished(..)' can read some data and emit 'newMessage'.
            QObject::disconnect(this->socket.data(), nullptr, this, nullptr);
            this->socket->finished(this->started && !this->socketReusable);
            this->socket.clear();
         }
         this->deleteLater();
      }

   protected:
      virtual void newMessage(const Common::Message& message) = 0;

      /**
        * The answer will never come if the socket is closed, there is no need to wait for the timer.
        */
      virtual void socketClosed()
      {
         this->timeoutNow();
      }

      /**
        * To be called once the whole answer has been received, if the socket has ended the transaction by itself,
        * see 'PeerMessageSocket::onNewMessage(..)': it may then be given to another request at any time.
        */
      void releaseSocket()
      {
         this->stopTimer();
         QObject::disconnect(this->socket.data(), nullptr, this, nullptr);
         this->socket.clear();
      }

      QSharedPointer<PeerMessageSocket> socket;

      /**
        * To be set if the socket is still owned once the whole answer has been received.
        */
      bool socketReusable = false;

   private:
      const Common::MessageHeader::MessageType type;
      const Request request;
      bool started = false;
   };
}
