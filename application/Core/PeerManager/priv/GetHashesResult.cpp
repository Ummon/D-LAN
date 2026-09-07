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
  
#include <priv/GetHashesResult.h>
using namespace PM;

#include <Common/Settings.h>

#include <priv/Log.h>

GetHashesResult::GetHashesResult(const Protos::Common::Entry& file, QSharedPointer<PeerMessageSocket> socket) :
   IGetHashesResult(SETTINGS.get<quint32>("get_hashes_timeout")), file(file), socket(socket)
{
}

void GetHashesResult::start()
{
   if (this->started)
      return;
   this->started = true;
   this->pending = true;
   this->startTimer();
   // The socket may be null if the connection pool was unable to give one, in this case the request will simply time out.
   if (!this->socket.isNull())
   {
      Protos::Core::GetHashes message;
      message.mutable_file()->CopyFrom(this->file);
      connect(this->socket.data(), &PeerMessageSocket::newMessage, this, &GetHashesResult::newMessage, Qt::DirectConnection);
      socket->send(Common::MessageHeader::CORE_GET_HASHES, message);
   }
}

void GetHashesResult::doDeleteLater()
{
   this->stopTimer();
   if (!this->socket.isNull())
   {
      disconnect(this->socket.data(), &PeerMessageSocket::newMessage, this, &GetHashesResult::newMessage);
      // Abandoning a hash stream must not expose its remaining replies to a new request.
      this->socket->finished(this->pending);
      this->socket.clear();
   }
   this->deleteLater();
}

void GetHashesResult::newMessage(const Common::Message& message)
{
   switch (message.getHeader().getType())
   {
   case Common::MessageHeader::CORE_GET_HASHES_RESULT:
      {
         const Protos::Core::GetHashesResult& hashesResult = message.getMessage<Protos::Core::GetHashesResult>();
         this->remainingHashes = hashesResult.nb_hash();
         if (hashesResult.status() != Protos::Core::GetHashesResult::OK || this->remainingHashes == 0)
            this->complete();
         else
            this->startTimer();
         emit result(hashesResult);
      }
      break;

   case Common::MessageHeader::CORE_HASH_RESULT:
      {
         const Protos::Core::HashResult& hashResult = message.getMessage<Protos::Core::HashResult>();
         if (this->remainingHashes == 0)
            return;
         if (--this->remainingHashes == 0)
            this->complete();
         else
            this->startTimer();
         emit nextHash(hashResult);
      }
      break;

   default:;
   }
}

void GetHashesResult::complete()
{
   this->pending = false;
   this->stopTimer();
   if (this->socket)
      disconnect(this->socket.data(), &PeerMessageSocket::newMessage, this, &GetHashesResult::newMessage);
   // PeerMessageSocket has already marked the socket idle. An old result must not
   // finish it again after a caller has started the next transaction.
   this->socket.clear();
}
