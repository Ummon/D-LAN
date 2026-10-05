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

GetHashesResult::GetHashesResult(const Protos::Core::GetHashes& request, QSharedPointer<PeerMessageSocket> socket) :
   Result(SETTINGS.get<quint32>("get_hashes_timeout"), Common::MessageHeader::CORE_GET_HASHES, request, socket)
{
}

void GetHashesResult::newMessage(const Common::Message& message)
{
   const Common::MessageHeader::MessageType type = message.getHeader().getType();
   if (type != Common::MessageHeader::CORE_GET_HASHES_RESULT && type != Common::MessageHeader::CORE_HASH_RESULT)
      return;

   // The socket counts the hashes: if this message is the last one, it has already ended the transaction and
   // isn't active anymore, see 'PeerMessageSocket::onNewMessage(..)'.
   // The socket is released before notifying the caller, which may immediately start another request on it.
   if (this->socket->isActive())
      this->startTimer();
   else
      this->releaseSocket();

   if (type == Common::MessageHeader::CORE_GET_HASHES_RESULT)
      emit result(message.getMessage<Protos::Core::GetHashesResult>());
   else
      emit nextHash(message.getMessage<Protos::Core::HashResult>());
}
