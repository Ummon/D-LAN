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

#include <priv/GetEntriesResult.h>
using namespace PM;

#include <Common/Settings.h>

GetEntriesResult::GetEntriesResult(const Protos::Core::GetEntries& dirs, QSharedPointer<PeerMessageSocket> socket) :
   Result(SETTINGS.get<quint32>("socket_timeout"), Common::MessageHeader::CORE_GET_ENTRIES, dirs, socket)
{
}

void GetEntriesResult::newMessage(const Common::Message& message)
{
   if (message.getHeader().getType() != Common::MessageHeader::CORE_GET_ENTRIES_RESULT)
      return;

   // Before notifying the caller, which may immediately start another request on this socket.
   this->releaseSocket();

   emit result(message.getMessage<Protos::Core::GetEntriesResult>());
}
