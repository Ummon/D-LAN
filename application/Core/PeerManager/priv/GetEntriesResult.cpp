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

#include <priv/Log.h>

GetEntriesResult::GetEntriesResult(const Protos::Core::GetEntries& dirs, QSharedPointer<PeerMessageSocket> socket) :
   IGetEntriesResult(SETTINGS.get<quint32>("socket_timeout")), dirs(dirs), socket(socket)
{
}

void GetEntriesResult::start()
{
   if (this->started)
      return;
   this->started = true;
   this->pending = true;
   this->startTimer();
   if (!this->socket.isNull())
   {
      connect(this->socket.data(), &PeerMessageSocket::newMessage, this, &GetEntriesResult::newMessage, Qt::DirectConnection);
      socket->send(Common::MessageHeader::CORE_GET_ENTRIES, this->dirs);
   }
}

void GetEntriesResult::doDeleteLater()
{
   this->stopTimer();
   if (!this->socket.isNull())
   {
      disconnect(this->socket.data(), &PeerMessageSocket::newMessage, this, &GetEntriesResult::newMessage);
      // An unfinished response has no request ID and cannot be reused by another request.
      this->socket->finished(this->pending);
      this->socket.clear();
   }
   this->deleteLater();
}

void GetEntriesResult::newMessage(const Common::Message& message)
{
   if (message.getHeader().getType() != Common::MessageHeader::CORE_GET_ENTRIES_RESULT)
      return;

   this->stopTimer();
   this->pending = false;

   if (!this->socket.isNull())
      disconnect(this->socket.data(), &PeerMessageSocket::newMessage, this, &GetEntriesResult::newMessage);

   // PeerMessageSocket has already finished the transaction. Drop ownership before
   // notifying callers, which may immediately start another request on this socket.
   this->socket.clear();

   const Protos::Core::GetEntriesResult& entries = message.getMessage<Protos::Core::GetEntriesResult>();
   emit result(entries);
}
