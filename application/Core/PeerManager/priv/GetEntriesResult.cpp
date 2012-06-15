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
   connect(this->socket.data(), SIGNAL(newMessage(Common::Message&)), this, SLOT(newMessage(Common::Message&)), Qt::DirectConnection);
   socket->send(Common::MessageHeader::CORE_GET_ENTRIES, this->dirs);
   this->startTimer();
}

void GetEntriesResult::doDeleteLater()
{
   disconnect(this->socket.data(), SIGNAL(newMessage(Common::Message&)), this, SLOT(newMessage(Common::Message&)));
   this->socket->finished();
   this->socket.clear();
   this->deleteLater();
}

void GetEntriesResult::newMessage(Common::Message& message)
{
   if (message.getHeader().getType() != Common::MessageHeader::CORE_GET_ENTRIES_RESULT)
      return;

   this->stopTimer();

   disconnect(this->socket.data(), SIGNAL(newMessage(Common::Message&)), this, SLOT(newMessage(Common::Message&)));

   const Protos::Core::GetEntriesResult& entries = message.getMessage<Protos::Core::GetEntriesResult>();
   emit result(entries);
}
