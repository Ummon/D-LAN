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
   Protos::Core::GetHashes message;
   message.mutable_file()->CopyFrom(this->file);
   connect(this->socket.data(), SIGNAL(newMessage(Common::Message&)), this, SLOT(newMessage(Common::Message&)), Qt::DirectConnection);
   socket->send(Common::MessageHeader::CORE_GET_HASHES, message);
   this->startTimer();
}

void GetHashesResult::doDeleteLater()
{
   disconnect(this->socket.data(), SIGNAL(newMessage(Common::Message&)), this, SLOT(newMessage(Common::Message&)));
   this->socket->finished();
   this->socket.clear();
   this->deleteLater();
}

void GetHashesResult::newMessage(Common::Message& message)
{
   switch (message.getHeader().getType())
   {
   case Common::MessageHeader::CORE_GET_HASHES_RESULT:
      {
         const Protos::Core::GetHashesResult& hashesResult = message.getMessage<Protos::Core::GetHashesResult>();
         this->startTimer(); // Restart the timer.
         emit result(hashesResult);
      }
      break;

   case Common::MessageHeader::CORE_HASH:
      {
         const Protos::Common::Hash& hash = message.getMessage<Protos::Common::Hash>();
         this->startTimer(); // Restart the timer.
         emit nextHash(Common::Hash(hash.hash()));
      }
      break;

   default:;
   }
}
