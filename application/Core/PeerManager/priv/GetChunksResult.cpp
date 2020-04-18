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

#include <priv/GetChunksResult.h>
using namespace PM;

#include <Common/Settings.h>

#include <priv/Log.h>

GetChunksResult::GetChunksResult(const Protos::Core::GetChunks& chunks, QSharedPointer<PeerMessageSocket> socket) :
   IGetChunksResult(SETTINGS.get<quint32>("socket_timeout")), chunks(chunks), socket(socket), closeTheSocket(false)
{
}

void GetChunksResult::start()
{
   connect(this->socket.data(), &PeerMessageSocket::newMessage, this, &GetChunksResult::newMessage, Qt::DirectConnection);
   socket->send(Common::MessageHeader::CORE_GET_CHUNKS, this->chunks);
   this->startTimer();
}

void GetChunksResult::setStatus(bool closeTheSocket)
{
   this->closeTheSocket = closeTheSocket;
}

void GetChunksResult::doDeleteLater()
{
   // We must disconnect because 'this->socket->finished' can read some data and emit 'newMessage'.
   disconnect(this->socket.data(), &PeerMessageSocket::newMessage, this, &GetChunksResult::newMessage);
   this->socket->finished(this->isTimedout() ? true : this->closeTheSocket);
   this->socket.clear();
   this->deleteLater();
}

void GetChunksResult::newMessage(const Common::Message& message)
{
   if (message.getHeader().getType() != Common::MessageHeader::CORE_GET_CHUNKS_RESULT)
      return;

   this->stopTimer();

   const Protos::Core::GetChunksResult& chunksResult = message.getMessage<Protos::Core::GetChunksResult>();
   emit result(chunksResult);

   if (chunksResult.status() == Protos::Core::GetChunksResult::OK)
   {
      socket->stopListening();
      emit stream(this->socket);
   }
   else
   {
      this->closeTheSocket = true;
      // Segfault, maybe we cannot disconnect a signal during a call to the connected slot (this method)!?.
      //disconnect(this->socket.data(), SIGNAL(newMessage(Common::MessageHeader::MessageType, const google::protobuf::Message&)), this, SLOT(newMessage(Common::MessageHeader::MessageType, const google::protobuf::Message&)));
   }
}
