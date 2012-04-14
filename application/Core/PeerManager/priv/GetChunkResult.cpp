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
  
#include <priv/GetChunkResult.h>
using namespace PM;

#include <Common/Settings.h>

#include <priv/Log.h>

GetChunkResult::GetChunkResult(const Protos::Core::GetChunk& chunk, QSharedPointer<PeerMessageSocket> socket) :
   IGetChunkResult(SETTINGS.get<quint32>("socket_timeout")), chunk(chunk), socket(socket), closeTheSocket(false)
{
}

void GetChunkResult::start()
{
   connect(this->socket.data(), SIGNAL(newMessage(Common::MessageHeader::MessageType, const google::protobuf::Message&)), this, SLOT(newMessage(Common::MessageHeader::MessageType, const google::protobuf::Message&)), Qt::DirectConnection);
   socket->send(Common::MessageHeader::CORE_GET_CHUNK, this->chunk);
   this->startTimer();
}

void GetChunkResult::setStatus(bool closeTheSocket)
{
   this->closeTheSocket = closeTheSocket;
}

void GetChunkResult::doDeleteLater()
{
   // We must disconnect because 'this->socket->finished' can read some data and emit 'newMessage'.
   disconnect(this->socket.data(), SIGNAL(newMessage(Common::MessageHeader::MessageType, const google::protobuf::Message&)), this, SLOT(newMessage(Common::MessageHeader::MessageType, const google::protobuf::Message&)));
   this->socket->finished(this->isTimedout() ? true : this->closeTheSocket);
   this->socket.clear();
   this->deleteLater();
}

void GetChunkResult::newMessage(Common::MessageHeader::MessageType type, const google::protobuf::Message& message)
{
   if (type != Common::MessageHeader::CORE_GET_CHUNK_RESULT)
      return;

   this->stopTimer();

   const Protos::Core::GetChunkResult& chunkResult = dynamic_cast<const Protos::Core::GetChunkResult&>(message);
   emit result(chunkResult);

   if (chunkResult.status() == Protos::Core::GetChunkResult::OK)
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
