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

GetChunksResult::GetChunksResult(const Protos::Core::GetChunks& chunks, QSharedPointer<PeerMessageSocket> socket) :
   Result(SETTINGS.get<quint32>("socket_timeout"), Common::MessageHeader::CORE_GET_CHUNKS, chunks, socket)
{
}

void GetChunksResult::setStatus(bool closeTheSocket)
{
   // A successful status is meaningful only after the stream has been handed off.
   // Cancellation before that point must close even if the caller reports no error.
   this->socketReusable = this->streaming && !closeTheSocket;
}

void GetChunksResult::newMessage(const Common::Message& message)
{
   if (message.getHeader().getType() != Common::MessageHeader::CORE_GET_CHUNKS_RESULT)
      return;

   this->stopTimer();

   const Protos::Core::GetChunksResult& chunksResult = message.getMessage<Protos::Core::GetChunksResult>();
   const bool success = chunksResult.status() == Protos::Core::GetChunksResult::OK;
   // The raw-stream boundary is established before notifying the receiver, which may release this result synchronously.
   if (success)
      this->socket->stopListening();

   emit result(chunksResult);

   if (success && !this->socket.isNull())
   {
      this->streaming = true;
      emit stream(this->socket);
   }
}

/**
  * Unlike the other results, a closed socket doesn't time out the request at once: 'DM::ChunkDownloader' asks
  * again right after a timeout, the wait is the only thing which paces its retries.
  */
void GetChunksResult::socketClosed()
{
}
