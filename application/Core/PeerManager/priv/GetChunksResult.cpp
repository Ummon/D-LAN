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
   if (this->state != State::NotStarted)
      return;
   this->state = State::AwaitingResponse;
   this->startTimer();
   // The socket may be null if the connection pool was unable to give one, in this case the request will simply time out.
   if (!this->socket.isNull())
   {
      connect(this->socket.data(), &PeerMessageSocket::newMessage, this, &GetChunksResult::newMessage, Qt::DirectConnection);
      socket->send(Common::MessageHeader::CORE_GET_CHUNKS, this->chunks);
   }
}

void GetChunksResult::setStatus(bool closeTheSocket)
{
   this->closeTheSocket = closeTheSocket;
   // A successful status is meaningful only after the stream has been handed off.
   // Cancellation before that point must close even if the caller reports no error.
   if (!closeTheSocket && this->state == State::Streaming)
      this->state = State::Complete;
}

void GetChunksResult::doDeleteLater()
{
   this->stopTimer();
   if (!this->socket.isNull())
   {
      // We must disconnect because 'this->socket->finished' can read some data and emit 'newMessage'.
      disconnect(this->socket.data(), &PeerMessageSocket::newMessage, this, &GetChunksResult::newMessage);
      const bool unfinished = this->state != State::NotStarted && this->state != State::Complete;
      this->socket->finished(unfinished || this->isTimedout() || this->closeTheSocket);
      this->socket.clear();
   }
   this->deleteLater();
}

void GetChunksResult::newMessage(const Common::Message& message)
{
   if (message.getHeader().getType() != Common::MessageHeader::CORE_GET_CHUNKS_RESULT)
      return;

   this->stopTimer();

   const Protos::Core::GetChunksResult& chunksResult = message.getMessage<Protos::Core::GetChunksResult>();
   const bool success = chunksResult.status() == Protos::Core::GetChunksResult::OK;
   this->state = State::AwaitingStream;
   if (!success)
      this->closeTheSocket = true;
   else if (this->socket)
      this->socket->stopListening();

   // The receiver may release this result synchronously. Establish the raw-stream
   // boundary and cancellation state before invoking it.
   emit result(chunksResult);

   if (this->socket && success)
   {
      this->state = State::Streaming;
      emit stream(this->socket);
   }
}
