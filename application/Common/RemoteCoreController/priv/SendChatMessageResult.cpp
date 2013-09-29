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
  
#include <priv/SendChatMessageResult.h>
using namespace RCC;

#include <Protos/gui_protocol.pb.h>

#include <Common/ProtoHelper.h>

#include <priv/InternalCoreConnection.h>

SendChatMessageResult::SendChatMessageResult(InternalCoreConnection* coreConnection, int socketTimeout, const QString& message, const QString& roomName, const QList<Common::Hash>& peerIDsAnswered) :
   ISendChatMessageResult(socketTimeout), coreConnection(coreConnection), message(message), roomName(roomName), peerIDsAnswered(peerIDsAnswered)
{
}

void SendChatMessageResult::start()
{
   Protos::GUI::ChatMessage chatMessage;
   Common::ProtoHelper::setStr(chatMessage, &Protos::GUI::ChatMessage::set_message, this->message);

   if (!this->roomName.isEmpty())
      Common::ProtoHelper::setStr(chatMessage, &Protos::GUI::ChatMessage::set_room, this->roomName);

   for (QListIterator<Common::Hash> i(this->peerIDsAnswered); i.hasNext();)
      chatMessage.add_peer_ids_answer()->set_hash(i.next().getData(), Common::Hash::HASH_SIZE);

   this->coreConnection->send(Common::MessageHeader::GUI_CHAT_MESSAGE, chatMessage);
   this->startTimer();
}

void SendChatMessageResult::setResult(const Protos::GUI::ChatMessageResult& chatMessageResult)
{
   this->stopTimer();
   emit result(chatMessageResult);
}
