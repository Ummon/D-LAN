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

#include <QRandomGenerator64>

#include <priv/ChatMessage.h>
using namespace CS;

#include <Common/ProtoHelper.h>

ChatMessage::ChatMessage(const QString& message, const Common::Hash& ownerID, const QString& ownerNick, const QString& roomName, const QList<Common::Hash>& peerIDsAnswer) :
   ID(QRandomGenerator64::global()->generate64()),
   message(message),
   ownerID(ownerID),
   peerIDsAnswer(peerIDsAnswer),
   time(QDateTime::currentDateTimeUtc()),
   ownerNick(ownerNick),
   room(roomName)
{
}

ChatMessage::ChatMessage(const Protos::Common::ChatMessage& chatMessage) :
   ID(chatMessage.id()),
   message(Common::ProtoHelper::getStr(chatMessage, &Protos::Common::ChatMessage::message)),
   ownerID(chatMessage.peer_id().hash()),
   time(chatMessage.time() > 0 ? QDateTime::fromMSecsSinceEpoch(chatMessage.time()) : QDateTime::currentDateTimeUtc()),
   ownerNick(Common::ProtoHelper::getStr(chatMessage, &Protos::Common::ChatMessage::peer_nick)),
   room(Common::ProtoHelper::getStr(chatMessage, &Protos::Common::ChatMessage::chat_room))
{
}

quint64 ChatMessage::getID() const
{
   return this->ID;
}

QDateTime ChatMessage::getTime() const
{
   return this->time;
}

void ChatMessage::fillProtoChatMessage(Protos::Common::ChatMessage& protoChatMessage) const
{
   protoChatMessage.set_id(this->ID);
   Common::ProtoHelper::setStr(protoChatMessage, &Protos::Common::ChatMessage::set_message, this->message);
   protoChatMessage.set_time(this->time.toMSecsSinceEpoch());
   protoChatMessage.mutable_peer_id()->set_hash(this->ownerID.getData(), Common::Hash::HASH_SIZE);
   Common::ProtoHelper::setStr(protoChatMessage, &Protos::Common::ChatMessage::set_peer_nick, this->ownerNick);
   if (!this->room.isEmpty())
      Common::ProtoHelper::setStr(protoChatMessage, &Protos::Common::ChatMessage::set_chat_room, this->room);
   for (QListIterator<Common::Hash> i(this->peerIDsAnswer); i.hasNext();)
      protoChatMessage.add_peer_ids_answer()->set_hash(i.next().getData(), Common::Hash::HASH_SIZE);
}

