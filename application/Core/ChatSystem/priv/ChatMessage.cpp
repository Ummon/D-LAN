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
#include <QTimeZone>

#include <priv/ChatMessage.h>
using namespace CS;

#include <Common/ProtoHelper.h>

ChatMessage::ChatMessage(
   const QString& message,
   const Common::Hash& ownerID,
   const QString& ownerNick,
   const QString& roomName,
   const QList<Common::Hash>& peerIDsAnswer
) :
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
   message(QString::fromStdString(chatMessage.message())),
   ownerID(chatMessage.peer_id().hash()),
   peerIDsAnswer(ChatMessage::getPeerIDsAnswer(chatMessage)),
   time(chatMessage.time() > 0 ? QDateTime::fromMSecsSinceEpoch(chatMessage.time(), QTimeZone::UTC) : QDateTime::currentDateTimeUtc()),
   ownerNick(QString::fromStdString(chatMessage.peer_nick())),
   room(QString::fromStdString(chatMessage.chat_room()))
{
}

QList<Common::Hash> ChatMessage::getPeerIDsAnswer(const Protos::Common::ChatMessage& chatMessage)
{
   QList<Common::Hash> result;
   result.reserve(chatMessage.peer_ids_answer_size());
   for (int i = 0; i < chatMessage.peer_ids_answer_size(); i++)
      result << Common::Hash(chatMessage.peer_ids_answer(i).hash());
   return result;
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
   protoChatMessage.set_message(this->message.toStdString());
   protoChatMessage.set_time(this->time.toMSecsSinceEpoch());
   protoChatMessage.mutable_peer_id()->set_hash(this->ownerID.getData(), Common::Hash::HASH_SIZE);
   protoChatMessage.set_peer_nick(this->ownerNick.toStdString());
   if (!this->room.isEmpty())
      protoChatMessage.set_chat_room(this->room.toStdString());
   for (QListIterator<Common::Hash> i(this->peerIDsAnswer); i.hasNext();)
      protoChatMessage.add_peer_ids_answer()->set_hash(i.next().getData(), Common::Hash::HASH_SIZE);
}

