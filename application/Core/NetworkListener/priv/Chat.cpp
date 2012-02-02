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
  
#include <priv/Chat.h>
using namespace NL;

#include <QDateTime>

#include <Common/ProtoHelper.h>
#include <Common/Network/MessageHeader.h>
#include <Common/Settings.h>

#include <priv/UDPListener.h>

/**
  * @class NL::Chat
  *
  * @author mcuony
  * @author gburri
  */

Chat::Chat(UDPListener& uDPListener) :
   uDPListener(uDPListener)
{
   Chat::connect(
      &this->uDPListener,
      SIGNAL(newChatMessage(const Common::Hash&, const Protos::Core::ChatMessage&)),
      this,
      SLOT(newChatMessage(const Common::Hash&, const Protos::Core::ChatMessage&))
   );
}

/**
  * Send a message to all other peers and save it into our list of messages.
  */
void Chat::send(const QString& message)
{
   Protos::Core::ChatMessage chatMessage;
   Common::ProtoHelper::setStr(chatMessage, &Protos::Core::ChatMessage::set_message, message);

   this->uDPListener.send(Common::MessageHeader::CORE_CHAT_MESSAGE, chatMessage);

   Protos::GUI::EventChatMessages_Message eventMessage;
   eventMessage.mutable_peer_id()->set_hash(this->uDPListener.getOwnID().getData(), Common::Hash::HASH_SIZE);
   eventMessage.set_time(QDateTime::currentMSecsSinceEpoch());
   eventMessage.set_message(chatMessage.message());
   this->messagesHistory.append(eventMessage);
}

Protos::GUI::EventChatMessages Chat::getLastMessages() const
{
   Protos::GUI::EventChatMessages eventMessages;
   eventMessages.mutable_message()->Reserve(this->messagesHistory.size());

   for (QLinkedListIterator<Protos::GUI::EventChatMessages_Message> i(this->messagesHistory); i.hasNext();)
      eventMessages.mutable_message()->Add()->CopyFrom(i.next());
   return eventMessages;
}

void Chat::newChatMessage(const Common::Hash& peerID, const Protos::Core::ChatMessage& message)
{
   Protos::GUI::EventChatMessages_Message eventMessage;
   eventMessage.mutable_peer_id()->set_hash(peerID.getData(), Common::Hash::HASH_SIZE);
   eventMessage.set_time(QDateTime::currentMSecsSinceEpoch());
   eventMessage.set_message(message.message());

   emit newMessage(eventMessage);

   this->messagesHistory.append(eventMessage);
   if (static_cast<quint32>(this->messagesHistory.size()) > SETTINGS.get<quint32>("max_number_of_chat_message_saved"))
      this->messagesHistory.removeFirst();
}
