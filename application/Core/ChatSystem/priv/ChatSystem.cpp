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
  
#include <priv/ChatSystem.h>
using namespace CS;

#include <QDateTime>

#include <Libs/MersenneTwister.h>

#include <Protos/common.pb.h>
#include <Protos/core_protocol.pb.h>

#include <Common/ProtoHelper.h>
#include <Common/Network/Message.h>
#include <Common/Settings.h>

#include <Core/PeerManager/IPeer.h>

#include <Core/NetworkListener/INetworkListener.h>

/**
  * @class CM::ChatSystem
  *
  */

ChatSystem::ChatSystem(QSharedPointer<PM::IPeerManager> peerManager, QSharedPointer<NL::INetworkListener> networkListener) :
   peerManager(peerManager),
   networkListener(networkListener)
{
   connect(this->networkListener.data(), SIGNAL(received(const Common::Message&)), this, SLOT(received(const Common::Message&)));

   connect(&this->getLastChatMessageTimer, SIGNAL(timeout()), this, SLOT(getLastChatMessage()));
   this->getLastChatMessageTimer.setInterval(SETTINGS.get<quint32>("get_last_chat_messages_period"));
   this->getLastChatMessageTimer.start();
   this->getLastChatMessage();
}

/**
  * Send a message to all other peers and save it into our list of messages.
  */
void ChatSystem::send(const QString& message)
{
   QSharedPointer<ChatMessage> chatMessage = this->messages.add(message, this->peerManager->getSelf()->getID());

   Protos::Common::ChatMessages protoChatMessages;
   Protos::Common::ChatMessage* protochatMessage = protoChatMessages.add_message();
   chatMessage->fillProtoChatMessage(*protochatMessage);
   protochatMessage->clear_time(); // We let the receive set the time.

   this->networkListener->send(Common::MessageHeader::CORE_CHAT_MESSAGES, protoChatMessages);

   emit newMessages(QList<QSharedPointer<IChatMessage>> { chatMessage });
}

void ChatSystem::received(const Common::Message& message)
{
   switch (message.getHeader().getType())
   {
   case Common::MessageHeader::CORE_CHAT_MESSAGES:
      {
         Protos::Common::ChatMessages chatMessages = message.getMessage<Protos::Common::ChatMessages>();
         for (int i = 0; i < chatMessages.message_size(); i++)
            if (!chatMessages.message(i).has_peer_id())
               chatMessages.mutable_message(i)->mutable_peer_id()->set_hash(message.getHeader().getSenderID().getData(), Common::Hash::HASH_SIZE);
         const QList<QSharedPointer<ChatMessage>>& messages = this->messages.add(chatMessages);
         emit newMessages(reinterpret_cast<const QList<QSharedPointer<IChatMessage>>&>(messages));
      }
      break;

   case Common::MessageHeader::CORE_GET_LAST_CHAT_MESSAGES:
      {
         const Protos::Core::GetLastChatMessages& getLastChatMessage = message.getMessage<Protos::Core::GetLastChatMessages>();
         const QList<QSharedPointer<ChatMessage>>& messages = this->messages.getUnknownMessage(getLastChatMessage);
         if (!messages.isEmpty())
         {
            Protos::Common::ChatMessages chatMessages;
            ChatMessages::fillProtoChatMessages(chatMessages, messages);
            this->networkListener->send(MessageHeader::CORE_CHAT_MESSAGES, chatMessages, message.getHeader().getSenderID());
         }
      }
      break;

   default:; // Ignore all other messages.
   }
}

/**
  * Ask to a random peer its last messages.
  */
void ChatSystem::getLastChatMessage()
{
   static const quint32 N = SETTINGS.get<quint32>("number_of_chat_messages_to_retrieve");

   const QList<quint64>& messageIDs = this->messages.getLastMessageIDs(N);
   Protos::Core::GetLastChatMessages getLastChatMessage;
   getLastChatMessage.set_number(N);
   for (QListIterator<quint64> i(messageIDs); i.hasNext();)
      getLastChatMessage.add_message_id(i.next());

   const QList<PM::IPeer*>& peers = this->peerManager->getPeers();
   this->networkListener->send(Common::MessageHeader::CORE_GET_LAST_CHAT_MESSAGES, getLastChatMessage, peers[this->mtrand.randInt(peers.size() - 1)]->getID());
}
