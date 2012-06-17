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
  
#ifndef CHATSYSTEM_CHATMESSAGES_H
#define CHATSYSTEM_CHATMESSAGES_H

#include <limits>

#include <QString>
#include <QList>
#include <QSet>
#include <QSharedPointer>

#include <Protos/common.pb.h>
#include <Protos/core_protocol.pb.h>

#include <Common/Hash.h>

#include <priv/ChatMessage.h>

namespace CS
{
   class ChatMessages
   {
   public:
      ChatMessages();

      QSharedPointer<ChatMessage> add(const QString& message, const Common::Hash& ownerID, const QString& ownerNick);
      QList<QSharedPointer<ChatMessage>> add(const Protos::Common::ChatMessages& chatMessages);

      QList<quint64> getLastMessageIDs(int n) const;

      QList<QSharedPointer<ChatMessage>> getUnknownMessages(const Protos::Core::GetLastChatMessages& getLastChatMessage);

      void fillProtoChatMessages(Protos::Common::ChatMessages& chatMessages, int number = std::numeric_limits<int>::max()) const;
      static void fillProtoChatMessages(Protos::Common::ChatMessages& chatMessages, const QList<QSharedPointer<ChatMessage>>& messages);

      void loadFromFile();
      void saveToFile() const;

   private:
      QList<QSharedPointer<ChatMessage>> insert(const QList<QSharedPointer<ChatMessage>>& messages);

      const int MAX_NUMBER_OF_STORED_CHAT_MESSAGES;

      mutable bool changed;

      QList<QSharedPointer<ChatMessage>> messages;
      QSet<quint64> messageIDs;
   };
}

#endif
