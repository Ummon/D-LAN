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
  
#pragma once

#include <limits>

#include <QString>
#include <QList>
#include <QSet>
#include <QSharedPointer>
#include <QSharedDataPointer>
#include <QSharedData>

#include <Protos/common.pb.h>
#include <Protos/core_protocol.pb.h>

#include <Common/Global.h>
#include <Common/Hash.h>

#include <priv/ChatMessage.h>

namespace CS
{
   class ChatMessages
   {
   public:
      const static Common::Global::DataFolderType FOLDER_TYPE_MESSAGES_SAVED = Common::Global::DataFolderType::LOCAL;

      ChatMessages();
      ChatMessages(const ChatMessages& other);

      QSharedPointer<ChatMessage> add(const QString& message, const Common::Hash& ownerID, const QString& ownerNick, const QString& roomName = QString(), const QList<Common::Hash>& peerIDsAnswer = QList<Common::Hash>());
      QList<QSharedPointer<ChatMessage>> add(const Protos::Common::ChatMessages& chatMessages);

      QList<quint64> getLastMessageIDs(int nMax) const;

      QList<QSharedPointer<ChatMessage>> getMessages() const;
      QList<QSharedPointer<ChatMessage>> getUnknownMessages(const Protos::Core::GetLastChatMessages& getLastChatMessage) const;

      void fillProtoChatMessages(Protos::Common::ChatMessages& chatMessages, int number = std::numeric_limits<int>::max()) const;
      static QList<QSharedPointer<ChatMessage>> fillProtoChatMessages(Protos::Common::ChatMessages& chatMessages, const QList<QSharedPointer<ChatMessage>>& messages, int maxByteSize = std::numeric_limits<int>::max());

      void loadFromFile(const QString& filename);
      void saveToFile(const QString& filename) const;

   private:
      QList<QSharedPointer<ChatMessage>> insert(const QList<QSharedPointer<ChatMessage>>& messages);

      struct ChatMessagesData : public QSharedData
      {
         mutable bool changed = false;
         QList<QSharedPointer<ChatMessage>> messages;
         QSet<quint64> messageIDs;
      };

      QSharedDataPointer<ChatMessagesData> d;
   };
}
