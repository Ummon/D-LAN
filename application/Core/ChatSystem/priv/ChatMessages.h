#ifndef CHATSYSTEM_CHATMESSAGES_H
#define CHATSYSTEM_CHATMESSAGES_H

#include <QString>
#include <QList>
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

      QSharedPointer<ChatMessage> add(const QString& message, const Common::Hash& ownerID);
      QList<QSharedPointer<ChatMessage>>  add(const Protos::Common::ChatMessages& chatMessages);

      QList<quint64> getLastMessageIDs(int n) const;

      QList<QSharedPointer<ChatMessage>> getUnknownMessage(const Protos::Core::GetLastChatMessages& getLastChatMessage);

      static void fillProtoChatMessages(Protos::Common::ChatMessages& chatMessages, const QList<QSharedPointer<ChatMessage>>& messages);

   private:
      void insert(const QList<QSharedPointer<ChatMessage>>& messages);

      const int MAX_NUMBER_OF_STORED_CHAT_MESSAGES;

      QList<QSharedPointer<ChatMessage>> messages;
   };
}

#endif
