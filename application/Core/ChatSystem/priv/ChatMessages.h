#ifndef CHATSYSTEM_CHATMESSAGES_H
#define CHATSYSTEM_CHATMESSAGES_H

#include <QString>
#include <QList>
#include <QSharedPointer>

#include <Common/Hash.h>

#include <priv/ChatMessage.h>

namespace CS
{
   class ChatMessages
   {
   public:
      ChatMessages();

      QSharedPointer<ChatMessage> add(const QString& message, const Common::Hash& ownerID);

   private:
      void insert(const QList<QSharedPointer<ChatMessage>>& messages);


      const quint32 MAX_NUMBER_OF_STORED_CHAT_MESSAGES;

      QList<QSharedPointer<ChatMessage>> messages;
   };
}

#endif
