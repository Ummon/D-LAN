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

      QSharedPointer<ChatMessage> add(const QString& message, const Common::Hash& ownerID);
      QList<QSharedPointer<ChatMessage>>  add(const Protos::Common::ChatMessages& chatMessages);

      QList<quint64> getLastMessageIDs(int n) const;

      QList<QSharedPointer<ChatMessage>> getUnknownMessage(const Protos::Core::GetLastChatMessages& getLastChatMessage);

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
