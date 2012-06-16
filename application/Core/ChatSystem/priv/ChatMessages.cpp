#include <priv/ChatMessages.h>
using namespace CS;

#include <QSet>

#include <Common/Settings.h>

ChatMessages::ChatMessages() :
   MAX_NUMBER_OF_STORED_CHAT_MESSAGES(SETTINGS.get<quint32>("max_number_of_stored_chat_messages"))
{}

QSharedPointer<ChatMessage> ChatMessages::add(const QString& message, const Common::Hash& ownerID)
{
   QSharedPointer<ChatMessage> mess = QSharedPointer<ChatMessage>(new ChatMessage(message, ownerID));

   this->insert(QList<QSharedPointer<ChatMessage>> { mess });

   return mess;
}

QList<QSharedPointer<ChatMessage> > ChatMessages::add(const Protos::Common::ChatMessages& chatMessages)
{
   QList<QSharedPointer<ChatMessage>> messages;

   for (int i = 0; i < chatMessages.message_size(); i++)
      messages << QSharedPointer<ChatMessage>(new ChatMessage(chatMessages.message(i)));

   this->insert(messages);

   return messages;
}

QList<quint64> ChatMessages::getLastMessageIDs(int n) const
{
   QList<quint64> result;

   QListIterator<QSharedPointer<ChatMessage>> i(messages);
   i.toBack();
   while (i.hasPrevious())
      result << i.previous()->getID();

   return result;
}

/**
  * The returned messages are sorted from oldest to youngest.
  */
QList<QSharedPointer<ChatMessage>> ChatMessages::getUnknownMessage(const Protos::Core::GetLastChatMessages& getLastChatMessage)
{
   QSet<quint64> knownIDs;
   for (int i = 0; i < getLastChatMessage.message_id_size(); i++)
      knownIDs.insert(getLastChatMessage.message_id(i));

   QList<QSharedPointer<ChatMessage>> result;
   for (int i = this->messages.size() - 1; i >= 0 && this->messages.size() - i <= int(getLastChatMessage.number()); i--)
   {
      if (!knownIDs.contains(this->messages[i]->getID()))
         result.prepend(this->messages[i]);
   }

   return result;
}

void ChatMessages::fillProtoChatMessages(Protos::Common::ChatMessages& chatMessages, const QList<QSharedPointer<ChatMessage>>& messages)
{
   for (QListIterator<QSharedPointer<ChatMessage>> i(messages); i.hasNext();)
      i.next()->fillProtoChatMessage(*chatMessages.add_message());
}

void ChatMessages::insert(const QList<QSharedPointer<ChatMessage>>& messages)
{
   QListIterator<QSharedPointer<ChatMessage>> i(messages);
   i.toBack();

   int j = this->messages.size();
   while (i.hasPrevious())
   {
      const QSharedPointer<ChatMessage>& mess = i.previous();
      while (j > 0 && this->messages[j-1]->getTime() > mess->getTime())
         j--;
      this->messages.insert(j, mess);
   }

   if (this->messages.size() > MAX_NUMBER_OF_STORED_CHAT_MESSAGES)
      this->messages.erase(this->messages.begin(), this->messages.begin() + (this->messages.size() - MAX_NUMBER_OF_STORED_CHAT_MESSAGES));
}
