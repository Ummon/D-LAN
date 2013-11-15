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
  
#include <priv/ChatMessages.h>
using namespace CS;

#include <QSet>

#include <Common/PersistentData.h>
#include <Common/Settings.h>

#include <priv/Log.h>

ChatMessages::ChatMessages() :
   d(new ChatMessagesData)
{}

ChatMessages::ChatMessages(const ChatMessages& other)
   : d(other.d)
{
}

QSharedPointer<ChatMessage> ChatMessages::add(const QString& message, const Common::Hash& ownerID, const QString& ownerNick, const QString& roomName, const QList<Common::Hash>& peerIDsAnswer)
{
   QSharedPointer<ChatMessage> mess = QSharedPointer<ChatMessage>(new ChatMessage(message, ownerID, ownerNick, roomName, peerIDsAnswer));

   this->insert(QList<QSharedPointer<ChatMessage>> { mess });

   return mess;
}

QList<QSharedPointer<ChatMessage>> ChatMessages::add(const Protos::Common::ChatMessages& chatMessages)
{
   QList<QSharedPointer<ChatMessage>> messages;

   for (int i = 0; i < chatMessages.message_size(); i++)
      messages << QSharedPointer<ChatMessage>(new ChatMessage(chatMessages.message(i)));

   return this->insert(messages);
}

QList<quint64> ChatMessages::getLastMessageIDs(int nMax) const
{
   QList<quint64> result;

   QListIterator<QSharedPointer<ChatMessage>> i(this->d->messages);
   i.toBack();
   int n = 0;
   while (i.hasPrevious() && n++ < nMax)
      result << i.previous()->getID();

   return result;
}

QList<QSharedPointer<ChatMessage>> ChatMessages::getMessages() const
{
   return this->d->messages;
}

/**
  * Returns the last unkown messages, the known message IDs are defined into 'getLastChatMessage'.
  * The returned messages are sorted from oldest to youngest.
  */
QList<QSharedPointer<ChatMessage>> ChatMessages::getUnknownMessages(const Protos::Core::GetLastChatMessages& getLastChatMessage) const
{
   QSet<quint64> knownIDs;
   knownIDs.reserve(getLastChatMessage.message_id_size());
   for (int i = 0; i < getLastChatMessage.message_id_size(); i++)
      knownIDs.insert(getLastChatMessage.message_id(i));

   QList<QSharedPointer<ChatMessage>> result;
   for (int i = this->d->messages.size() - 1; i >= 0 && this->d->messages.size() - i <= int(getLastChatMessage.number()); i--)
   {
      if (!knownIDs.remove(this->d->messages[i]->getID()))
         result.prepend(this->d->messages[i]);
   }

   return result;
}

void ChatMessages::fillProtoChatMessages(Protos::Common::ChatMessages& chatMessages, int number) const
{
   int i = number > this->d->messages.size() ? 0 : this->d->messages.size() - number;
   while (i < this->d->messages.size())
      this->d->messages[i++]->fillProtoChatMessage(*chatMessages.add_message());
}

/**
  * Fill 'chatMessages' with the given messages. 'chatMessages' must not exceed 'maxByteSize'.
  * @return The messages which aren't put in 'chatMessages'.
  */
QList<QSharedPointer<ChatMessage>> ChatMessages::fillProtoChatMessages(Protos::Common::ChatMessages& chatMessages, const QList<QSharedPointer<ChatMessage>>& messages, int maxByteSize)
{
   QList<QSharedPointer<ChatMessage>> result(messages);
   for (QMutableListIterator<QSharedPointer<ChatMessage>> i(result); i.hasNext();)
   {
      i.next()->fillProtoChatMessage(*chatMessages.add_message());
      if (maxByteSize != std::numeric_limits<int>::max() && chatMessages.ByteSize() > maxByteSize)
      {
         chatMessages.mutable_message()->RemoveLast();
         return result;
      }
      i.remove();
   }
   return result;
}


/**
  * Load the chat messages from the file previously saved in the user home and return it.
  */
void ChatMessages::loadFromFile(const QString& filename)
{
   try
   {
      Protos::Common::ChatMessages chatMessages;
      Common::PersistentData::getValue(filename, chatMessages, FOLDER_TYPE_MESSAGES_SAVED);
      this->add(chatMessages);
      this->d->changed = false;
   }
   catch (Common::UnknownValueException&)
   {
      L_WARN(QString("The saved chat messages cannot be retrived (the file doesn't exist) : %1").arg(filename));
   }
   catch (...)
   {
      L_WARN(QString("The saved chat messages cannot be retrived (Unkown exception) : %1").arg(filename));
   }
}

/**
  * Save the chat messages to a file in the user home.
  */
void ChatMessages::saveToFile(const QString& filename) const
{
   if (!this->d->changed)
      return;

   try
   {
      Protos::Common::ChatMessages chatMessages;
      this->fillProtoChatMessages(chatMessages);
      Common::PersistentData::setValue(filename, chatMessages, FOLDER_TYPE_MESSAGES_SAVED);
      this->d->changed = false;
   }
   catch (Common::PersistentDataIOException& err)
   {
      L_ERRO(err.message);
   }
}

/**
  * We return the inserted messages. A message will not be inserted if:
  *  - The messages size is equal to 'MAX_NUMBER_OF_STORED_CHAT_MESSAGES' and the message to insert is older than the oldest message in the list.
  *  - The message is already in the list.
  */
QList<QSharedPointer<ChatMessage>> ChatMessages::insert(const QList<QSharedPointer<ChatMessage>>& messages)
{                                   
   static const int MAX_NUMBER_OF_STORED_CHAT_MESSAGES = SETTINGS.get<quint32>("max_number_of_stored_chat_messages");

   QList<QSharedPointer<ChatMessage>> insertedMessages;

   QListIterator<QSharedPointer<ChatMessage>> i(messages);
   i.toBack();

   int j = this->d->messages.size();
   while (i.hasPrevious())
   {
      const QSharedPointer<ChatMessage>& mess = i.previous();

      if (this->d->messageIDs.contains(mess->getID()))
         continue;

      while (j > 0 && this->d->messages[j-1]->getTime() > mess->getTime())
         j--;

      if (this->d->messages.size() != MAX_NUMBER_OF_STORED_CHAT_MESSAGES || j != 0) // We avoid to insert a message which will ne deleted right after.
      {
         insertedMessages.prepend(mess);
         this->d->messageIDs.insert(mess->getID());
         this->d->messages.insert(j, mess);
      }
   }

   if (this->d->messages.size() > MAX_NUMBER_OF_STORED_CHAT_MESSAGES)
   {
      const auto begin = this->d->messages.begin();
      const auto end = this->d->messages.begin() + (this->d->messages.size() - MAX_NUMBER_OF_STORED_CHAT_MESSAGES);
      for (auto m = begin; m != end; m++)
         this->d->messageIDs.remove((*m)->getID());
      this->d->messages.erase(begin, end);
   }

   this->d->changed = !insertedMessages.isEmpty();

   return insertedMessages;
}
