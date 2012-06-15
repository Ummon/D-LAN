#include <priv/ChatMessages.h>
using namespace CS;

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

void ChatMessages::insert(const QList<QSharedPointer<ChatMessage>>& messages)
{
   /*int j = this->messages.size();
   for (int i = messages.size() - 1; i >= 0; i--)
   {
      const Common::Hash peerID(messages.message(i).peer_id().hash());
      Message message {
         peerID,
         this->peerListModel.getNick(peerID),
         QDateTime::fromMSecsSinceEpoch(messages.message(i).time()),
         Common::ProtoHelper::getStr(messages.message(i), &Protos::Common::ChatMessage::message)
      };

      while (j > 0 && this->messages[j-1].dateTime > message.dateTime)
         j--;

      this->beginInsertRows(QModelIndex(), j, j);
      this->messages.insert(j, message);
      this->endInsertRows();
   }*/
}
