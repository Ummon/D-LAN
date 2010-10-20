#include <ChatModel.h>
using namespace GUI;

#include <QtAlgorithms>

#include <Common/ProtoHelper.h>
#include <Common/Global.h>
#include <Common/Settings.h>

ChatModel::ChatModel(CoreConnection& coreConnection, PeerListModel& peerListModel)
   : coreConnection(coreConnection), peerListModel(peerListModel)
{
   connect(&this->coreConnection, SIGNAL(newChatMessage(const Common::Hash&, const QString&)), this, SLOT(newChatMessage(const Common::Hash&, const QString&)));
}

int ChatModel::rowCount(const QModelIndex& parent) const
{
   return this->messages.size();
}

int ChatModel::columnCount(const QModelIndex& parent) const
{
   return 3;
}

QVariant ChatModel::data(const QModelIndex& index, int role) const
{
   if (role != Qt::DisplayRole || index.row() >= this->messages.size())
      return QVariant();

   switch (index.column())
   {
   case 0: return this->messages[index.row()].dateTime.toString("HH:mm:ss");
   case 1: return this->messages[index.row()].nick;
   case 2: return this->messages[index.row()].message;
   default: return QVariant();
   }
}

void ChatModel::newChatMessage(const Common::Hash& peerID, const QString& message)
{
   QString nick = this->peerListModel.getNick(peerID);
   if (nick.isNull())
      nick = "<unknown>";

   this->beginInsertRows(QModelIndex(), messages.size(), messages.size());
   messages << Message(peerID, nick, QDateTime::currentDateTime(), message);
   this->endInsertRows();

   if (this->messages.size() > SETTINGS.get<quint32>("max_chat_message_displayed"))
   {
      this->beginRemoveRows(QModelIndex(), 0, 0);
      messages.removeFirst();
      this->endRemoveRows();
   }
}
