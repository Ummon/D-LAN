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
  
#include <Chat/ChatModel.h>
using namespace GUI;

#include <QtAlgorithms>
#include <QStringBuilder>

#include <Protos/gui_protocol.pb.h>

#include <Common/ProtoHelper.h>
#include <Common/Global.h>
#include <Common/Settings.h>

ChatModel::ChatModel(QSharedPointer<RCC::ICoreConnection> coreConnection, PeerListModel& peerListModel) :
   coreConnection(coreConnection), peerListModel(peerListModel)
{
   connect(this->coreConnection.data(), SIGNAL(newChatMessages(const Protos::GUI::EventChatMessages&)), this, SLOT(newChatMessages(const Protos::GUI::EventChatMessages&)));
}

/**
  * Return a string with all the field: "<date> <nick> <message>".
  */
QString ChatModel::getLineStr(int row) const
{
   return this->data(this->index(row, 0)).toString() % " " % this->data(this->index(row, 1)).toString() % " " % this->data(this->index(row, 2)).toString();
}

bool ChatModel::isMessageIsOurs(int row) const
{
   if (row >= this->messages.size())
      return false;

   return this->messages[row].peerID == this->coreConnection->getRemoteID();
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
   if (role != Qt::DisplayRole && role != Qt::EditRole || index.row() >= this->messages.size())
      return QVariant();

   switch (index.column())
   {
   case 0: return this->messages[index.row()].dateTime.toString("HH:mm:ss");
   case 1:
      {
         QString nick = this->messages[index.row()].nick;
         if (nick.size() > MAX_NICK_LENGTH)
            return nick.left(MAX_NICK_LENGTH-3).append("...");
         return nick;
      }
   case 2: return this->messages[index.row()].message;
   default: return QVariant();
   }
}

Qt::ItemFlags ChatModel::flags(const QModelIndex& index) const
{
   if (index.column() == 0)
      return Qt::ItemIsSelectable | Qt::ItemIsEnabled;
   else
      return Qt::ItemIsSelectable | Qt::ItemIsEditable | Qt::ItemIsEnabled;
}

void ChatModel::newChatMessage(const Common::Hash& peerID, const QString& message)
{
   QString nick = this->peerListModel.getNick(peerID);

   this->beginInsertRows(QModelIndex(), this->messages.size(), this->messages.size());
   this->messages << Message { peerID, nick, QDateTime::currentDateTime(), message };
   this->endInsertRows();

   if (static_cast<quint32>(this->messages.size()) > SETTINGS.get<quint32>("max_chat_message_displayed"))
   {
      this->beginRemoveRows(QModelIndex(), 0, 0);
      this->messages.removeFirst();
      this->endRemoveRows();
   }
}

void ChatModel::newChatMessages(const Protos::GUI::EventChatMessages& messages)
{
   if (messages.message_size() < 1)
      return;

   int j = this->messages.size();
   for (int i = messages.message_size() - 1; i >= 0; i--)
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
   }

   const int nbMessageToDelete = this->messages.size() - SETTINGS.get<quint32>("max_chat_message_displayed");

   if (nbMessageToDelete > 0)
   {
      this->beginRemoveRows(QModelIndex(), 0, nbMessageToDelete - 1);

      QList<Message>::iterator i = this->messages.begin();
      for (int n = 0; n < nbMessageToDelete && i != this->messages.end(); n++, i++);
      this->messages.erase(this->messages.begin(), i);

      this->endRemoveRows();
   }
}
