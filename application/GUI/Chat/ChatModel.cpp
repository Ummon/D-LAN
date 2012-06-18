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

#include <Protos/common.pb.h>

#include <Common/ProtoHelper.h>
#include <Common/Global.h>
#include <Common/Settings.h>

ChatModel::ChatModel(QSharedPointer<RCC::ICoreConnection> coreConnection, PeerListModel& peerListModel) :
   coreConnection(coreConnection), peerListModel(peerListModel)
{
   connect(this->coreConnection.data(), SIGNAL(newChatMessages(const Protos::Common::ChatMessages&)), this, SLOT(newChatMessages(const Protos::Common::ChatMessages&)));
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
   return 1;
}

QVariant ChatModel::data(const QModelIndex& index, int role) const
{
   if (index.row() >= this->messages.size())
      return QVariant();

   switch (role)
   {
   case Qt::DisplayRole:
      {
         const Message& mess = this->messages[index.row()];
         QString content;
         content
            .append(mess.dateTime.toString("[HH:mm:ss] "))
            .append("<b>").append(mess.nick).append("</b>: ")
            .append(mess.message);
         return content;
      }
      break;
   }

   return QVariant();
}

Qt::ItemFlags ChatModel::flags(const QModelIndex& index) const
{
   if (index.column() == 0)
      return Qt::ItemIsSelectable | Qt::ItemIsEnabled;
   else
      return Qt::ItemIsSelectable | Qt::ItemIsEditable | Qt::ItemIsEnabled;
}

void ChatModel::newChatMessages(const Protos::Common::ChatMessages& messages)
{
   if (messages.message_size() == 0)
      return;

   int j = this->messages.size();
   int previousJ;
   QList<Message> toInsert;

   for (int i = messages.message_size() - 1; i >= 0; i--)
   {
      const Common::Hash peerID(messages.message(i).peer_id().hash());
      Message message {
         messages.message(i).id(),
         peerID,
         this->peerListModel.getNick(peerID, Common::ProtoHelper::getStr(messages.message(i), &Protos::Common::ChatMessage::peer_nick)),
         QDateTime::fromMSecsSinceEpoch(messages.message(i).time()),
         Common::ProtoHelper::getStr(messages.message(i), &Protos::Common::ChatMessage::message)
      };

      previousJ = j;
      while (j > 0 && this->messages[j-1].dateTime > message.dateTime)
         j--;

      if (previousJ != j && !toInsert.isEmpty())
      {
         this->beginInsertRows(QModelIndex(), previousJ, previousJ + toInsert.size() - 1);
         for (QListIterator<Message> k(toInsert); k.hasNext();)
            this->messages.insert(previousJ, k.next());
         this->endInsertRows();
         toInsert.clear();
      }

      toInsert << message;

      // Special case for the last message.
      if (i == 0)
      {
         this->beginInsertRows(QModelIndex(), j, j + toInsert.size() - 1);
         for (QListIterator<Message> k(toInsert); k.hasNext();)
            this->messages.insert(j, k.next());
         this->endInsertRows();
      }
   }

   static const quint32 MAX_NB_MESSAGES = SETTINGS.get<quint32>("max_chat_message_displayed");
   const int nbMessageToDelete = this->messages.size() - MAX_NB_MESSAGES;

   if (nbMessageToDelete > 0)
   {
      this->beginRemoveRows(QModelIndex(), 0, nbMessageToDelete - 1);
      this->messages.erase(this->messages.begin(), this->messages.begin() + nbMessageToDelete);
      this->endRemoveRows();
   }
}
