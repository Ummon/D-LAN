/**
  * Aybabtu - A decentralized LAN file sharing software.
  * Copyright (C) 2010-2011 Greg Burri <greg.burri@gmail.com>
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
   case 1:
      {
         QString nick = this->messages[index.row()].nick;
         if (nick.size() > 12)
            return nick.left(8).append("...");
         return nick;
      }
   case 2: return this->messages[index.row()].message;
   default: return QVariant();
   }
}

void ChatModel::newChatMessage(const Common::Hash& peerID, const QString& message)
{
   QString nick = this->peerListModel.getNick(peerID);

   this->beginInsertRows(QModelIndex(), this->messages.size(), this->messages.size());
   this->messages << Message(peerID, nick, QDateTime::currentDateTime(), message);
   this->endInsertRows();

   if (static_cast<quint32>(this->messages.size()) > SETTINGS.get<quint32>("max_chat_message_displayed"))
   {
      this->beginRemoveRows(QModelIndex(), 0, 0);
      this->messages.removeFirst();
      this->endRemoveRows();
   }
}
