/**
  * D-LAN - A decentralized LAN file sharing software.
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
  
#ifndef GUI_CHATMODEL_H
#define GUI_CHATMODEL_H

#include <QAbstractTableModel>
#include <QString>
#include <QDateTime>
#include <QList>

#include <Protos/gui_protocol.pb.h>

#include <Common/Hash.h>
#include <Common/RemoteCoreController/ICoreConnection.h>

#include <PeerList/PeerListModel.h>

namespace GUI
{
   class ChatModel : public QAbstractTableModel
   {
      Q_OBJECT
   public:
      ChatModel(QSharedPointer<RCC::ICoreConnection> coreConnection, PeerListModel& peerListModel);

      QString getLineStr(int row) const;
      bool isMessageIsOurs(int row) const;

      int rowCount(const QModelIndex& parent = QModelIndex()) const;
      int columnCount(const QModelIndex& parent = QModelIndex()) const;
      QVariant data(const QModelIndex& index, int role = Qt::DisplayRole) const;
      Qt::ItemFlags flags(const QModelIndex& index) const;

      void newChatMessage(const Common::Hash& peerID, const QString& message);

   private slots:
      void newChatMessages(const Protos::GUI::EventChatMessages& messages);

   private:
      QSharedPointer<RCC::ICoreConnection> coreConnection;
      PeerListModel& peerListModel;

      struct Message
      {
         Message(const Common::Hash& peerID, const QString& nick, const QDateTime& dateTime, const QString& message) :
            peerID(peerID), nick(nick), dateTime(dateTime), message(message) {}

         Common::Hash peerID;
         QString nick;
         QDateTime dateTime;
         QString message;
      };

      QList<Message> messages;
   };

}

#endif
