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
  
#ifndef GUI_CHATMODEL_H
#define GUI_CHATMODEL_H

#include <QAbstractTableModel>
#include <QString>
#include <QDateTime>
#include <QList>
#include <QSize>
#include <QRegExp>
#include <QPair>

#include <Protos/gui_protocol.pb.h>

#include <Common/Hash.h>
#include <Common/RemoteCoreController/ICoreConnection.h>

#include <Peers/PeerListModel.h>
#include <Emoticons/Emoticons.h>

namespace GUI
{
   class ChatModel : public QAbstractTableModel
   {
      Q_OBJECT
      static const int MAX_NICK_LENGTH = 12;

   public:
      ChatModel(QSharedPointer<RCC::ICoreConnection> coreConnection, PeerListModel& peerListModel, const Emoticons& emoticons, const QString& roomName = QString());

      bool isMainChat() const;
      QString getRoomName() const;

      QList<QPair<Common::Hash, QString>> getRelevantLastPeers() const;

      QString getLineStr(int row, bool withHTML = true) const;
      Common::Hash getPeerID(int row) const;
      bool isMessageIsOurs(int row) const;

      int rowCount(const QModelIndex& parent = QModelIndex()) const;
      int columnCount(const QModelIndex& parent = QModelIndex()) const;
      QVariant data(const QModelIndex& index, int role = Qt::DisplayRole) const;
      //Qt::ItemFlags flags(const QModelIndex& index) const;

      inline QSize getCachedSize(const QModelIndex& index) { return this->messages[index.row()].size; }
      inline void insertCachedSize(const QModelIndex& index, const QSize& size) { this->messages[index.row()].size = size; }
      inline void removeCachedSize(const QModelIndex& index) { this->messages[index.row()].size = QSize(); }

      void sendMessage(const QString& message, const QList<Common::Hash>& peerIDsAnswered = QList<Common::Hash>());

   private slots:
      void newChatMessages(const Protos::Common::ChatMessages& messages);

   private:
      struct Message
      {
         quint64 ID;
         Common::Hash peerID;
         bool answeringToUs;
         QString nick;
         QDateTime dateTime;
         QString message;
         QSize size; // Ugly hack, we cache the rendered size to speed-up the method 'ChatDelegate::sizeHint'.
      };

      QString formatMessage(const Message& message) const;

      QSharedPointer<RCC::ICoreConnection> coreConnection;
      PeerListModel& peerListModel;
      const Emoticons& emoticons;

      QString roomName; // Empty for main chat.
      QList<Message> messages; // Always sorted by date-time.

      QRegExp regexMatchMessageContent;
      QRegExp regexMatchFirstBR;
      QRegExp regexMatchLastBR;
   };

}

#endif
