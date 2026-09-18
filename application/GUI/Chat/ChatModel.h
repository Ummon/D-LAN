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
  
#pragma once

#include <QAbstractTableModel>
#include <QString>
#include <QSharedPointer>
#include <QDateTime>
#include <QList>
#include <QSize>
#include <QFont>
// #include <QRegularExpression>
#include <QPair>

#include <Protos/gui_protocol.pb.h>

#include <Common/Hash.h>
#include <Common/RemoteCoreController/ICoreConnection.h>
#include <Common/RemoteCoreController/ISendChatMessageResult.h>

#include <Peers/PeerListModel.h>
// #include <Emoticons/Emoticons.h>

namespace GUI
{
   class ChatModel : public QAbstractTableModel
   {
      Q_OBJECT
      static const int MAX_NICK_LENGTH = 12;

   public:
      ChatModel(
         QSharedPointer<RCC::ICoreConnection> coreConnection,
         PeerListModel& peerListModel,
         // const Emoticons& emoticons,
         const QString& roomName = QString()
      );

      bool isMainChat() const;
      QString getRoomName() const;

      QList<QPair<Common::Hash, QString>> getSortedOtherPeersByRelevance() const;
      QString getNick(const Common::Hash& id) const;

      QString getLineStr(int row) const;
      Common::Hash getPeerID(int row) const;
      bool isMessageIsOurs(int row) const;

      int rowCount(const QModelIndex& parent = QModelIndex()) const override;
      int columnCount(const QModelIndex& parent = QModelIndex()) const override;
      QVariant data(const QModelIndex& index, int role = Qt::DisplayRole) const override;
      //Qt::ItemFlags flags(const QModelIndex& index) const;

      inline QSize getCachedSize(const QModelIndex& index, const QString& markdown, const QFont& font, const QString& theme, int width) const
      {
         if (!this->isValidMessageIndex(index))
            return QSize();
         const auto& cached = this->messages[index.row()].renderedSize;
         return cached.size.width() == width && cached.markdown == markdown && cached.font == font && cached.theme == theme
            ? cached.size : QSize();
      }
      inline void insertCachedSize(const QModelIndex& index, const QSize& size, const QString& markdown, const QFont& font, const QString& theme)
      {
         if (this->isValidMessageIndex(index))
            this->messages[index.row()].renderedSize = { size, markdown, font, theme };
      }

      enum SendMessageStatus
      {
         OK,
         MESSAGE_TOO_LARGE,
         TIMEOUT,
         ERROR_UNKNOWN
      };

      void sendMessage(const QString& message, const QList<Common::Hash>& peerIDsAnswered = QList<Common::Hash>(), quint64 draftRevision = 0);

   private:
      bool isValidMessageIndex(const QModelIndex& index) const;
      void sendRawMessage(const QString& message, const QList<Common::Hash>& peerIDsAnswered, quint64 draftRevision);

   signals:
      void sendMessageStatus(GUI::ChatModel::SendMessageStatus status, quint64 draftRevision);

   private slots:
      void newChatMessages(const Protos::Common::ChatMessages& messages);
      void result(const Protos::GUI::ChatMessageResult& result, quint64 draftRevision);
      void resultTimeout(quint64 draftRevision);

   private:
      void removeResult(const RCC::ISendChatMessageResult* result);

      QList<QSharedPointer<RCC::ISendChatMessageResult>> results;

      struct Message
      {
         quint64 ID;
         Common::Hash peerID;
         bool answeringToUs;
         QString nick;
         QDateTime dateTime;
         QString message;
         // Keep lightweight size results even after the parsed document is evicted.
         struct RenderedSize
         {
            QSize size;
            QString markdown;
            QFont font;
            QString theme;
         } renderedSize;
         bool separateSenderLine = false;
      };

      QString formatMessage(const Message& message) const;

      QSharedPointer<RCC::ICoreConnection> coreConnection;
      PeerListModel& peerListModel;
      // const Emoticons& emoticons;

      QString roomName; // Empty for main chat.
      QList<Message> messages; // Always sorted by date-time.
      QList<Common::Hash> peersAnsweringToUs;

      // QRegularExpression regexMatchMessageContent;
      // QRegularExpression regexMatchFirstBR;
      // QRegularExpression regexMatchLastBR;
   };

}
