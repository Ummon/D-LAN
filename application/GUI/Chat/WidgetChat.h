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
  
#ifndef GUI_WIDGETCHAT_H
#define GUI_WIDGETCHAT_H

#include <QWidget>
#include <QStyledItemDelegate>

#include <Common/RemoteCoreController/ICoreConnection.h>

#include <Peers/PeerListModel.h>
#include <Chat/ChatModel.h>
#include <WidgetDocument.h>

namespace Ui {
  class WidgetChat;
}

namespace GUI
{
   class ChatDelegate : public QStyledItemDelegate
   {
   public:
      void paint(QPainter* painter, const QStyleOptionViewItem& option, const QModelIndex& index) const;
      QSize	sizeHint(const QStyleOptionViewItem& option, const QModelIndex& index) const;
//      QWidget* createEditor(QWidget* parent, const QStyleOptionViewItem& option, const QModelIndex& index) const;
//      void setEditorData(QWidget* editor, const QModelIndex& index) const;
   };

   class WidgetChat : public WidgetDocument
   {
      Q_OBJECT
   public:
      explicit WidgetChat(QSharedPointer<RCC::ICoreConnection> coreConnection, PeerListModel& peerListModel, QWidget* parent = nullptr);
      explicit WidgetChat(QSharedPointer<RCC::ICoreConnection> coreConnection, PeerListModel& peerListModel, const QString& roomName, QWidget* parent = nullptr);
      ~WidgetChat();

      bool isGeneral() const;
      QString getRoomName() const;

      void installEventFilterOnInput(QObject* filterObj);

   private slots:
      void sendMessage();
      void newRows(const QModelIndex& parent, int start, int end);
      void scrollChanged(int value);

      void displayContextMenuDownloads(const QPoint& point);
      void copySelectedLineToClipboard();

      void setToBold();

   protected:
      void keyPressEvent(QKeyEvent* event);
      void changeEvent(QEvent* event);

   private:
      void init();

      void onActivate();

      void setNewMessageState(bool newMessage);

      Ui::WidgetChat *ui;

      QSharedPointer<RCC::ICoreConnection> coreConnection;
      ChatModel chatModel;
      ChatDelegate chatDelegate;

      bool autoScroll;
   };
}
#endif
