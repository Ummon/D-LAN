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
  
#ifndef GUI_WIDGETCHAT_H
#define GUI_WIDGETCHAT_H

#include <QWidget>
#include <QStyledItemDelegate>

#include <Common/RemoteCoreController/ICoreConnection.h>

#include <PeerList/PeerListModel.h>
#include <Chat/ChatModel.h>

namespace Ui {
  class WidgetChat;
}

namespace GUI
{
   class ChatDelegate : public QStyledItemDelegate
   {
   public:
      void paint(QPainter* painter, const QStyleOptionViewItem& option, const QModelIndex& index) const;
      QWidget* createEditor(QWidget* parent, const QStyleOptionViewItem& option, const QModelIndex& index) const;
      void setEditorData(QWidget* editor, const QModelIndex& index) const;
   };

   class WidgetChat : public QWidget
   {
      Q_OBJECT
   public:
      explicit WidgetChat(QSharedPointer<RCC::ICoreConnection> coreConnection, PeerListModel& peerListModel, QWidget *parent = 0);
      ~WidgetChat();

   private slots:
      void sendMessage();
      void newRows(const QModelIndex& parent, int start, int end);
      void scrollChanged(int value);

      void displayContextMenuDownloads(const QPoint& point);
      void copySelectedLineToClipboard();

   protected:
      void showEvent(QShowEvent* event);
      void keyPressEvent(QKeyEvent* event);

   private:
      void setNewMessageState(bool newMessage);

      Ui::WidgetChat *ui;

      QSharedPointer<RCC::ICoreConnection> coreConnection;
      ChatModel chatModel;
      ChatDelegate chatDelegate;

      bool autoScroll;
   };
}
#endif
