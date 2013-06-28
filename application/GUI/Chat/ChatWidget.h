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
  
#ifndef GUI_CHATWINDOW_H
#define GUI_CHATWINDOW_H

#include <QWidget>
#include <QTextDocument>
#include <QStyledItemDelegate>
#include <QTextCharFormat>
#include <QUrl>

#include <Common/RemoteCoreController/ICoreConnection.h>

#include <Peers/PeerListModel.h>
#include <Chat/ChatModel.h>
#include <MDI/MdiWidget.h>
#include <Emoticons/Emoticons.h>
#include <Emoticons/EmoticonsWidget.h>

namespace Ui {
  class ChatWidget;
}

namespace GUI
{
   class ChatDelegate : public QStyledItemDelegate
   {
   public:
      ChatDelegate(QTextDocument& textDocument);
      void paint(QPainter* painter, const QStyleOptionViewItem& option, const QModelIndex& index) const;
      QSize	sizeHint(const QStyleOptionViewItem& option, const QModelIndex& index) const;
//      QWidget* createEditor(QWidget* parent, const QStyleOptionViewItem& option, const QModelIndex& index) const;
//      void setEditorData(QWidget* editor, const QModelIndex& index) const;

   private:
      QTextDocument& textDocument;
   };

   class ChatWidget : public MdiWidget
   {
      static const int DEFAULT_FONT_SIZE;

      Q_OBJECT
   public:
      explicit ChatWidget(QSharedPointer<RCC::ICoreConnection> coreConnection, Emoticons& emoticons, PeerListModel& peerListModel, QWidget* parent = nullptr);
      explicit ChatWidget(QSharedPointer<RCC::ICoreConnection> coreConnection, Emoticons& emoticons, PeerListModel& peerListModel, const QString& roomName, QWidget* parent = nullptr);
      ~ChatWidget();

      bool isGeneral() const;
      QString getRoomName() const;

   private slots:
      void sendMessage();
      void newRows(const QModelIndex& parent, int start, int end);
      void scrollChanged(int value);

      void displayContextMenuDownloads(const QPoint& point);
      void copySelectedLineToClipboard();

      void currentCharFormatChanged(const QTextCharFormat& charFormat);
      void cursorPositionChanged();
      void textChanged();

      void setFocusTxtMessage();

      void comboFontSizeChanged(int index);
      void setBold(bool toggled);
      void setItalic(bool toggled);
      void setUnderline(bool toggled);
      void setTextColor(QColor color);

      void resetFormat();

      void emoticonsButtonToggled(bool);
      void messageWordTyped(int position, const QString& word);
      void emoticonsWindowHidden();
      void emoticonsWindowHiddenDelayed();
      void insertEmoticon(const QString& theme, const QString& emoticonName);
      void defaultEmoticonThemeChanged(const QString& theme);

   protected:
      void keyPressEvent(QKeyEvent* keyEvent);
      void changeEvent(QEvent* event);
      bool eventFilter(QObject* obj, QEvent* event);

   private:
      void init();
      void applyCurrentFormat();
      void connectFormatWidgets();
      void disconnectFormatWidgets();
      void setComboFontSize(int fontSize);
      void displayEmoticons(const QPoint& positionSender, const QSize& sizeSender);

      void onActivate();

      void setNewMessageState(bool newMessage);

      static QUrl buildUrlEmoticon(const QString& theme, const QString& emoticonName);

      Ui::ChatWidget* ui;
      EmoticonsWidget* emoticonsWidget;

      QTextDocument textDocument;

      QSharedPointer<RCC::ICoreConnection> coreConnection;
      Emoticons& emoticons;
      ChatModel chatModel;
      ChatDelegate chatDelegate;

      bool autoScroll;
   };
}
#endif
