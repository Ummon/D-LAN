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
  
#include <Chat/WidgetChat.h>
#include <ui_WidgetChat.h>
using namespace GUI;

/**
  * @class GUI::ChatDelegate
  *
  * To be able to select some message text via a QLineEdit and copy it.
  */

void ChatDelegate::paint(QPainter* painter, const QStyleOptionViewItem& option, const QModelIndex& index) const
{
   QStyleOptionViewItemV4 newOption(option);
   newOption.state = option.state & (~QStyle::State_HasFocus);
   QStyledItemDelegate::paint(painter, newOption, index);
}

QWidget* ChatDelegate::createEditor(QWidget* parent, const QStyleOptionViewItem& option, const QModelIndex& index) const
{
   QLineEdit* line = new QLineEdit(parent);
   line->setFrame(false);
   line->setReadOnly(true);
   return line;
}

void ChatDelegate::setEditorData(QWidget* editor, const QModelIndex& index) const
{
   // Set editor data.
   QLineEdit* line = static_cast<QLineEdit*>(editor);
   line->setText(index.model()->data(index, Qt::DisplayRole).toString());
}

/////

WidgetChat::WidgetChat(QSharedPointer<RCC::ICoreConnection> coreConnection, PeerListModel& peerListModel, QWidget* parent) :
   QWidget(parent), ui(new Ui::WidgetChat), coreConnection(coreConnection), chatModel(coreConnection, peerListModel)
{
   this->ui->setupUi(this);

   this->ui->tblChat->setModel(&this->chatModel);
   this->ui->tblChat->setItemDelegate(&this->chatDelegate);
   this->ui->tblChat->setVerticalScrollMode(QAbstractItemView::ScrollPerPixel);
   this->ui->tblChat->horizontalHeader()->setVisible(false);
   this->ui->tblChat->horizontalHeader()->setResizeMode(0, QHeaderView::ResizeToContents);
   this->ui->tblChat->horizontalHeader()->setResizeMode(1, QHeaderView::ResizeToContents);
   this->ui->tblChat->horizontalHeader()->setResizeMode(2, QHeaderView::Stretch);

   //this->ui->tblChat->verticalHeader()->setResizeMode(QHeaderView::ResizeToContents);
   this->ui->tblChat->verticalHeader()->setResizeMode(QHeaderView::Fixed);
   this->ui->tblChat->verticalHeader()->setDefaultSectionSize(QApplication::fontMetrics().height() + 2);

   this->ui->tblChat->verticalHeader()->setVisible(false);
   this->ui->tblChat->setSelectionBehavior(QAbstractItemView::SelectRows);
   this->ui->tblChat->setSelectionMode(QAbstractItemView::ExtendedSelection);
   this->ui->tblChat->setShowGrid(false);
   this->ui->tblChat->setAlternatingRowColors(true);

   this->ui->tblChat->setEditTriggers(QAbstractItemView::AllEditTriggers);

   connect(&this->chatModel, SIGNAL(rowsInserted(const QModelIndex&, int, int)), this, SLOT(newRows()));

   connect(this->ui->butSend, SIGNAL(clicked()), this, SLOT(sendMessage()));
   connect(this->ui->txtMessage, SIGNAL(returnPressed()), this, SLOT(sendMessage()));
}

WidgetChat::~WidgetChat()
{
   delete this->ui;
}

void WidgetChat::sendMessage()
{
   this->ui->txtMessage->setText(this->ui->txtMessage->text().trimmed());
   if (this->ui->txtMessage->text().isEmpty())
      return;

   static_cast<ChatModel*>(this->ui->tblChat->model())->newChatMessage(this->coreConnection->getID(), this->ui->txtMessage->text());

   this->coreConnection->sendChatMessage(this->ui->txtMessage->text());
   this->ui->txtMessage->clear();
}

void WidgetChat::newRows()
{
   this->ui->tblChat->scrollToBottom();   
   this->setNewMessageState(true);
}

void WidgetChat::showEvent(QShowEvent* event)
{
   this->setNewMessageState(false);
}

void WidgetChat::setNewMessageState(bool newMessage)
{
   if (newMessage && !this->isAncestorOf(QApplication::focusWidget()))
   {
      this->setWindowIcon(QIcon(":/icons/ressources/chat_new_message.png"));
      this->parentWidget()->setWindowIcon(QIcon(":/icons/ressources/chat_new_message.png"));
   }
   else
   {
      this->setWindowIcon(QIcon(":/icons/ressources/chat.png"));
      this->parentWidget()->setWindowIcon(QIcon(":/icons/ressources/chat.png"));
   }
}
