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
  
#include <Chat/ChatWidget.h>
#include <ui_ChatWidget.h>
using namespace GUI;

#include <QMenu>
#include <QTextDocument>
#include <QAbstractTextDocumentLayout>
#include <QPainter>
#include <QClipboard>
#include <QKeyEvent>
#include <QScrollBar>

#include <Log.h>

/**
  * @class GUI::ChatDelegate
  *
  * To be able to select some message text via a QLineEdit and copy it.
  */

void ChatDelegate::paint(QPainter* painter, const QStyleOptionViewItem& option, const QModelIndex& index) const
{
   QStyleOptionViewItemV4 optionV4(option);
   this->initStyleOption(&optionV4, index);

   optionV4.state = option.state & (~QStyle::State_HasFocus);

   QStyle* style = optionV4.widget ? optionV4.widget->style() : QApplication::style();

   QTextDocument doc;
   doc.setHtml(optionV4.text);
   doc.setTextWidth(optionV4.rect.width());

   optionV4.text = QString();
   style->drawControl(QStyle::CE_ItemViewItem, &optionV4, painter, optionV4.widget);

   QAbstractTextDocumentLayout::PaintContext ctx;
   ctx.palette = optionV4.palette;

   // Highlighting text if item is selected.
   if (optionV4.state & QStyle::State_Selected && optionV4.state & QStyle::State_Active)
       ctx.palette.setColor(QPalette::Text, optionV4.palette.color(QPalette::Active, QPalette::HighlightedText));

   QRect textRect = style->subElementRect(QStyle::SE_ItemViewItemText, &optionV4);
   painter->save();
   painter->translate(textRect.topLeft());
   painter->setClipRect(textRect.translated(-textRect.topLeft()));
   doc.documentLayout()->draw(painter, ctx);
   painter->restore();
}

/**
  * This method is called for EACH rows when a new message is inserted, this can be very heavy.
  * To reduce the latency we use a cache containing the size of each message.
  */
QSize	ChatDelegate::sizeHint(const QStyleOptionViewItem& option, const QModelIndex& index) const
{
   ChatModel* model = const_cast<ChatModel*>(static_cast<const ChatModel*>(index.model()));
   QSize cachedSize = model->getCachedSize(index);
   if (cachedSize.isValid())
   {
      if (cachedSize.width() == option.rect.width())
         return cachedSize;
      else
         model->removeCachedSize(index);
   }

   QStyleOptionViewItemV4 optionV4 = option;
   initStyleOption(&optionV4, index);

   QTextDocument doc;
   doc.setHtml(optionV4.text);
   doc.setTextWidth(optionV4.rect.width());
   QSize size(optionV4.rect.width(), doc.size().height()); // Width should be "doc.idealWidth()".
   model->insertCachedSize(index, size);
   return size;
}

//QWidget* ChatDelegate::createEditor(QWidget* parent, const QStyleOptionViewItem& option, const QModelIndex& index) const
//{
//   QLineEdit* line = new QLineEdit(parent);
//   line->setFrame(false);
//   line->setReadOnly(true);
//   return line;
//}

//void ChatDelegate::setEditorData(QWidget* editor, const QModelIndex& index) const
//{
//   // Set editor data.
//   QLineEdit* line = static_cast<QLineEdit*>(editor);
//   line->setText(index.model()->data(index, Qt::DisplayRole).toString());
//}

/////

ChatWidget::ChatWidget(QSharedPointer<RCC::ICoreConnection> coreConnection, PeerListModel& peerListModel, QWidget* parent) :
   MdiWidget(parent),
   ui(new Ui::ChatWidget),
   coreConnection(coreConnection),
   chatModel(coreConnection, peerListModel),
   autoScroll(true)
{
   this->init();
}

ChatWidget::ChatWidget(QSharedPointer<RCC::ICoreConnection> coreConnection, PeerListModel& peerListModel, const QString& roomName, QWidget* parent) :
   MdiWidget(parent),
   ui(new Ui::ChatWidget),
   coreConnection(coreConnection),
   chatModel(coreConnection, peerListModel, roomName),
   autoScroll(true)
{
   this->init();
}

ChatWidget::~ChatWidget()
{
   delete this->ui;
}

bool ChatWidget::isGeneral() const
{
   return this->getRoomName().isEmpty();
}

QString ChatWidget::getRoomName() const
{
   return this->chatModel.getRoomName();
}

/**
  * Install a event filter to the input widget (QLineEdit). For exemple, it allows to grap some key sequence as shortcut like CTRL-S to search.
  */
void ChatWidget::installEventFilterOnInput(QObject* filterObj)
{
   this->ui->txtMessage->installEventFilter(filterObj);
}

void ChatWidget::sendMessage()
{
   //this->ui->txtMessage->setText(this->ui->txtMessage->text().trimmed());
   if (this->ui->txtMessage->toHtml().isEmpty())
      return;

   this->coreConnection->sendChatMessage(this->ui->txtMessage->toHtml(), this->chatModel.getRoomName());
   this->ui->txtMessage->clear();
}

void ChatWidget::newRows(const QModelIndex& parent, int start, int end)
{
   for (int i = start; i <= end; i++)
      if (this->chatModel.isMessageIsOurs(i))
      {
         this->autoScroll = true;
         break;
      }

   if (this->autoScroll)
      this->ui->tblChat->scrollToBottom();
   this->setNewMessageState(true);
}

void ChatWidget::scrollChanged(int value)
{
   this->autoScroll = value == this->ui->tblChat->verticalScrollBar()->maximum();
}

void ChatWidget::displayContextMenuDownloads(const QPoint& point)
{
   QMenu menu;
   menu.addAction(tr("Copy selected lines"), this, SLOT(copySelectedLineToClipboard()));
   menu.exec(this->ui->tblChat->mapToGlobal(point));
}

void ChatWidget::copySelectedLineToClipboard()
{
   QString lines;
   QModelIndexList selection = this->ui->tblChat->selectionModel()->selectedRows();
   for (QListIterator<QModelIndex> i(selection); i.hasNext();)
   {
      lines.append(this->chatModel.getLineStr(i.next().row())).append('\n');
   }
   QApplication::clipboard()->setText(lines);
}

void ChatWidget::setToBold()
{
   //this->ui->txtMessage->setHtml("<h1>prout</h1>");
   //QString html = this->ui->txtMessage->toHtml();
   this->sendMessage();
}

void ChatWidget::keyPressEvent(QKeyEvent* event)
{
   // CTRL.
   if (event->modifiers().testFlag(Qt::ControlModifier))
   {
      switch (event->key())
      {
      case 'c':
      case 'C':
         this->copySelectedLineToClipboard();
      }
   }
}

void ChatWidget::changeEvent(QEvent* event)
{
   if (event->type() == QEvent::LanguageChange)
      this->ui->retranslateUi(this);
   else
      QWidget::changeEvent(event);
}

void ChatWidget::init()
{
   this->ui->setupUi(this);

   if (this->chatModel.isMainChat())
   {
      this->ui->tblRoomPeers->hide();
   }
   else
   {
      this->ui->splitter->setStretchFactor(0, 4);
      this->ui->splitter->setStretchFactor(1, 1);

      this->setWindowTitle(this->chatModel.getRoomName());
   }

   this->ui->tblChat->setModel(&this->chatModel);
   this->ui->tblChat->setItemDelegate(&this->chatDelegate);
   this->ui->tblChat->setWordWrap(true);
   this->ui->tblChat->setVerticalScrollMode(QAbstractItemView::ScrollPerPixel);
   this->ui->tblChat->horizontalHeader()->setVisible(false);
   this->ui->tblChat->horizontalHeader()->setResizeMode(QHeaderView::Stretch);
   this->ui->tblChat->verticalHeader()->setResizeMode(QHeaderView::ResizeToContents);
   this->ui->tblChat->verticalHeader()->setVisible(false);
   this->ui->tblChat->setSelectionBehavior(QAbstractItemView::SelectRows);
   this->ui->tblChat->setSelectionMode(QAbstractItemView::ExtendedSelection);
   this->ui->tblChat->setShowGrid(false);
   this->ui->tblChat->setAutoScroll(false);

   this->ui->tblChat->setEditTriggers(QAbstractItemView::AllEditTriggers);

   this->ui->tblChat->setContextMenuPolicy(Qt::CustomContextMenu);
   connect(this->ui->tblChat, SIGNAL(customContextMenuRequested(const QPoint&)), this, SLOT(displayContextMenuDownloads(const QPoint&)));

   connect(&this->chatModel, SIGNAL(rowsInserted(const QModelIndex&, int, int)), this, SLOT(newRows(const QModelIndex&, int, int)));
   connect(this->ui->tblChat->verticalScrollBar(), SIGNAL(valueChanged(int)), this, SLOT(scrollChanged(int)));

   connect(this->ui->txtMessage, SIGNAL(returnPressed()), this, SLOT(sendMessage()));

   connect(this->ui->butBold, SIGNAL(clicked()), this, SLOT(setToBold()));

   this->setNewMessageState(false);
}

void ChatWidget::onActivate()
{
   this->setNewMessageState(false);
   this->ui->txtMessage->setFocus();
}

void ChatWidget::setNewMessageState(bool newMessage)
{
   if (newMessage /*&& !this->isAncestorOf(QApplication::focusWidget())*/)
   {      
      if (this->chatModel.isMainChat())
      {
         this->setWindowIcon(QIcon(":/icons/ressources/chat_new_mess.png"));
         //this->parentWidget()->setWindowIcon(QIcon(":/icons/ressources/chat_new_mess.png"));
      }
      else
      {
         this->setWindowIcon(QIcon(":/icons/ressources/chat_room_new_mess.png"));
         //this->parentWidget()->setWindowIcon(QIcon(":/icons/ressources/chat_room_new_mess.png"));
      }
   }
   else
   {
      if (this->chatModel.isMainChat())
      {
         this->setWindowIcon(QIcon(":/icons/ressources/chat.png"));
         //this->parentWidget()->setWindowIcon(QIcon(":/icons/ressources/chat.png"));
      }
      else
      {
         this->setWindowIcon(QIcon(":/icons/ressources/chat_room.png"));
         //this->parentWidget()->setWindowIcon(QIcon(":/icons/ressources/chat_room.png"));
      }
   }
}
