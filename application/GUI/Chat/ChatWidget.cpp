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

#include <QHostAddress>
#include <QMenu>
#include <QMessageBox>
#include <QTextDocument>
#include <QTextBlock>
#include <QTextDocumentFragment>
#include <QAbstractTextDocumentLayout>
#include <QPainter>
#include <QClipboard>
#include <QKeyEvent>
#include <QScrollBar>
#include <QScreen>
#include <QTimer>
#include <QIcon>

#include <Log.h>
#include <Common/Settings.h>

Q_DECLARE_METATYPE(QHostAddress)

void PeerListChatDelegate::paint(QPainter* painter, const QStyleOptionViewItem& option, const QModelIndex& index) const
{
   QStyleOptionViewItem newOption(option);
   newOption.state = option.state & (~QStyle::State_HasFocus);

   // Show the selection only if the widget is active.
   if (!(newOption.state & QStyle::State_Active))
      newOption.state = newOption.state & (~QStyle::State_Selected);

   QStyledItemDelegate::paint(painter, newOption, index);
}

/////

/**
  * @class GUI::ChatDelegate
  *
  * To be able to select some message text via a QLineEdit and copy it.
  */

ChatDelegate::ChatDelegate(EmoticonTextDocument& textDocument)
   : textDocument(textDocument)
{
}

void ChatDelegate::paint(QPainter* painter, const QStyleOptionViewItem& option, const QModelIndex& index) const
{
   QStyleOptionViewItem newOption(option);
   this->initStyleOption(&newOption, index);

   newOption.state = option.state & (~QStyle::State_HasFocus);

   QStyle* style = newOption.widget ? newOption.widget->style() : QApplication::style();

   this->textDocument.setMarkdown(newOption.text);
   this->textDocument.setTextWidth(newOption.rect.width());

   // Aligne all emoticons at the middle.
   QTextCursor cur(&this->textDocument);
   for (QTextBlock b = this->textDocument.begin(); b.isValid(); b = b.next())
      for (auto it = b.begin(); !it.atEnd(); ++it) {
         const QTextFragment frag = it.fragment();
         if (frag.charFormat().isImageFormat()) {
            QTextCharFormat fmt;
            fmt.setVerticalAlignment(QTextCharFormat::AlignMiddle);
            cur.setPosition(frag.position());
            cur.setPosition(frag.position() + frag.length(), QTextCursor::KeepAnchor);
            cur.mergeCharFormat(fmt);
         }
      }

   newOption.text = QString();
   style->drawControl(QStyle::CE_ItemViewItem, &newOption, painter, newOption.widget);

   QAbstractTextDocumentLayout::PaintContext ctx;
   ctx.palette = newOption.palette;

   // Highlighting text if item is selected.
   if (newOption.state & QStyle::State_Selected && newOption.state & QStyle::State_Active)
      ctx.palette.setColor(QPalette::Text, newOption.palette.color(QPalette::Active, QPalette::HighlightedText));

   QRect textRect = style->subElementRect(QStyle::SE_ItemViewItemText, &newOption);
   painter->save();
   painter->translate(textRect.topLeft());
   painter->setClipRect(textRect.translated(-textRect.topLeft()));
   this->textDocument.documentLayout()->draw(painter, ctx);
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

   QStyleOptionViewItem newOption = option;
   initStyleOption(&newOption, index);

   this->textDocument.setMarkdown(newOption.text);
   this->textDocument.setTextWidth(newOption.rect.width());
   QSize size(newOption.rect.width(), this->textDocument.size().height()); // Width should be "doc.idealWidth()".
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

const QChar ChatWidget::EXPLICIT_LINE_RETURN(0x2800);

ChatWidget::ChatWidget(QSharedPointer<RCC::ICoreConnection> coreConnection, Emoticons& emoticons, QWidget* parent) :
   MdiWidget(parent),
   ui(new Ui::ChatWidget),
   textDocument(emoticons),
   coreConnection(coreConnection),
   emoticons(emoticons),
   peerListModel(coreConnection),
   chatModel(coreConnection, this->peerListModel),
   chatDelegate(textDocument)
{
   this->init();
}

ChatWidget::ChatWidget(
   QSharedPointer<RCC::ICoreConnection> coreConnection,
   Emoticons& emoticons,
   const QString& roomName,
   QWidget* parent
) :
   MdiWidget(parent),
   ui(new Ui::ChatWidget),
   textDocument(emoticons),
   coreConnection(coreConnection),
   emoticons(emoticons),
   peerListModel(coreConnection),
   chatModel(coreConnection, this->peerListModel, roomName),
   chatDelegate(textDocument)
{
   this->init();
   this->peerListModel.setRoom(roomName);
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

void ChatWidget::sendMessage()
{
   QString md = this->ui->txtMessage->toMarkdown();
   md.replace(QChar(10), ' '); // 'toMarkdown' inserts some '\n'.. we remove it.
   md.replace(EXPLICIT_LINE_RETURN, '\n'); // We replace the explicit line returns (U+2800) by a '\n'.
   this->chatModel.sendMessage(md, this->getPeerAnswers());
   this->answers.clear();
   this->currentAnswer = {};
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

void ChatWidget::sendMessageStatus(ChatModel::SendMessageStatus status)
{
   switch (status)
   {
   case  ChatModel::OK:
      this->ui->txtMessage->document()->clear();
      break;

   case ChatModel::MESSAGE_TOO_LARGE:
      QMessageBox::information(this, tr("Unable to send message"), tr("The message is too long"));
      break;

   default:
      QMessageBox::information(this, tr("Unable to send message"), tr("The message can't be send, unknown error"));
      break;
   }
}

void ChatWidget::scrollChanged(int value)
{
   this->autoScroll = value == this->ui->tblChat->verticalScrollBar()->maximum();
}

void ChatWidget::displayContextMenuPeers(const QPoint& point)
{
   QModelIndex i = this->ui->tblRoomPeers->currentIndex();
   QHostAddress addr = i.isValid() ? this->peerListModel.getPeerIP(i.row()) : QHostAddress();
   QVariant addrVariant;
   addrVariant.setValue(addr);

   QMenu menu;
   menu.addAction(QIcon(":/icons/ressources/folder.svg"), tr("Browse"), this, &ChatWidget::browseSelectedPeers);

   if (!addr.isNull())
   {
      QAction* copyIPAction = menu.addAction(tr("Copy IP: %1").arg(addr.toString()), this, &ChatWidget::copyIPToClipboard);
      copyIPAction->setData(addrVariant);
   }

   menu.exec(this->ui->tblRoomPeers->mapToGlobal(point));
}

void ChatWidget::browseSelectedPeers()
{
   foreach (QModelIndex i, this->ui->tblRoomPeers->selectionModel()->selectedIndexes())
   {
      if (i.isValid())
      {
         Common::Hash peerID = this->peerListModel.getPeerID(i.row());
         if (!peerID.isNull())
            emit browsePeer(peerID);
      }
   }

   this->ui->tblRoomPeers->clearSelection();
}

void ChatWidget::copyIPToClipboard()
{
   QAction* action = dynamic_cast<QAction*>(this->sender());
   if (action)
   {
      QHostAddress address = action->data().value<QHostAddress>();
      QApplication::clipboard()->setText(address.toString());
   }
}

void ChatWidget::displayContextMenu(const QPoint& point)
{
   QMenu menu;
   menu.addAction(tr("Copy selected lines"), this, &ChatWidget::copySelectedLineToClipboard);
   menu.addAction(QIcon(":/icons/ressources/folder.svg"), tr("Browse selected peers"), this, &ChatWidget::browseSelectedMessages);
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

void ChatWidget::browseSelectedMessages()
{
   QSet<Common::Hash> peersSent;

   foreach (QModelIndex i, this->ui->tblChat->selectionModel()->selectedIndexes())
      if (i.isValid())
      {
         Common::Hash peerID = this->chatModel.getPeerID(i.row());
         if (!peerID.isNull() && !peersSent.contains(peerID))
         {
            peersSent.insert(peerID);
            emit browsePeer(peerID);
         }
      }
}

/**
  * Update the format widgets depending of the cursor position.
  */
void ChatWidget::currentCharFormatChanged(const QTextCharFormat& charFormat)
{
   if (this->ui->txtMessage->textCursor().position() > 0 || this->ui->txtMessage->document()->characterCount() > 1)
   {
      this->disconnectFormatWidgets();

      this->ui->butBold->setChecked(charFormat.fontWeight() >= QFont::Bold);
      this->ui->butItalic->setChecked(charFormat.fontItalic());
      this->ui->butUnderline->setChecked(charFormat.fontUnderline());

      this->connectFormatWidgets();
   }
   else // Special case to avoid to reset the formatting when the cursor is put at the beginning.
   {
      disconnect(
         this->ui->txtMessage,
         &ChatTextEdit::currentCharFormatChanged,
         this,
         &ChatWidget::currentCharFormatChanged
      );

      this->applyCurrentFormat();

      connect(
         this->ui->txtMessage,
         &ChatTextEdit::currentCharFormatChanged,
         this,
         &ChatWidget::currentCharFormatChanged
      );
   }
}

// void ChatWidget::cursorPositionChanged()
// {
//    if (this->ui->txtMessage->textCursor().position() != 0)
//    {
//       this->disconnectFormatWidgets();
//       this->ui->butColorBox->setColor(this->ui->txtMessage->textColor());
//       this->connectFormatWidgets();
//    }
//    else
//    {
//       disconnect(this->ui->txtMessage, &ChatTextEdit::cursorPositionChanged, this, &ChatWidget::cursorPositionChanged);
//       this->ui->txtMessage->setTextColor(this->ui->butColorBox->getCurrentColor());
//       connect(this->ui->txtMessage, &ChatTextEdit::cursorPositionChanged, this, &ChatWidget::cursorPositionChanged);
//    }
// }

/**
  * Adjust the text edit size depending of the size of each lines.
  */
void ChatWidget::textChanged()
{
   this->ui->txtMessage->setFixedHeight(this->ui->txtMessage->document()->size().height());
}

/**
  * When a document changes, the answer references have to be updated. They may be deleted if the changes collide with them.
  */
void ChatWidget::documentChanged(int position, int charsRemoved, int charsAdded)
{
   if (this->answers.getList().isEmpty())
      return;

   // const int delta = charsAdded - charsRemoved;

   // TODO.....
   /*for (const auto& answer: )

   QMutableLinkedListIterator i<Answer>{this->answers.getList()};
   i.toBack();

   while (i.hasPrevious()) {
      const auto& answer = e.previous();

      if (
          charsRemoved > 0 && position < answer.begin && position + charsRemoved >= answer.begin || // If there is one or more character removed into the answer or . . .
          position >= answer.begin && position < answer.end // . . . if there is one or more character added or removed into the answer
          )
      {
         // The answer is removed.
         i.remove();
      }

   } while (i != this->answers.getList().begin());*/
}

void ChatWidget::setFocusTxtMessage()
{
   this->ui->txtMessage->setFocus();
}

void ChatWidget::setBold(bool toggled)
{
   this->ui->txtMessage->setFontWeight(toggled ? QFont::Bold : QFont::Normal);
}

void ChatWidget::setItalic(bool toggled)
{
   this->ui->txtMessage->setFontItalic(toggled);
}

void ChatWidget::setUnderline(bool toggled)
{
   this->ui->txtMessage->setFontUnderline(toggled);
}

void ChatWidget::resetFormat()
{
   this->ui->butBold->setChecked(false);
   this->ui->butItalic->setChecked(false);
   this->ui->butUnderline->setChecked(false);

   this->applyCurrentFormat();
}

void ChatWidget::emoticonsButtonToggled(bool checked)
{
   if (checked)
   {
      QAbstractButton* sender = dynamic_cast<QAbstractButton*>(this->sender());
         this->displayEmoticons(this->mapToGlobal(sender->pos()), sender->size());
   }
}

void ChatWidget::messageWordTyped(int position, const QString& word)
{
   const QString& smile = this->emoticons.getSmileName(word);
   if (!smile.isEmpty())
   {
      QTextCursor cursor(this->ui->txtMessage->document());
      cursor.setPosition(position);
      cursor.setPosition(position + word.length(), QTextCursor::KeepAnchor);
      cursor.deleteChar();

      QTextImageFormat format;
      format.setName(buildUrlEmoticon(this->emoticons.getDefaultTheme(), smile).toString());
      format.setVerticalAlignment(QTextCharFormat::AlignMiddle);
      cursor.insertImage(format);
   }
}

void ChatWidget::emoticonsWindowHidden()
{
   // I know it's bad but I didn't find another solution.
   // The issue is when the emoticons window is displayed and the user press on the emoticons button again. In this
   // case the window 'hidden' signal is triggered before the button 'toggled' signal, so the button is set to unchecked before it is pressed again . . .
   QTimer::singleShot(100, this, &ChatWidget::emoticonsWindowHiddenDelayed);
}

void ChatWidget::emoticonsWindowHiddenDelayed()
{
   if (this->ui->butEmoticons->isChecked())
      this->ui->butEmoticons->setChecked(false);
}

void ChatWidget::insertEmoticon(const QString& theme, const QString& emoticonName)
{
   if (
      !this->ui->txtMessage->textCursor().atStart() &&
      !this->ui->txtMessage->document()->characterAt(this->ui->txtMessage->textCursor().position() - 1).isSpace()
   )
      this->ui->txtMessage->insertPlainText(" ");

   QTextCursor cursor = this->ui->txtMessage->textCursor();
   QTextImageFormat format;
   format.setName(buildUrlEmoticon(theme, emoticonName).toString());
   format.setVerticalAlignment(QTextCharFormat::AlignMiddle);
   cursor.insertImage(format);

   this->ui->txtMessage->insertPlainText(" ");
}

void ChatWidget::defaultEmoticonThemeChanged(const QString& theme)
{
   SETTINGS.set("default_emoticon_theme", theme);
   this->emoticons.setDefaultTheme(theme);
   SETTINGS.save();
}

void ChatWidget::autoCompleteStringAdded(QString str)
{
   if (!this->peerNameInsertionMode)
      return;

   this->currentAnswer.end += str.size();
   this->ui->txtMessage->insertPlainText(str);
}

void ChatWidget::autoCompleteLastCharRemoved()
{
   if (!this->peerNameInsertionMode)
      return;

   if (this->currentAnswer.end > this->currentAnswer.begin + 1)
   {
      this->ui->txtMessage->textCursor().deletePreviousChar();
      this->currentAnswer.end -= 1;
   }
}

void ChatWidget::autoCompleteClosed()
{
   if (!this->peerNameInsertionMode)
      return;

   this->peerNameInsertionMode = false;

   const Common::Hash& current = this->autoComplete->getCurrent();
   const QString& nick = this->peerListModel.getNick(current);

   if (nick.isNull())
   {
      QTextCursor cursor(this->ui->txtMessage->document());
      cursor.setPosition(this->currentAnswer.begin - (this->currentAnswer.startWithSpace ? 1 : 0));
      cursor.setPosition(this->currentAnswer.end, QTextCursor::KeepAnchor);
      cursor.deleteChar();
   }
   else
   {
      QTextCursor cursor(this->ui->txtMessage->document());
      cursor.setPosition(this->currentAnswer.begin + 1);
      cursor.setPosition(this->currentAnswer.end, QTextCursor::KeepAnchor);
      cursor.insertText(nick + ' ');
      this->answers.insert(this->currentAnswer);
   }

   this->currentAnswer = {};
}

void ChatWidget::keyPressEvent(QKeyEvent* keyEvent)
{
   // CTRL.
   if (keyEvent->modifiers().testFlag(Qt::ControlModifier))
   {
      switch (keyEvent->key())
      {
      case 'c':
      case 'C':
         this->copySelectedLineToClipboard();
         keyEvent->accept();
         break;

      case 'b':
      case 'B':
         this->ui->butBold->toggle();
         keyEvent->accept();
         break;

      case 'i':
      case 'I':
         this->ui->butItalic->toggle();
         keyEvent->accept();
         break;

      case 'u':
      case 'U':
         this->ui->butUnderline->toggle();
         keyEvent->accept();
         break;
      }
   }

   MdiWidget::keyPressEvent(keyEvent);
}

void ChatWidget::changeEvent(QEvent* event)
{
   if (event->type() == QEvent::LanguageChange)
      this->ui->retranslateUi(this);

   QWidget::changeEvent(event);
}

/**
  * To grab events from the text box ('ui->txtMessage').
  */
bool ChatWidget::eventFilter(QObject* obj, QEvent* event)
{
   if (obj == this->ui->txtMessage && event->type() == QEvent::KeyPress)
   {
      QKeyEvent* keyEvent = static_cast<QKeyEvent*>(event);
      switch (keyEvent->key())
      {
      case Qt::Key_Return: // 'return' : It sends the current message or validate the current peer name in peer name insertion.
         if (!(keyEvent->modifiers() & Qt::ShiftModifier))
         {
            this->sendMessage();
            return true;
         }
         else
         {
            // We add special characters to know where the explicit line returns are put,
            // they will be replaced in 'sendMessage'.
            QTextCursor cursor = this->ui->txtMessage->textCursor();
            cursor.insertText(QString(EXPLICIT_LINE_RETURN));
         }
         break;

      case Qt::Key_Tab: // 'tab' : begins a peer name insertion or in peer name insertion mode step through each peer names.
         this->activatePeerNameInsertionMode();
         return true;
      }
   }

   return MdiWidget::eventFilter(obj, event);
}

void ChatWidget::init()
{
   this->peerNameInsertionMode = false;
   this->autoScroll = true;

   this->ui->setupUi(this);

   this->emoticons.setDefaultTheme(SETTINGS.get<QString>("default_emoticon_theme"));

   this->emoticonsWidget = new EmoticonsWidget(this->emoticons, this);
   this->emoticonsWidget->setWindowFlags(Qt::Popup);

   this->autoComplete = new AutoComplete(this);
   this->autoComplete->setWindowFlags(Qt::Popup);
   this->autoComplete->setVisible(false);

   this->ui->txtMessage->setEmoticons(&this->emoticons);

   if (this->chatModel.isMainChat())
   {
      this->setWindowTitle(tr("Chat"));
      this->ui->tblRoomPeers->hide();
   }
   else
   {
      this->ui->splitter->setStretchFactor(0, 5);
      this->ui->splitter->setStretchFactor(1, 1);

      this->setWindowTitle(this->chatModel.getRoomName());

      this->peerListModel.setSortType(Protos::GUI::Settings::BY_NICK);
      this->peerListModel.setDisplayOnlyPeersWithStatusOK(true);
      this->peerListModel.setToolTipEnabled(false);

      this->ui->tblRoomPeers->setModel(&this->peerListModel);
      this->ui->tblRoomPeers->setItemDelegate(&this->peerListDelegate);
      this->ui->tblRoomPeers->hideColumn(0);
      this->ui->tblRoomPeers->hideColumn(2);
      this->ui->tblRoomPeers->horizontalHeader()->setSectionResizeMode(1, QHeaderView::Stretch);
      this->ui->tblRoomPeers->horizontalHeader()->setVisible(false);
      this->ui->tblRoomPeers->verticalHeader()->setSectionResizeMode(QHeaderView::Fixed); // TODO: is there an another way to reduce the row size?
      this->ui->tblRoomPeers->verticalHeader()->setDefaultSectionSize(QFontMetrics(QApplication::font()).height() + 4);
      this->ui->tblRoomPeers->verticalHeader()->setVisible(false);
      this->ui->tblRoomPeers->setSelectionBehavior(QAbstractItemView::SelectRows);
      this->ui->tblRoomPeers->setSelectionMode(QAbstractItemView::ExtendedSelection);
      this->ui->tblRoomPeers->setShowGrid(false);
      this->ui->tblRoomPeers->setAlternatingRowColors(false);
      this->ui->tblRoomPeers->setContextMenuPolicy(Qt::CustomContextMenu);
      connect(this->ui->tblRoomPeers, &QTableView::customContextMenuRequested, this, &ChatWidget::displayContextMenuPeers);
      connect(this->ui->tblRoomPeers, &QTableView::doubleClicked, this, &ChatWidget::browseSelectedPeers);
   }

   this->applyCurrentFormat();

   this->ui->tblChat->setModel(&this->chatModel);
   this->ui->tblChat->setItemDelegate(&this->chatDelegate);
   this->ui->tblChat->setWordWrap(true);
   this->ui->tblChat->setVerticalScrollMode(QAbstractItemView::ScrollPerPixel);
   this->ui->tblChat->horizontalHeader()->setVisible(false);
   this->ui->tblChat->horizontalHeader()->setSectionResizeMode(QHeaderView::Stretch);
   this->ui->tblChat->verticalHeader()->setSectionResizeMode(QHeaderView::ResizeToContents);
   this->ui->tblChat->verticalHeader()->setVisible(false);
   this->ui->tblChat->setSelectionBehavior(QAbstractItemView::SelectRows);
   this->ui->tblChat->setSelectionMode(QAbstractItemView::ExtendedSelection);
   this->ui->tblChat->setShowGrid(false);
   this->ui->tblChat->setAutoScroll(false);

   this->ui->tblChat->setEditTriggers(QAbstractItemView::AllEditTriggers);

   this->ui->tblChat->setContextMenuPolicy(Qt::CustomContextMenu);
   connect(this->ui->tblChat, &QTableView::customContextMenuRequested, this, &ChatWidget::displayContextMenu);

   connect(&this->chatModel, &ChatModel::rowsInserted, this, &ChatWidget::newRows);
   connect(&this->chatModel, &ChatModel::sendMessageStatus, this, &ChatWidget::sendMessageStatus);

   connect(this->ui->tblChat->verticalScrollBar(), &QScrollBar::valueChanged, this, &ChatWidget::scrollChanged);

   connect(this->ui->txtMessage, &ChatTextEdit::currentCharFormatChanged, this, &ChatWidget::currentCharFormatChanged);
   // connect(this->ui->txtMessage, &ChatTextEdit::cursorPositionChanged, this, &ChatWidget::cursorPositionChanged);
   connect(this->ui->txtMessage, &ChatTextEdit::textChanged, this, &ChatWidget::textChanged);
   connect(this->ui->txtMessage->document(), &QTextDocument::contentsChange, this, &ChatWidget::documentChanged);

   // connect(this->ui->cmbFontSize, &QComboBox::currentIndexChanged, this, &ChatWidget::setFocusTxtMessage);
   connect(this->ui->butBold, &QPushButton::clicked, this, &ChatWidget::setFocusTxtMessage);
   connect(this->ui->butItalic, &QPushButton::clicked, this, &ChatWidget::setFocusTxtMessage);
   connect(this->ui->butUnderline, &QPushButton::clicked, this, &ChatWidget::setFocusTxtMessage);
   // connect(this->ui->butColorBox, &QPushButton::clicked, this, &ChatWidget::setFocusTxtMessage);

   connect(this->ui->butResetFormat, &QPushButton::clicked, this, &ChatWidget::setFocusTxtMessage);
   connect(this->ui->butResetFormat, &QPushButton::clicked, this, &ChatWidget::resetFormat);

   connect(this->ui->butEmoticons, &QPushButton::toggled, this, &ChatWidget::emoticonsButtonToggled);
   connect(this->ui->txtMessage, &ChatTextEdit::wordTyped, this, &ChatWidget::messageWordTyped);
   connect(this->emoticonsWidget, &EmoticonsWidget::hidden, this, &ChatWidget::emoticonsWindowHidden);
   connect(this->emoticonsWidget, &EmoticonsWidget::emoticonChosen, this, &ChatWidget::insertEmoticon);
   connect(this->emoticonsWidget, &EmoticonsWidget::defaultThemeChanged, this, &ChatWidget::defaultEmoticonThemeChanged);

   connect(this->autoComplete, &AutoComplete::stringAdded, this, &ChatWidget::autoCompleteStringAdded);
   connect(this->autoComplete, &AutoComplete::lastCharRemoved, this, &ChatWidget::autoCompleteLastCharRemoved);
   connect(this->autoComplete, &AutoComplete::closed, this, &ChatWidget::autoCompleteClosed);

   this->connectFormatWidgets();

   this->ui->txtMessage->installEventFilter(this);

   // ALT-<num> is used to switch bewteen windows, we tell the text edit widget to ignore them.
   for (char c = '0'; c <= '9'; c++)
      this->ui->txtMessage->addIgnoreKeyCombination({ Qt::AltModifier, c });

   this->setNewMessageState(false);
}

void ChatWidget::applyCurrentFormat()
{
   this->ui->txtMessage->setFontWeight(this->ui->butBold->isChecked() ? QFont::Bold : QFont::Normal);
   this->ui->txtMessage->setFontItalic(this->ui->butItalic->isChecked());
   this->ui->txtMessage->setFontUnderline(this->ui->butUnderline->isChecked());
}

void ChatWidget::connectFormatWidgets()
{
   connect(this->ui->butBold, &QPushButton::toggled, this, &ChatWidget::setBold);
   connect(this->ui->butItalic, &QPushButton::toggled, this, &ChatWidget::setItalic);
   connect(this->ui->butUnderline, &QPushButton::toggled, this, &ChatWidget::setUnderline);
}

void ChatWidget::disconnectFormatWidgets()
{
   disconnect(this->ui->butBold, &QPushButton::toggled, this, &ChatWidget::setBold);
   disconnect(this->ui->butItalic, &QPushButton::toggled, this, &ChatWidget::setItalic);
   disconnect(this->ui->butUnderline, &QPushButton::toggled, this, &ChatWidget::setUnderline);
}

void ChatWidget::displayEmoticons(const QPoint& positionSender, const QSize& sizeSender)
{
   this->emoticonsWidget->show();

   this->emoticonsWidget->move(
      positionSender.x() + sizeSender.width(),
      positionSender.y() + sizeSender.height() - this->emoticonsWidget->height()
   );

   // TODO: Check if correct.
   QRect desktopGeom = qGuiApp->primaryScreen()->availableGeometry();
   if (this->emoticonsWidget->pos().y() < 0)
      this->emoticonsWidget->move(this->emoticonsWidget->pos().x(), 0);

   if (this->emoticonsWidget->pos().x() + this->emoticonsWidget->width() > desktopGeom.width())
      this->emoticonsWidget->move(positionSender.x() - this->emoticonsWidget->width(), this->emoticonsWidget->pos().y());
}

void ChatWidget::activatePeerNameInsertionMode()
{
   if (this->peerNameInsertionMode)
      return;

   const bool insertSpaceBefore =
      !this->ui->txtMessage->textCursor().atStart() &&
      !this->ui->txtMessage->document()->characterAt(this->ui->txtMessage->textCursor().position() - 1).isSpace();

   if (insertSpaceBefore)
      this->ui->txtMessage->insertPlainText(" ");
   this->ui->txtMessage->insertPlainText("@");

   const int cursorPosition = this->ui->txtMessage->textCursor().position();
   currentAnswer.startWithSpace = insertSpaceBefore;
   currentAnswer.begin = cursorPosition - 1;
   currentAnswer.end = cursorPosition;
   currentAnswer.peerID = Common::Hash();

   QRect cursorRect = this->ui->txtMessage->cursorRect();
   const QPoint& pos = this->ui->txtMessage->viewport()->mapToGlobal(cursorRect.bottomRight());

   this->autoComplete->show();
   this->autoComplete->move(pos.x(), pos.y());
   this->autoComplete->setValues(this->chatModel.getSortedOtherPeersByRelevance());

   this->peerNameInsertionMode = true;
}

QList<Common::Hash> ChatWidget::getPeerAnswers() const
{
   QList<Common::Hash> result;
   for (const auto& a : this->answers.getList())
      result.append(a.peerID);
   return result;
}

void ChatWidget::onActivate()
{
   this->setNewMessageState(false);
   this->ui->txtMessage->setFocus();
}

void ChatWidget::setNewMessageState(bool newMessage)
{
   if (newMessage)
   {      
      if (this->chatModel.isMainChat())
      {
         this->setWindowIcon(QIcon(":/icons/ressources/chat_new_mess.svg"));
      }
      else
      {
         this->setWindowIcon(QIcon(":/icons/ressources/chat_room_new_mess.svg"));
      }
   }
   else
   {
      if (this->chatModel.isMainChat())
      {
         this->setWindowIcon(QIcon(":/icons/ressources/chat.svg"));
      }
      else
      {
         this->setWindowIcon(QIcon(":/icons/ressources/chat_room.svg"));
      }
   }
}

QUrl ChatWidget::buildUrlEmoticon(const QString& theme, const QString& emoticonName)
{
   return QUrl(QString("emoticons://%1/%2").arg(theme, emoticonName));
}

QString ChatWidget::mdEmoticon(const QString& theme, const QString& emoticonName)
{
   return QString("![%1](%2)").arg(emoticonName, buildUrlEmoticon(theme, emoticonName).toString());
}
