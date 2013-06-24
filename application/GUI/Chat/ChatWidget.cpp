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
#include <QDesktopWidget>
#include <QTimer>

#include <Log.h>
#include <Common/Settings.h>

/**
  * @class GUI::ChatDelegate
  *
  * To be able to select some message text via a QLineEdit and copy it.
  */

ChatDelegate::ChatDelegate(QTextDocument& textDocument)
   : textDocument(textDocument)
{
}

void ChatDelegate::paint(QPainter* painter, const QStyleOptionViewItem& option, const QModelIndex& index) const
{
   QStyleOptionViewItemV4 optionV4(option);
   this->initStyleOption(&optionV4, index);

   optionV4.state = option.state & (~QStyle::State_HasFocus);

   QStyle* style = optionV4.widget ? optionV4.widget->style() : QApplication::style();

   this->textDocument.setHtml(optionV4.text);
   this->textDocument.setTextWidth(optionV4.rect.width());

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

   QStyleOptionViewItemV4 optionV4 = option;
   initStyleOption(&optionV4, index);

   this->textDocument.setHtml(optionV4.text);
   this->textDocument.setTextWidth(optionV4.rect.width());
   QSize size(optionV4.rect.width(), this->textDocument.size().height()); // Width should be "doc.idealWidth()".
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

const int ChatWidget::DEFAULT_FONT_SIZE(8);

ChatWidget::ChatWidget(QSharedPointer<RCC::ICoreConnection> coreConnection, Emoticons& emoticons, PeerListModel& peerListModel, QWidget* parent) :
   MdiWidget(parent),
   ui(new Ui::ChatWidget),
   coreConnection(coreConnection),
   emoticons(emoticons),
   chatModel(coreConnection, peerListModel),
   chatDelegate(textDocument),
   autoScroll(true)
{
   this->init();
}

ChatWidget::ChatWidget(QSharedPointer<RCC::ICoreConnection> coreConnection, Emoticons& emoticons, PeerListModel& peerListModel, const QString& roomName, QWidget* parent) :
   MdiWidget(parent),
   ui(new Ui::ChatWidget),
   coreConnection(coreConnection),
   emoticons(emoticons),
   chatModel(coreConnection, peerListModel, roomName),
   chatDelegate(textDocument),
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

void ChatWidget::sendMessage()
{
   this->chatModel.sendMessage(this->ui->txtMessage->toHtml());
   this->ui->txtMessage->document()->setHtml(""); // We avoid the 'clear()' method because it removes all ressources (emoticon images).
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

/**
  * Update the format widgets depending of the cursor position.
  */
void ChatWidget::currentCharFormatChanged(const QTextCharFormat& charFormat)
{
   if (this->ui->txtMessage->textCursor().position() > 0 || this->ui->txtMessage->document()->characterCount() > 1)
   {
      this->disconnectFormatWidgets();

      this->setComboFontSize(charFormat.fontPointSize());

      this->ui->butBold->setChecked(charFormat.fontWeight() >= QFont::Bold);
      this->ui->butItalic->setChecked(charFormat.fontItalic());
      this->ui->butUnderline->setChecked(charFormat.fontUnderline());

      this->connectFormatWidgets();
   }
   else // Special case to avoid to reset the formatting when the cursor is put at the begining.
   {
      this->ui->txtMessage->disconnect(this, SIGNAL(currentCharFormatChanged(QTextCharFormat)));
      this->applyCurrentFormat();
      connect(this->ui->txtMessage, SIGNAL(currentCharFormatChanged(QTextCharFormat)), this, SLOT(currentCharFormatChanged(QTextCharFormat)));
   }
}

void ChatWidget::cursorPositionChanged()
{
   if (this->ui->txtMessage->textCursor().position() != 0)
   {
      this->disconnectFormatWidgets();
      this->ui->butColorBox->setColor(this->ui->txtMessage->textColor());
      this->connectFormatWidgets();
   }
   else
   {
      this->ui->txtMessage->disconnect(this, SIGNAL(cursorPositionChanged()));
      this->ui->txtMessage->setTextColor(this->ui->butColorBox->getCurrentColor());
      connect(this->ui->txtMessage, SIGNAL(cursorPositionChanged()), this, SLOT(cursorPositionChanged()));
   }
}

/**
  * Adjust the text edit size depending of the size of each lines.
  */
void ChatWidget::textChanged()
{
   this->ui->txtMessage->setFixedHeight(this->ui->txtMessage->document()->size().height());
}

void ChatWidget::setFocusTxtMessage()
{
   this->ui->txtMessage->setFocus();
}

void ChatWidget::comboFontSizeChanged(int index)
{
   this->ui->txtMessage->setFontPointSize(this->ui->cmbFontSize->itemData(index).toInt());
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

void ChatWidget::setTextColor(QColor color)
{
   this->ui->txtMessage->setTextColor(color);
}

void ChatWidget::resetFormat()
{
   for (int i = 0; i < this->ui->cmbFontSize->count(); i++)
   {
      if (this->ui->cmbFontSize->itemData(i).toInt() == DEFAULT_FONT_SIZE)
      {
         this->ui->cmbFontSize->setCurrentIndex(i);
         break;
      }
   }

   this->ui->butBold->setChecked(false);
   this->ui->butItalic->setChecked(false);
   this->ui->butUnderline->setChecked(false);

   this->ui->butColorBox->setColor(QColor(0, 0, 0));

   this->applyCurrentFormat();
}

void ChatWidget::emoticonsButtonToggled(bool checked)
{
   L_DEBU(QString("emoticonsButtonToggled: %1").arg(checked));

   if (checked)
   {
      this->fuck = true;
      QAbstractButton* sender = dynamic_cast<QAbstractButton*>(this->sender());
         this->displayEmoticons(this->mapToGlobal(sender->pos()), sender->size());
      this->fuck = false;
   }
}

void ChatWidget::emoticonsWindowHidden()
{
   // I know it's bad but I didn't find another solution.
   // The issue is when the emoticons window is displayed and the user press on the emoticons button again. In this
   // case the window 'hidden' signal is triggered before the button 'toggled' signal, so the button is set to unchecked before it is pressed again...
   QTimer::singleShot(100, this, SLOT(emoticonsWindowHiddenDelayed()));
}

void ChatWidget::emoticonsWindowHiddenDelayed()
{
   L_DEBU(QString("emoticonsWindowHidden, isChecked: %1").arg(this->ui->butEmoticons->isChecked()));
   if (this->ui->butEmoticons->isChecked())
      this->ui->butEmoticons->setChecked(false);
}

void ChatWidget::insertEmoticon(const QString& theme, const QString& emoticonName)
{
   if (!this->ui->txtMessage->textCursor().atBlockStart())
      this->ui->txtMessage->insertPlainText(" ");

   this->ui->txtMessage->insertHtml(QString("<img src=\"%1\" />").arg(buildUrlEmoticon(theme, emoticonName).toString()));

   this->ui->txtMessage->insertPlainText(" ");
}

void ChatWidget::defaultEmoticonThemeChanged(const QString& theme)
{
   SETTINGS.set("default_emoticon_theme", theme);
   SETTINGS.save();
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
   else
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
      if (keyEvent->key() == Qt::Key_Return && !(keyEvent->modifiers() & Qt::ShiftModifier))
      {
         this->sendMessage();
         return true;
      }
   }

   return MdiWidget::eventFilter(obj, event);
}

void ChatWidget::init()
{
   this->ui->setupUi(this);

   this->emoticonsWidget = new EmoticonsWidget(this->emoticons, this);
   this->emoticonsWidget->setWindowFlags(Qt::Popup);
   this->emoticonsWidget->setDefaultTheme(SETTINGS.get<QString>("default_emoticon_theme"));

   foreach (QString theme, this->emoticons.getThemes())
      foreach (QString smileName, this->emoticons.getSmileNames(theme))
      {
         const QPixmap& image = this->emoticons.getSmileImage(theme, smileName);
         const QUrl& url = buildUrlEmoticon(theme, smileName);
         this->textDocument.addResource(QTextDocument::ImageResource, url, image);
         this->ui->txtMessage->document()->addResource(QTextDocument::ImageResource, url, image);
      }

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

   int defaultFontIndex = 0;
   for (int fontSize = 6, i = 0; fontSize <= 28; fontSize++, i++)
   {
      this->ui->cmbFontSize->addItem(QString::number(fontSize), fontSize);
      if (fontSize >= 12)
         fontSize++;
      if (fontSize == DEFAULT_FONT_SIZE)
         defaultFontIndex = i;
   }
   this->ui->cmbFontSize->setCurrentIndex(defaultFontIndex);
   this->ui->txtMessage->setFontPointSize(DEFAULT_FONT_SIZE);

   this->applyCurrentFormat();

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

   connect(this->ui->txtMessage, SIGNAL(currentCharFormatChanged(QTextCharFormat)), this, SLOT(currentCharFormatChanged(QTextCharFormat)));
   connect(this->ui->txtMessage, SIGNAL(cursorPositionChanged()), this, SLOT(cursorPositionChanged()));
   connect(this->ui->txtMessage, SIGNAL(textChanged()), this, SLOT(textChanged()));

   connect(this->ui->cmbFontSize, SIGNAL(currentIndexChanged(int)), this, SLOT(setFocusTxtMessage()));
   connect(this->ui->butBold, SIGNAL(clicked()), this, SLOT(setFocusTxtMessage()));
   connect(this->ui->butItalic, SIGNAL(clicked()), this, SLOT(setFocusTxtMessage()));
   connect(this->ui->butUnderline, SIGNAL(clicked()), this, SLOT(setFocusTxtMessage()));
   connect(this->ui->butColorBox, SIGNAL(clicked()), this, SLOT(setFocusTxtMessage()));

   connect(this->ui->butResetFormat, SIGNAL(clicked()), this, SLOT(setFocusTxtMessage()));
   connect(this->ui->butResetFormat, SIGNAL(clicked()), this, SLOT(resetFormat()));

   connect(this->ui->butEmoticons, SIGNAL(toggled(bool)), this, SLOT(emoticonsButtonToggled(bool)));
   connect(this->emoticonsWidget, SIGNAL(hidden()), this, SLOT(emoticonsWindowHidden()));
   connect(this->emoticonsWidget, SIGNAL(emoticonChoosen(QString, QString)), this, SLOT(insertEmoticon(QString, QString)));
   connect(this->emoticonsWidget, SIGNAL(defaultThemeChanged(QString)), this, SLOT(defaultEmoticonThemeChanged(QString)));

   this->connectFormatWidgets();

   this->ui->txtMessage->installEventFilter(this);

   // ALT-<num> is used to switch bewteen windows, we tell the text edit widget to ignore them.
   for (char c = '0'; c <= '9'; c++)
      this->ui->txtMessage->addIgnoreKeyCombination({ Qt::AltModifier, c });

this->setNewMessageState(false);
}

void ChatWidget::applyCurrentFormat()
{
   this->ui->txtMessage->setFontPointSize(this->ui->cmbFontSize->itemData(this->ui->cmbFontSize->currentIndex()).toInt());
   this->ui->txtMessage->setFontWeight(this->ui->butBold->isChecked() ? QFont::Bold : QFont::Normal);
   this->ui->txtMessage->setFontItalic(this->ui->butItalic->isChecked());
   this->ui->txtMessage->setFontUnderline(this->ui->butUnderline->isChecked());
   this->ui->txtMessage->setTextColor(this->ui->butColorBox->getCurrentColor());
}

void ChatWidget::connectFormatWidgets()
{
   connect(this->ui->cmbFontSize, SIGNAL(currentIndexChanged(int)), this, SLOT(comboFontSizeChanged(int)));
   connect(this->ui->butBold, SIGNAL(toggled(bool)), this, SLOT(setBold(bool)));
   connect(this->ui->butItalic, SIGNAL(toggled(bool)), this, SLOT(setItalic(bool)));
   connect(this->ui->butUnderline, SIGNAL(toggled(bool)), this, SLOT(setUnderline(bool)));
   connect(this->ui->butColorBox, SIGNAL(colorChanged(QColor)), this, SLOT(setTextColor(QColor)));
}

void ChatWidget::disconnectFormatWidgets()
{
   this->ui->cmbFontSize->disconnect(this, SLOT(comboFontSizeChanged(int)));
   this->ui->butBold->disconnect(this, SLOT(setBold(bool)));
   this->ui->butItalic->disconnect(this, SLOT(setItalic(bool)));
   this->ui->butUnderline->disconnect(this, SLOT(setUnderline(bool)));
   this->ui->butColorBox->disconnect(this, SLOT(setTextColor(QColor)));
}

void ChatWidget::setComboFontSize(int fontSize)
{
   int defaultIndex = 0;
   for (int i = 0; i < this->ui->cmbFontSize->count(); i++)
   {
      const int currentFontSize = this->ui->cmbFontSize->itemData(i).toInt();
      if (currentFontSize == fontSize)
      {
         this->ui->cmbFontSize->setCurrentIndex(i);
         return;
      }
      else if (currentFontSize == DEFAULT_FONT_SIZE)
      {
         defaultIndex = i;
      }
   }

   this->ui->cmbFontSize->setCurrentIndex(defaultIndex);
}

void ChatWidget::displayEmoticons(const QPoint& positionSender, const QSize& sizeSender)
{
   this->emoticonsWidget->show();

   this->emoticonsWidget->move(positionSender.x() + sizeSender.width(), positionSender.y() + sizeSender.height() - this->emoticonsWidget->height());

   QDesktopWidget* desktop = QApplication::desktop();
   QRect desktopGeom = desktop->availableGeometry(this);
   if (this->emoticonsWidget->pos().y() < 0)
      this->emoticonsWidget->move(this->emoticonsWidget->pos().x(), 0);

   if (this->emoticonsWidget->pos().x() + this->emoticonsWidget->width() > desktopGeom.width())
      this->emoticonsWidget->move(positionSender.x() - this->emoticonsWidget->width(), this->emoticonsWidget->pos().y());
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
         this->setWindowIcon(QIcon(":/icons/ressources/chat_new_mess.png"));
      }
      else
      {
         this->setWindowIcon(QIcon(":/icons/ressources/chat_room_new_mess.png"));
      }
   }
   else
   {
      if (this->chatModel.isMainChat())
      {
         this->setWindowIcon(QIcon(":/icons/ressources/chat.png"));
      }
      else
      {
         this->setWindowIcon(QIcon(":/icons/ressources/chat_room.png"));
      }
   }
}

QUrl ChatWidget::buildUrlEmoticon(const QString& theme, const QString& emoticonName)
{
   return QUrl(QString("emoticons://%1/%2").arg(theme).arg(emoticonName));
}
