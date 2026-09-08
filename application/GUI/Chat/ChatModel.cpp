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

#include <Chat/ChatModel.h>
using namespace GUI;

#include <QtAlgorithms>
#include <QStringBuilder>
#include <QImage>
#include <QResource>
#include <QDomDocument>
#include <QTextDocument>
#include <QTextBlock>
#include <QRegularExpression>

#include <Protos/common.pb.h>

#include <Common/ProtoHelper.h>
#include <Common/Global.h>
#include <Common/Settings.h>

#include <Log.h>

/**
  * Escape the characters CommonMark gives a meaning to, so a string can be inserted into a message without
  * being interpreted. The nicks come from remote peers, a nick like "[x](http://...)" would otherwise put a
  * clickable link into the messages shown to every user, see 'ChatDelegate::paint(..)'.
  */
static QString escapeMarkdown(const QString& str)
{
   QString result;
   result.reserve(str.size());

   for (const QChar c : str)
   {
      // CommonMark: a backslash escapes any ASCII punctuation character, which Qt splits between the
      // punctuation and the symbol categories.
      if (c.unicode() < 0x80 && (c.isPunct() || c.isSymbol()))
         result += '\\';
      result += c;
   }

   return result;
}

static bool needsSeparateSenderLine(const QString& markdown)
{
   // Qt does not consistently flag single-line indented code in the parsed block format.
   int indentation = 0;
   for (const QChar c : markdown)
   {
      if (c == '\t')
         return true;
      if (c != ' ')
         break;
      if (++indentation == 4)
         return true;
   }

   QTextDocument document;
   document.setMarkdown(markdown);
   // Qt wraps Markdown source at a fixed column, particularly around long emoticon URLs.
   // Those soft wraps still form one paragraph and must not move the sender onto another line.
   if (document.blockCount() > 1)
      return true;

   // Reference definitions can disappear from the parsed document, but need their own line.
   static const QRegularExpression referenceDefinition("^\\s*\\[[^\\r\\n]+\\]:");
   if (referenceDefinition.match(markdown).hasMatch())
      return true;

   const QTextBlock firstBlock = document.firstBlock();
   const QTextBlockFormat format = firstBlock.blockFormat();
   return firstBlock.text().isEmpty() || firstBlock.textList() || format.headingLevel() > 0 ||
      format.nonBreakableLines() || format.intProperty(QTextFormat::BlockQuoteLevel) > 0 ||
      format.hasProperty(QTextFormat::BlockTrailingHorizontalRulerWidth);
}

ChatModel::ChatModel(
   QSharedPointer<RCC::ICoreConnection> coreConnection,
   PeerListModel& peerListModel,
   // const Emoticons& emoticons,
   const QString& roomName
) :
   coreConnection(coreConnection),
   peerListModel(peerListModel),
   // emoticons(emoticons),
   roomName(roomName)
   // regexMatchMessageContent("<p[^>]+>"),
   // regexMatchFirstBR("^\\s*<br[^>]*>"),
   // regexMatchLastBR("<br[^>]*>\\s*$")
{
   connect(
      this->coreConnection.data(),
      &RCC::ICoreConnection::newChatMessages,
      this,
      &ChatModel::newChatMessages
   );
}

bool ChatModel::isMainChat() const
{
   return this->roomName.isEmpty();
}

QString ChatModel::getRoomName() const
{
   return this->roomName;
}

/**
  * Returns the most relevant peers from the last messages. The peers which have replied to us are put first.
  */
QList<QPair<Common::Hash, QString>> ChatModel::getSortedOtherPeersByRelevance() const
{
   QList<QPair<Common::Hash, QString>> result;
   QSet<Common::Hash> processedPeers;
   const Common::Hash ourself = this->coreConnection->getRemoteID();
   QHash<Common::Hash, QString> latestNicks;
   for (auto i = this->messages.crbegin(); i != this->messages.crend(); ++i)
      if (!latestNicks.contains(i->peerID))
         latestNicks.insert(i->peerID, i->nick);

   // Relevance determines peer order, not which historical nickname is displayed.
   const auto appendPeer = [&](const Common::Hash& peerID) {
      if (peerID == ourself || processedPeers.contains(peerID))
         return;
      QString nick = this->peerListModel.getNick(peerID);
      if (nick.isEmpty())
         nick = latestNicks.value(peerID);
      if (!nick.isEmpty())
      {
         result.append(qMakePair(peerID, nick));
         processedPeers.insert(peerID);
      }
   };

   // First level: peers answering to us.
   for (auto i = this->messages.crbegin(); i != this->messages.crend(); ++i)
      if (i->answeringToUs)
         appendPeer(i->peerID);

   // Second level: peers which have posted a message.
   for (auto i = this->messages.crbegin(); i != this->messages.crend(); ++i)
      if (!i->answeringToUs)
         appendPeer(i->peerID);

   // Third level: the rest.
   for (int i = 0; i < this->peerListModel.rowCount(); ++i)
      appendPeer(this->peerListModel.getPeerID(i));

   return result;
}

QString ChatModel::getNick(const Common::Hash& id) const
{
   for (int i = this->messages.size() - 1; i >= 0; --i)
   {
      if (this->messages[i].peerID == id)
         return this->messages[i].nick;
   }

   return QString();
}

/**
  * Return a string with all the field: "[<date>] <nick>: <message>".
  */
QString ChatModel::getLineStr(int row) const
{
   if (row < 0 || row >= this->messages.size())
      return QString();

   QString result = this->formatMessage(this->messages[row]);
   // if (!withHTML)
   // {
   //    QDomDocument doc;
   //    doc.setContent(result);

   //    QDomElement HTMLElement = doc.firstChildElement("html");
   //    QDomElement BodyElement = HTMLElement.firstChildElement("body");
   //    QDomElement currentElement = BodyElement.firstChildElement();

   //    while (!currentElement.isNull())
   //    {
   //       QDomElement nextElement = currentElement.nextSiblingElement();
   //       if (currentElement.tagName() == "img")
   //       {
   //          QStringList srcEmoticon = currentElement.attribute("src").split('/', Qt::SkipEmptyParts);
   //          if (srcEmoticon.count() == 3)
   //          {
   //             QStringList emoticonSymbols = this->emoticons.getSmileSymbols(srcEmoticon[1], srcEmoticon[2]);
   //             if (!emoticonSymbols.isEmpty())
   //                currentElement.parentNode().replaceChild(doc.createTextNode(emoticonSymbols.first()), currentElement);
   //          }
   //       }
   //       else if (currentElement.tagName() == "span")
   //       {
   //          QDomElement innerSpanElement = currentElement.firstChildElement();
   //          while (!innerSpanElement.isNull())
   //          {
   //             QDomElement nextInnerSpanElement = innerSpanElement.nextSiblingElement();
   //             if (innerSpanElement.tagName() == "br")
   //                innerSpanElement.parentNode().replaceChild(doc.createTextNode("\n"), innerSpanElement);
   //             innerSpanElement = nextInnerSpanElement;
   //          }
   //       }
   //       currentElement = nextElement;
   //    }

   //    return BodyElement.text();
   // }

   return result;
}

Common::Hash ChatModel::getPeerID(int row) const
{
   if (row < 0 || row >= this->messages.size())
      return Common::Hash();

   return this->messages[row].peerID;
}

bool ChatModel::isMessageIsOurs(int row) const
{
   if (row < 0 || row >= this->messages.size())
      return false;

   return this->messages[row].peerID == this->coreConnection->getRemoteID();
}

int ChatModel::rowCount(const QModelIndex& parent) const
{
   return parent.isValid() ? 0 : this->messages.size();
}

int ChatModel::columnCount(const QModelIndex& parent) const
{
   return parent.isValid() ? 0 : 1;
}

QVariant ChatModel::data(const QModelIndex& index, int role) const
{
   if (!this->isValidMessageIndex(index))
      return QVariant();

   switch (role)
   {
   case Qt::DisplayRole:
      return this->formatMessage(this->messages[index.row()]);
   }

   return QVariant();
}

bool ChatModel::isValidMessageIndex(const QModelIndex& index) const
{
   return index.isValid() && index.model() == this && index.column() == 0 &&
      index.row() >= 0 && index.row() < this->messages.size();
}

void ChatModel::sendMessage(const QString& message, const QList<Common::Hash>& peerIDsAnswered, quint64 draftRevision)
{
   const QString trimmedMessage = message.trimmed();

   if (trimmedMessage.isEmpty())
      return;

   this->sendRawMessage(trimmedMessage, peerIDsAnswered, draftRevision);
}
void ChatModel::sendRawMessage(const QString& message, const QList<Common::Hash>& peerIDsAnswered, quint64 draftRevision)
{
   QSharedPointer<RCC::ISendChatMessageResult> result =
      this->coreConnection->sendChatMessage(message, this->roomName, peerIDsAnswered);

   // Carry each draft's revision with its result, even when replies arrive out of order.
   connect(result.data(), &RCC::ISendChatMessageResult::result, this, [this, draftRevision](const Protos::GUI::ChatMessageResult& result) {
      this->result(result, draftRevision);
   });
   connect(result.data(), &Common::Timeoutable::timeout, this, [this, draftRevision]() {
      this->resultTimeout(draftRevision);
   });
   this->results << result;
   result->start();
}

/*Qt::ItemFlags ChatModel::flags(const QModelIndex& index) const
{
   if (index.column() == 0)
      return Qt::ItemIsSelectable | Qt::ItemIsEnabled;
   else
      return Qt::ItemIsSelectable | Qt::ItemIsEditable | Qt::ItemIsEnabled;
}*/

void ChatModel::newChatMessages(const Protos::Common::ChatMessages& messages)
{
   if (messages.messages_size() == 0)
      return;

   QString roomName = QString::fromStdString(messages.messages(0).chat_room());
   if (roomName != this->roomName)
      return;

   const Common::Hash& ourPeerID = this->coreConnection->getRemoteID();

   int j = this->messages.size();
   QList<Message> toInsert;

   for (int i = messages.messages_size() - 1; i >= 0; i--)
   {
      const Common::Hash peerID(messages.messages(i).peer_id().hash());

      bool isTheMessageAnsweringToUs = false;
      for (int j = 0; j < messages.messages(i).peer_ids_answer_size(); j++)
         if (Common::Hash(messages.messages(i).peer_ids_answer(j).hash()) == ourPeerID)
         {
            isTheMessageAnsweringToUs = true;
            break;
         }

      Message message {
         messages.messages(i).id(),
         peerID,
         isTheMessageAnsweringToUs,
         this->peerListModel.getNick(peerID, QString::fromStdString(messages.messages(i).peer_nick())),
         QDateTime::fromMSecsSinceEpoch(messages.messages(i).time()),
         QString::fromStdString(messages.messages(i).message())
      };
      message.separateSenderLine = needsSeparateSenderLine(message.message);

      int previousJ = j;
      while (j > 0 && this->messages[j-1].dateTime > message.dateTime)
         j--;

      if (previousJ != j && !toInsert.isEmpty())
      {
         this->beginInsertRows(QModelIndex(), previousJ, previousJ + toInsert.size() - 1);
         for (QListIterator<Message> k(toInsert); k.hasNext();)
            this->messages.insert(previousJ, k.next());
         this->endInsertRows();
         toInsert.clear();
      }

      toInsert << message;

      // Special case for the last message.
      if (i == 0)
      {
         this->beginInsertRows(QModelIndex(), j, j + toInsert.size() - 1);
         for (QListIterator<Message> k(toInsert); k.hasNext();)
            this->messages.insert(j, k.next());
         this->endInsertRows();
      }
   }

   static const quint32 MAX_NB_MESSAGES = SETTINGS.get<quint32>("max_chat_message_displayed");
   const int nbMessageToDelete = this->messages.size() - MAX_NB_MESSAGES;

   if (nbMessageToDelete > 0)
   {
      this->beginRemoveRows(QModelIndex(), 0, nbMessageToDelete - 1);
      this->messages.erase(this->messages.begin(), this->messages.begin() + nbMessageToDelete);
      this->endRemoveRows();
   }
}

void ChatModel::result(const Protos::GUI::ChatMessageResult& result, quint64 draftRevision)
{
   switch (result.status())
   {
   case Protos::GUI::ChatMessageResult::OK:
      emit sendMessageStatus(OK, draftRevision);
      break;

   case Protos::GUI::ChatMessageResult::MESSAGE_TOO_LARGE:
      emit sendMessageStatus(MESSAGE_TOO_LARGE, draftRevision);
      break;

   default:
      emit sendMessageStatus(ERROR_UNKNOWN, draftRevision);
      break;
   }

   this->removeResult(qobject_cast<RCC::ISendChatMessageResult*>(this->sender()));
}

void ChatModel::resultTimeout(quint64 draftRevision)
{
   emit sendMessageStatus(TIMEOUT, draftRevision);
   this->removeResult(qobject_cast<RCC::ISendChatMessageResult*>(this->sender()));
}

/**
  * Forget the given result, it has been answered or has timed out.
  *
  * The removal is deferred for two reasons:
  *  - Both callers are slots called synchronously from a signal emitted by 'result' itself, so dropping the
  *    last reference here would destroy it, and its timer, while that timer is being dispatched. Qt then
  *    silently drops the pending timers of other objects: with two messages in flight, the timeout of the
  *    second one never fires. Same reason as the 'deleteLater()' of the commit 9476210f.
  *  - The results aren't necessarily answered in the order they were sent, so the one to remove has to be
  *    looked up: 'removeFirst()' used to drop whichever was at the front, and to be undefined on an empty list.
  */
void ChatModel::removeResult(const RCC::ISendChatMessageResult* result)
{
   if (!result)
      return;

   QMetaObject::invokeMethod(
      this,
      [this, result]()
      {
         for (int i = 0; i < this->results.size(); i++)
            if (this->results[i].data() == result)
            {
               this->results.removeAt(i);
               return;
            }
      },
      Qt::QueuedConnection
   );
}

QString ChatModel::formatMessage(const Message& message) const
{
   const QDateTime now = QDateTime::currentDateTime();

   return
      QString()
         .append(
            now.date() == message.dateTime.date() ?
              message.dateTime.toString("[HH:mm:ss] ")
            : message.dateTime.toString("[%1 HH:mm:ss] ").arg(message.dateTime.date().toString(Qt::TextDate)))
         .append("*").append(escapeMarkdown(message.nick)).append("*:")
         .append(message.separateSenderLine ? "\n\n" : " ")
         .append(message.message);
}
