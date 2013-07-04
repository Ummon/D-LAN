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

#include <Protos/common.pb.h>

#include <Common/ProtoHelper.h>
#include <Common/Global.h>
#include <Common/Settings.h>

#include <Log.h>

ChatModel::ChatModel(QSharedPointer<RCC::ICoreConnection> coreConnection, PeerListModel& peerListModel, const Emoticons& emoticons, const QString& roomName) :
   coreConnection(coreConnection),
   peerListModel(peerListModel),
   emoticons(emoticons),
   roomName(roomName),
   regexMatchMessageContent("<p[^>]+>"),
   regexMatchFirstBR("^\\s*<br[^>]*>"),
   regexMatchLastBR("<br[^>]*>\\s*$")
{
   connect(this->coreConnection.data(), SIGNAL(newChatMessages(const Protos::Common::ChatMessages&)), this, SLOT(newChatMessages(const Protos::Common::ChatMessages&)));
}

bool ChatModel::isMainChat() const
{
   return this->roomName.isEmpty();
}

QString ChatModel::getRoomName() const
{
   return this->roomName;
}


QList<QPair<Common::Hash, QString> > ChatModel::getRelevantLastPeers() const
{
   QList<QPair<Common::Hash, QString> > result;

   QListIterator<Message> i(this->messages);
   i.toBack();

   while (i.hasPrevious())
   {
      const Message& message = i.previous();
      if (message.answeringToUs)
         result << QPair<Common::Hash, QString>(message.peerID, message.nick);
   }

   i.toBack();
   while (i.hasPrevious())
   {
      const Message& message = i.previous();
      if (!message.answeringToUs)
         result << QPair<Common::Hash, QString>(message.peerID, message.nick);
   }

   return result;
}

/**
  * Return a string with all the field: "[<date>] <nick>: <message>".
  */
QString ChatModel::getLineStr(int row, bool withHTML) const
{
   QString result = this->formatMessage(this->messages[row]);
   if (!withHTML)
   {
      QDomDocument doc;
      QString* message = nullptr;
      doc.setContent(result, message);

      QDomElement HTMLElement = doc.firstChildElement("html");
      QDomElement BodyElement = HTMLElement.firstChildElement("body");
      QDomElement currentElement = BodyElement.firstChildElement();

      while (!currentElement.isNull())
      {
         QDomElement nextElement = currentElement.nextSiblingElement();
         if (currentElement.tagName() == "img")
         {
            QStringList srcEmoticon = currentElement.attribute("src").split('/', QString::SkipEmptyParts);
            if (srcEmoticon.count() == 3)
            {
               QStringList emoticonSymbols = this->emoticons.getSmileSymbols(srcEmoticon[1], srcEmoticon[2]);
               if (!emoticonSymbols.isEmpty())
                  currentElement.parentNode().replaceChild(doc.createTextNode(emoticonSymbols.first()), currentElement);
            }
         }
         else if (currentElement.tagName() == "span")
         {
            QDomElement innerSpanElement = currentElement.firstChildElement();
            while (!innerSpanElement.isNull())
            {
               QDomElement nextInnerSpanElement = innerSpanElement.nextSiblingElement();
               if (innerSpanElement.tagName() == "br")
                  innerSpanElement.parentNode().replaceChild(doc.createTextNode("\n"), innerSpanElement);
               innerSpanElement = nextInnerSpanElement;
            }
         }
         currentElement = nextElement;
      }

      return BodyElement.text();
   }

   return result;
}

bool ChatModel::isMessageIsOurs(int row) const
{
   if (row >= this->messages.size())
      return false;

   return this->messages[row].peerID == this->coreConnection->getRemoteID();
}

int ChatModel::rowCount(const QModelIndex& parent) const
{
   return this->messages.size();
}

int ChatModel::columnCount(const QModelIndex& parent) const
{
   return 1;
}

QVariant ChatModel::data(const QModelIndex& index, int role) const
{
   if (index.row() >= this->messages.size())
      return QVariant();

   switch (role)
   {
   case Qt::DisplayRole:
      return this->formatMessage(this->messages[index.row()]);
   }

   return QVariant();
}

void ChatModel::sendMessage(const QString& message, const QList<Common::Hash>& peerIDsAnswered)
{
   if (message.isEmpty())
      return;

   // Remove the HTML header and footer with a regular expression ... I know: http://stackoverflow.com/a/1732454 ...
   const int beginning = this->regexMatchMessageContent.indexIn(message);
   const int end = message.lastIndexOf("</p>");

   if (beginning != -1 && end > beginning + this->regexMatchMessageContent.matchedLength())
   {
      QString innerMessage = message.mid(beginning + this->regexMatchMessageContent.matchedLength(), end - beginning - this->regexMatchMessageContent.matchedLength()).trimmed();

      innerMessage.remove(this->regexMatchFirstBR);
      innerMessage.remove(this->regexMatchLastBR);

      if (!innerMessage.isEmpty())
         this->coreConnection->sendChatMessage(innerMessage, this->roomName);
   }
   else
   {
      const QString trimmedMessage = message.trimmed();
      if (!trimmedMessage.isEmpty())
         this->coreConnection->sendChatMessage(trimmedMessage, this->roomName);
   }
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
   if (messages.message_size() == 0)
      return;

   QString roomName = messages.message(0).has_chat_room() ? Common::ProtoHelper::getStr(messages.message(0), &Protos::Common::ChatMessage::chat_room) : QString();
   if (roomName != this->roomName)
      return;

   const Common::Hash& ourPeerID = this->coreConnection->getRemoteID();

   int j = this->messages.size();
   int previousJ;
   QList<Message> toInsert;

   for (int i = messages.message_size() - 1; i >= 0; i--)
   {
      const Common::Hash peerID(messages.message(i).peer_id().hash());

      bool isTheMessageAnsweringToUs = false;
      for (int j = 0; j < messages.message(i).peer_ids_answer_size(); j++)
         if (Common::Hash(messages.message(i).peer_ids_answer(j).hash()) == ourPeerID)
         {
            isTheMessageAnsweringToUs = true;
            break;
         }

      Message message {
         messages.message(i).id(),
         peerID,
         isTheMessageAnsweringToUs,
         this->peerListModel.getNick(peerID, Common::ProtoHelper::getStr(messages.message(i), &Protos::Common::ChatMessage::peer_nick)),
         QDateTime::fromMSecsSinceEpoch(messages.message(i).time()),
         Common::ProtoHelper::getStr(messages.message(i), &Protos::Common::ChatMessage::message)
      };

      previousJ = j;
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

QString ChatModel::formatMessage(const Message& message) const
{
   return
      QString(
         "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0//EN\" \"http://www.w3.org/TR/REC-html40/strict.dtd\">"
         "<html><head><meta name=\"qrichtext\" content=\"1\" /><style type=\"text/css\">"
         "p, li { white-space: pre-wrap; }"
         "</style></head><body style=\" font-family:'Sans'; font-size:8pt; font-weight:400; font-style:normal;\">")
      .append(message.dateTime.toString("[HH:mm:ss] "))
      .append("<b>").append(message.nick).append("</b>: ")
      .append(message.message)
      .append("</body></html>");
}
