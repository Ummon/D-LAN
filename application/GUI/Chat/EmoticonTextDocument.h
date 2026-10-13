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

#include <QList>
#include <QRectF>
#include <QTextCharFormat>
#include <QTextDocument>

#include <Emoticons/Emoticons.h>

namespace GUI
{
   class EmoticonTextDocument : public QTextDocument
   {
   public:
      // A mention, the "@nick" of a peer answered by a message, is sent enclosed in a tag having this title:
      // '<span title="mention" ..>@nick</span>'. Qt keeps the title of a tag as the tooltip of the format of its
      // text, it's the only attribute of a tag left once a message is parsed.
      static const QString MENTION_TITLE;
      static bool isMention(const QTextCharFormat& format);

      EmoticonTextDocument(const Emoticons& emoticons, QObject* parent = nullptr);
      void setMarkdown(const QString& markdown);
      static QString toClipboardMarkdown(const QString& markdown, const Emoticons& emoticons);
      QList<QRectF> mentionRects() const;

   protected:
      QVariant loadResource(int type, const QUrl& name) override;

   private:
      const Emoticons& emoticons;
   };
}
