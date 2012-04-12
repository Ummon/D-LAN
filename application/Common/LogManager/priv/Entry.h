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
  
#ifndef LOGMANAGER_ENTRY_H
#define LOGMANAGER_ENTRY_H

#include <QString>
#include <QDateTime>
#include <QRegExp>

#include <IEntry.h>

namespace LM
{
   class Entry : public IEntry
   {
      static const QString DATE_TIME_FORMAT;
      static const QString DATE_TIME_FORMAT_WITH_MS;
      static const QString SEVERITIES_STR[];
      static QRegExp lineRegExp;

   public:
      Entry(const QString& line);
      Entry(const QDateTime& dateTime, Severity severity, const QString& name, const QString& thread, const QString& source, const QString& message);

      QString toStrLine() const;

      QDateTime getDate() const;
      QString getDateStr(bool withMs = true) const;
      Severity getSeverity() const;
      QString getSeverityStr() const;
      QString getName() const;
      QString getThread() const;
      QString getSource() const;
      QString getMessage() const;
      QString getMessageWithLF() const;

   private:
      static QString severityToStr(Severity severity);
      static Severity severityFromStr(const QString& severity);

      QDateTime date;
      Severity severity;
      QString name;
      QString thread;
      QString source;
      QString message;
   };
}
#endif
