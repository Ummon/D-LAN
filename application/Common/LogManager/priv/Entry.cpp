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
  
#include <priv/Entry.h>
using namespace LM;

#include <QString>
#include <QStringList>

#include <Exceptions.h>

const QString Entry::DATE_TIME_FORMAT("yyyy-MM-dd HH:mm:ss");
const QString Entry::DATE_TIME_FORMAT_WITH_MS("yyyy-MM-dd HH:mm:ss.zzz");
const QString Entry::SEVERITIES_STR[] = {"Fatal", "Error", "Warning", "Debug", "User", "Unkown"};
QRegExp Entry::lineRegExp("(\\S{10} \\S{8})\\.(\\d{3}) \\[(.+)\\] \\{(.+)\\} \\((\\w+)\\) (?:<(\\S+:\\d+)> )?: (.*)");

Entry::Entry(const QString& line) :
   severity(SV_UNKNOWN)
{
   //lineRegExp.setMinimal(true); // Non-greedy.
   if (!Entry::lineRegExp.exactMatch(line))
      throw MalformedEntryLog(line);

   QStringList capturedTexts = Entry::lineRegExp.capturedTexts();

   if (capturedTexts.count() < 7)
      return;

   this->date = QDateTime::fromString(capturedTexts[1], Entry::DATE_TIME_FORMAT);
   this->date = this->date.addMSecs(capturedTexts[2].toInt());

   this->severity = Entry::severityFromStr(capturedTexts[3]);
   this->name = capturedTexts[4];
   this->thread = capturedTexts[5];

   if (capturedTexts.count() == 7)
      this->message = capturedTexts[6];
   else if (capturedTexts.count() == 8)
   {
      this->source = capturedTexts[6];
      this->message = capturedTexts[7];
   }
}

Entry::Entry(const QDateTime& dateTime, Severity severity, const QString& name, const QString& thread, const QString& source, const QString& message) :
   date(dateTime), severity(severity), name(name), thread(thread), source(source), message(message)
{
   this->message.replace('\n', "<lf>");
}

/**
  * Message endlines ('\n') are replaced by "<lf>" to guaranted one line per entry.
  */
QString Entry::toStrLine() const
{
   QString formatedMessage =
      QString(!this->source.isNull() ? "%1 [%2] {%3} (%4) <%5> : %6" : "%1 [%2] {%3} (%4) : %5").arg
      (
         this->getDateStr(),
         Entry::severityToStr(this->severity),
         this->name,
         this->thread
      );

   if (!this->source.isNull())
      formatedMessage = formatedMessage.arg(this->source);

   return formatedMessage.arg(this->message);
}

QDateTime Entry::getDate() const
{
   return this->date;
}

QString Entry::getDateStr(bool withMs) const
{
   if (withMs)
      return date.toString(DATE_TIME_FORMAT_WITH_MS);
   else
      return date.toString(DATE_TIME_FORMAT);
}

Severity Entry::getSeverity() const
{
   return this->severity;
}

QString Entry::getSeverityStr() const
{
   return Entry::severityToStr(this->severity);
}

QString Entry::getName() const
{
   return this->name;
}

QString Entry::getThread() const
{
   return this->thread;
}

QString Entry::getSource() const
{
   return this->source;
}

QString Entry::getMessage() const
{
   return this->message;
}

QString Entry::getMessageWithLF() const
{
   QString messageCopy(this->message);
   messageCopy.replace("<lf>", "\n");
   return messageCopy;
}

QString Entry::severityToStr(Severity severity)
{
   switch (severity)
   {
   case SV_FATAL_ERROR: return Entry::SEVERITIES_STR[0];
   case SV_ERROR: return Entry::SEVERITIES_STR[1];
   case SV_WARNING: return Entry::SEVERITIES_STR[2];
   case SV_DEBUG: return Entry::SEVERITIES_STR[3];
   case SV_END_USER: return Entry::SEVERITIES_STR[4];
   case SV_UNKNOWN:
   default: return Entry::SEVERITIES_STR[5];
   }
}

Severity Entry::severityFromStr(const QString& severity)
{
   for (int i = 0; i < 6; i++)
      if (Entry::SEVERITIES_STR[i] == severity)
      {
         // Better than a 'while' and a 'shift'?
         switch (i)
         {
         case 0: return SV_FATAL_ERROR;
         case 1: return SV_ERROR;
         case 2: return SV_WARNING;
         case 3: return SV_DEBUG;
         case 4: return SV_END_USER;
         case 5: return SV_UNKNOWN;
         }
      }
   return SV_UNKNOWN;
}
