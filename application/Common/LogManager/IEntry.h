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
  
#ifndef LOGMANAGER_IENTRY_H
#define LOGMANAGER_IENTRY_H

#include <QString>
#include <QDateTime>

namespace LM
{
   enum Severity
   {
      SV_FATAL_ERROR = 0x01,
      SV_ERROR = 0x02,
      SV_WARNING = 0x04,
      SV_DEBUG = 0x08,
      SV_END_USER = 0x10,
      SV_UNKNOWN = 0x20
   };

   class IEntry
   {
   public:
      virtual ~IEntry() {}

      virtual QDateTime getDate() const = 0;
      virtual QString getDateStr(bool withMs = true) const = 0;
      virtual Severity getSeverity() const = 0;
      virtual QString getSeverityStr() const = 0;
      virtual QString getName() const = 0;
      virtual QString getThread() const = 0;
      virtual QString getSource() const = 0;
      virtual QString getMessage() const = 0;
      virtual QString getMessageWithLF() const = 0;
   };
}
#endif
