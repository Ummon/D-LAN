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
  
#ifndef LOGMANAGER_ILOGGER_H
#define LOGMANAGER_ILOGGER_H

#include <QString>

#include <Common/LogManager/ILoggable.h>
#include <Common/LogManager/IEntry.h>

namespace LM
{
   /**
     * Example of a log line :
     * 04-05-2010 17:16:44 [User] {Logger1} (896) <Tests.cpp:30> : This is an user message log
     * Line return (\n) are replaced by the string "<lf>".
     * All strings are logged in UTF-8.
     */
   class ILogger
   {
   public:
      virtual ~ILogger() {}

      virtual void log(const QString& message, Severity severity, const char* filename = 0, int line = 0) const = 0;
      virtual void log(const ILoggable& object, Severity severity, const char* filename = 0, int line = 0) const = 0;
   };
}
#endif
