#ifndef LOGMANAGER_ILOGGER_H
#define LOGMANAGER_ILOGGER_H

#include <QString>

#include "ILoggable.h"
#include "IEntry.h"

namespace LM
{
   /**
     * Example of a log line :
     * 04-05-2010 17:16:44 [User] {Logger1} (896) <Tests.cpp:30> : This is an user message log
     * Line return (\n) are replaced by the string "<cr>".
     * All strings are logged in UTF-8.
     */
   class ILogger
   {
   public:
      virtual ~ILogger() {}

      virtual void log(const QString& message, Severity severity, const char* filename = 0, int line = 0) = 0;
      virtual void log(const ILoggable& object, Severity severity, const char* filename = 0, int line = 0) = 0;
   };
}
#endif
