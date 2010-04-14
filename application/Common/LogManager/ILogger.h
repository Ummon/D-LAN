#ifndef LOGMANAGER_ILOGGER_H
#define LOGMANAGER_ILOGGER_H

#include <QString>

#include "ILoggable.h"
#include "IEntry.h"

namespace LM
{
   /**
     * Example of a log line :
     * 14-04-2010 23:07:15: [User] (3224) <Tests.cpp:24> Logger1: This is an user message log
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
