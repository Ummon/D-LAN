#ifndef LOGMANAGER_ILOGGER_H
#define LOGMANAGER_ILOGGER_H

#include <QString>

#include <ILoggable.h>
#include <IEntry.h>

namespace LogManager
{
   class ILogger
   {
   public:
      virtual void log(const QString& message, Severity severity) = 0;
      virtual void log(const ILoggable& object, Severity severity) = 0;
   };
}
#endif
