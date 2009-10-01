#ifndef LOGMANAGER_LOGGER_H
#define LOGMANAGER_LOGGER_H

#include <ILogger.h>

namespace LogManager
{
   class Logger : public ILogger
   {  
   public:
      void log(const QString& message, Severity severity);
      void log(const ILoggable& object, Severity severity);     
   };
}
#endif
