#ifndef LOGMANAGER_LOGGER_H
#define LOGMANAGER_LOGGER_H

#include <QString.h>
#include <QTextStream.h>

#include <ILogger.h>

namespace LogManager
{
   class Logger : public ILogger
   {  
   public:
      Logger(QTextStream* stream, const QString& name);            
      
      void log(const QString& message, Severity severity);
      void log(const ILoggable& object, Severity severity);   
      
   private:
      QTextStream* out;  
   };
}
#endif
