#ifndef LOGMANAGER_LOGGER_H
#define LOGMANAGER_LOGGER_H

#include <qstring.h>
#include <qtextstream.h>

#include <ILogger.h>

namespace LogManager
{
   class Logger : public ILogger
   {
   public:
      Logger(const QString& name);
      Logger(QTextStream* stream, const QString& name);

      void log(const QString& message, Severity severity);
      void log(const ILoggable& object, Severity severity);

   private:
      QString name;
      QTextStream* out;
   };
}
#endif
