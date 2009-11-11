#ifndef LOGMANAGER_LOGGER_H
#define LOGMANAGER_LOGGER_H

#include <QString>
#include <QTextStream>
#include <QMutex>

#include <ILogger.h>

namespace LM
{
   class Logger : public ILogger
   {
   public:
      Logger(const QString& name);
      ~Logger();

      void log(const QString& message, Severity severity);
      void log(const ILoggable& object, Severity severity);

   private:
      QString name;

      static QTextStream* out;
      static QMutex mutex;
      static int nbLogger;
   };
}
#endif
