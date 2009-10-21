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
      Logger(QTextStream* stream, const QString& name);

      void log(const QString& message, Severity severity);
      void log(const ILoggable& object, Severity severity);

   private:
      QString name;
      QTextStream* out;
      static QMutex mutex;
   };
}
#endif
