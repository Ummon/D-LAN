#ifndef LOGMANAGER_LOGGER_H
#define LOGMANAGER_LOGGER_H

#include <QString>
#include <QTextStream>
#include <QDir>
#include <QMutex>

#include <ILogger.h>

namespace LM
{
   const int NB_LOGFILE = 10; ///< The maximum number of log file, if there is more file the oldest will be deleted.

   class Logger : public ILogger
   {
   public:
      Logger(const QString& name);
      ~Logger();

      void log(const QString& message, Severity severity);
      void log(const ILoggable& object, Severity severity);

   private:
      void deleteOldestLog(const QDir& logDir);

      QString name;

      static QTextStream* out;
      static QMutex mutex;
      static int nbLogger;
   };
}
#endif
