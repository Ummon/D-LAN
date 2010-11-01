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
      static QTextStream* out;
      static QMutex mutex;
      static int nbLogger;
      static QString logDirName;

   public:
      static void setLogDirName(const QString& logDirName);

      Logger(const QString& name);
      virtual ~Logger();

      void log(const QString& message, Severity severity, const char* filename = 0, int line = 0) const;
      void log(const ILoggable& object, Severity severity, const char* filename = 0, int line = 0) const;

   private:
      static void createFileLog();
      static void deleteOldestLog(const QDir& logDir);

      QString name;
   };
}
#endif
