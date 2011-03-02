/**
  * Aybabtu - A decentralized LAN file sharing software.
  * Copyright (C) 2010-2011 Greg Burri <greg.burri@gmail.com>
  *
  * This program is free software: you can redistribute it and/or modify
  * it under the terms of the GNU General Public License as published by
  * the Free Software Foundation, either version 3 of the License, or
  * (at your option) any later version.
  *
  * This program is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  *
  * You should have received a copy of the GNU General Public License
  * along with this program.  If not, see <http://www.gnu.org/licenses/>.
  */
  
#ifndef LOGMANAGER_LOGGER_H
#define LOGMANAGER_LOGGER_H

#include <QString>
#include <QTextStream>
#include <QDir>
#include <QMutex>
#include <QSharedPointer>

#include <priv/LoggerHook.h>
#include <ILogger.h>

namespace LM
{
   const int NB_LOGFILE = 10; ///< The maximum number of log file, if there is more file the oldest will be deleted.

   class LoggerHooks
   {
   public:
      int size() const;
      QWeakPointer<LoggerHook> operator<< (const QWeakPointer<LoggerHook> hook);
      QWeakPointer<LoggerHook> operator[] (int i);

   private:
      void removeDeletedHooks();

      QList< QWeakPointer<LoggerHook> > loggerHooks;
   };

   class Logger : public ILogger
   {
      static QTextStream out;
      static QFile file;

      static QMutex mutex;

      static QString logDirName;

      static LoggerHooks loggerHooks;

   public:
      static void setLogDirName(const QString& logDirName);
      static void addALoggerHook(QSharedPointer<LoggerHook> loggerHook);

      Logger(const QString& name);
      ~Logger();

      void log(const QString& message, Severity severity, const char* filename = 0, int line = 0) const;
      void log(const ILoggable& object, Severity severity, const char* filename = 0, int line = 0) const;

   private:
      static void createFileLog();
      static void deleteOldestLog(const QDir& logDir);

      QString name;
   };
}
#endif
