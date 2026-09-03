/**
  * D-LAN - A decentralized LAN file sharing software.
  * Copyright (C) 2010-2012 Greg Burri <greg.burri@gmail.com>
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

#include <priv/Logger.h>
using namespace LM;

#include <algorithm>

#include <QtDebug>
#include <QThread>
#include <QSharedPointer>

#include <Common/Constants.h>
#include <Common/Global.h>

#include <Constants.h>

#include <priv/Entry.h>

QWeakPointer<LoggerHook> LoggerHooks::operator<< (const QWeakPointer<LoggerHook> hook)
{
   this->loggerHooks << hook;
   return hook;
}

QList<QSharedPointer<LoggerHook>> LoggerHooks::takeAliveHooks()
{
   this->removeDeletedHooks();

   QList<QSharedPointer<LoggerHook>> aliveHooks;
   aliveHooks.reserve(this->loggerHooks.size());

   // A hook may expire right after 'removeDeletedHooks()', its owner isn't protected by 'Logger::mutex',
   // thus each strong reference must be tested.
   for (const QWeakPointer<LoggerHook>& hook : this->loggerHooks)
      if (const QSharedPointer<LoggerHook> hookStrongRef = hook.toStrongRef())
         aliveHooks << hookStrongRef;

   return aliveHooks;
}

void LoggerHooks::removeDeletedHooks()
{
   for (QMutableListIterator<QWeakPointer<LoggerHook>> i(this->loggerHooks); i.hasNext();)
      if (i.next().isNull())
         i.remove();
}

/////

Logger::State& Logger::getState()
{
   // Built on the first use: no dependency on the initialization order of the static objects of the other
   // compilation units. Never deleted, see the declaration.
   static State* const state = new State();
   return *state;
}

void Logger::setLogDirName(const QString& logDirName)
{
   State& state = Logger::getState();
   QMutexLocker locker(&state.mutex);
   state.logDirName = logDirName;
}

/**
  * The name of the folder, relative to the local data folder, where the log files are written.
  * Mirrors the default applied lazily by 'createFileLog()'.
  */
QString Logger::getLogDirName()
{
   State& state = Logger::getState();
   QMutexLocker locker(&state.mutex);
   return state.logDirName.isEmpty() ? DEFAULT_LOG_FOLDER_NAME : state.logDirName;
}

void Logger::addALoggerHook(QSharedPointer<LoggerHook> loggerHook)
{
   State& state = Logger::getState();
   QMutexLocker locker(&state.mutex);
   state.loggerHooks << loggerHook.toWeakRef();
}

Logger::Logger(const QString& name) :
   name(name)
{
}

Logger::~Logger()
{
}

bool Logger::log(const QString& message, Severity severity, const char* filename, int line) const
{
   State& state = Logger::getState();
   QMutexLocker locker(&state.mutex);

   QString threadName = QThread::currentThread()->objectName();
   threadName = threadName.isEmpty() ? QString::number((intptr_t)QThread::currentThreadId()) : threadName;

   QString filenameLine;
   if (filename && line)
      filenameLine = QString("%1:%2").arg(filename, QString::number(line));

   QSharedPointer<Entry> entry(new Entry(QDateTime::currentDateTime(), severity, this->name, threadName, filenameLine, message));

   // Say to all hooks there is a new message.
   for (const QSharedPointer<LoggerHook>& hook : state.loggerHooks.takeAliveHooks())
      hook->newMessage(entry);

   if (!Logger::createFileLog())
      return false;

   state.out << entry->toStrLine() << Qt::endl;

   return true;
}

bool Logger::log(const ILoggable& object, Severity severity, const char* filename, int line) const
{
   return this->log(object.toStringLog(), severity, filename, line);
}

namespace
{
   /**
     * To sort a 'QList<QFileInfo>' by its last modified date.
     * See 'Logger::deleteOldestLog(..)'.
     */
   bool fileInfoLessThan(const QFileInfo& f1, const QFileInfo& f2)
   {
      return f1.lastModified() < f2.lastModified();
   }
}

/**
  * It will create the file log and open it for writing if it doesn't already exist.
  * Must be called in a mutex.
  */
bool Logger::createFileLog()
{
   State& state = Logger::getState();

   if (!state.file.isOpen())
   {
      if (state.logDirName.isEmpty())
         state.logDirName = DEFAULT_LOG_FOLDER_NAME;

      QTextStream outErr(stderr);

      try
      {
         QDir appDir(Common::Global::getDataFolder(Common::Global::DataFolderType::LOCAL));

         if (!appDir.exists(state.logDirName) && !appDir.mkdir(state.logDirName))
         {
            outErr << "Error, cannot create log directory: " << appDir.absoluteFilePath(state.logDirName) << Qt::endl;
            return false;
         }
         else
         {
            QDir logDir(appDir.absoluteFilePath(state.logDirName));

            QString filename = QDateTime::currentDateTime().toString("yyyy_MM_dd-hh_mm_ss") + ".log";

            state.file.setFileName(logDir.absoluteFilePath(filename));
            if (!state.file.open(QIODevice::WriteOnly))
            {
               outErr << "Error, cannot create log file: " << logDir.absoluteFilePath(filename) << Qt::endl;
               return false;
            }
            else
            {
               Logger::deleteOldestLog(logDir);
               state.out.setDevice(&state.file);
               state.out.setEncoding(QStringConverter::Utf8);
            }
         }
      }
      catch (Common::Global::UnableToGetFolder& e)
      {
         outErr << "Error, cannot create the application data directory: " << e.errorMessage << Qt::endl;
         return false;
      }
   }

   return true;
}

void Logger::deleteOldestLog(const QDir& logDir)
{
   QList<QFileInfo> entries;
   foreach (QFileInfo entry, logDir.entryInfoList())
   {
      if (entry.fileName() == "." || entry.fileName() == ".." || !entry.fileName().endsWith(".log"))
         continue;
      if (entry.isFile())
         entries.append(entry);
   }
   std::sort(entries.begin(), entries.end(), fileInfoLessThan);

   while (entries.size() > NB_LOGFILE)
      QFile::remove(entries.takeFirst().absoluteFilePath());
}
