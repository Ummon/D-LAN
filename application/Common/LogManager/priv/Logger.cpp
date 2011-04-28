/**
  * D-LAN - A decentralized LAN file sharing software.
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
  
#include <priv/Logger.h>
using namespace LM;

#include <QtDebug>
#include <QThread>
#include <QSharedPointer>

#include <Common/Constants.h>
#include <Common/Global.h>

#include <Constants.h>

#include <priv/Entry.h>

int LoggerHooks::size() const
{
   const_cast<LoggerHooks*>(this)->removeDeletedHooks();
   return this->loggerHooks.size();
}

QWeakPointer<LoggerHook> LoggerHooks::operator<< (const QWeakPointer<LoggerHook> hook)
{
   this->loggerHooks << hook;
   return hook;
}

QWeakPointer<LoggerHook> LoggerHooks::operator[] (int i)
{
   this->removeDeletedHooks();
   return this->loggerHooks[i];
}

void LoggerHooks::removeDeletedHooks()
{
   for (QMutableListIterator< QWeakPointer<LoggerHook> > i(this->loggerHooks); i.hasNext();)
      if (!i.next().data())
         i.remove();
}

/////

QTextStream Logger::out;
QFile Logger::file;
QMutex Logger::mutex(QMutex::Recursive);
QString Logger::logDirName;

LoggerHooks Logger::loggerHooks;

void Logger::setLogDirName(const QString& logDirName)
{
   Logger::logDirName = logDirName;
}

void Logger::addALoggerHook(QSharedPointer<LoggerHook> loggerHook)
{
   QMutexLocker locker(&Logger::mutex);
   Logger::loggerHooks << loggerHook.toWeakRef();
}

/**
  * We can't use 'Logger::mutex' in constructor and destructor because we don't know if the object already exist (contructor) or
  * if it has been already deleted (destructor).
  */
Logger::Logger(const QString& name) :
   name(name)
{
}

Logger::~Logger()
{
}

void Logger::log(const QString& message, Severity severity, const char* filename, int line) const
{
   QMutexLocker locker(&Logger::mutex);

   QString threadName = QThread::currentThread()->objectName();
   threadName = threadName.isEmpty() ? QString::number((quint32)QThread::currentThreadId()) : threadName;

   QString filenameLine;
   if (filename && line)
      filenameLine = QString("%1:%2").arg(filename, QString::number(line));

   QSharedPointer<Entry> entry(new Entry(QDateTime::currentDateTime(), severity, this->name, threadName, filenameLine, message));

   // Say to all hooks there is a new message.
   for (int i = 0; i < this->loggerHooks.size(); i++)
      this->loggerHooks[i].data()->newMessage(entry);

   Logger::createFileLog();
   Logger::out << entry->toStrLine() << endl;
}

void Logger::log(const ILoggable& object, Severity severity, const char* filename, int line) const
{
    this->log(object.toStringLog(), severity, filename, line);
}

/**
  * To sort a 'QList<QFileInfo>' by its last modified date.
  * See 'Logger::deleteOldestLog(..)'.
  */
bool fileInfoLessThan(const QFileInfo& f1, const QFileInfo& f2)
{
   return f1.lastModified() < f2.lastModified();
}

/**
  * It will create the file log and open it for writing if it doesn't already exist.
  * Must be called in a mutex.
  */
void Logger::createFileLog()
{
   if (!Logger::file.isOpen())
   {
      if (logDirName.isEmpty())
         logDirName = DEFAULT_LOG_FOLDER_NAME;

      QTextStream outErr(stderr);

      try
      {
         QDir appDir(Common::Global::getDataFolder(Common::Global::LOCAL));

         if (!appDir.exists(logDirName) && !appDir.mkdir(logDirName))
         {
            outErr << "Error, cannot create log directory : " << appDir.absoluteFilePath(logDirName) << endl;
         }
         else
         {
            QDir logDir(appDir.absoluteFilePath(logDirName));

            QString filename = QDateTime::currentDateTime().toString("yyyy_MM_dd-hh_mm_ss") + ".log";

            Logger::file.setFileName(logDir.absoluteFilePath(filename));
            if (!Logger::file.open(QIODevice::WriteOnly))
            {
               outErr << "Error, cannot create log file : " << logDir.absoluteFilePath(filename) << endl;
            }
            else
            {
               Logger::deleteOldestLog(logDir);
               Logger::out.setDevice(&Logger::file);
               Logger::out.setCodec("UTF-8");
            }
         }
      }
      catch(Common::Global::UnableToGetFolder& e)
      {
         outErr << "Error, cannot create the application data directory: " << e.getMessage() << endl;
      }
   }
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
   qSort(entries.begin(), entries.end(), fileInfoLessThan);

   while (entries.size() > NB_LOGFILE)
      QFile::remove(entries.takeFirst().absoluteFilePath());
}
