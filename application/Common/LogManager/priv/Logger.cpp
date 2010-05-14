#include <priv/Logger.h>
using namespace LM;

#include <QtDebug>
#include <QThread>

#include <Constants.h>
#include <Global.h>

#include <priv/Entry.h>

QTextStream* Logger::out(0);
QMutex Logger::mutex;
int Logger::nbLogger(0);

Logger::Logger(const QString& name)
   : name(name)
{
   QMutexLocker lock(&Logger::mutex);
   Logger::nbLogger += 1;
}

Logger::~Logger()
{
   QMutexLocker lock(&Logger::mutex);
   Logger::nbLogger -= 1;

   if (Logger::nbLogger == 0 && this->out)
   {
      delete this->out->device(); // Is this necessary?
      delete this->out;
   }
}

void Logger::log(const QString& originalMessage, Severity severity, const char* filename, int line) const
{
   QMutexLocker lock(&Logger::mutex);

   Logger::createFileLog();

   QString threadName = QThread::currentThread()->objectName();

   QString message(originalMessage);
   message.replace('\n', "<lf>");

   bool logFilnameAndLineNumber = filename && line;

   QString formatedMessage =
      QString(logFilnameAndLineNumber ? "%1 [%2] {%3} (%4) <%5:%6> : %7" : "%1 [%2] {%3} (%4) : %5").arg
      (
         Entry::dateToStr(QDateTime::currentDateTime()),
         Entry::severityToStr(severity),
         this->name,
         threadName.isEmpty() ? QString::number((quint32)QThread::currentThreadId()) : threadName
      );

   if (logFilnameAndLineNumber)
      formatedMessage = formatedMessage.arg(filename, QString::number(line));

   formatedMessage = formatedMessage.arg(message);

   if (Logger::out)
      (*Logger::out) << formatedMessage << endl;
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
   if (!Logger::out)
   {
      QTextStream out(stderr);

      if (!Common::Global::createApplicationFolder())
      {
         out << "Error, cannot create application directory : " << Common::APPLICATION_FOLDER_PATH << endl;
      }
      else
      {
         QDir appDir(Common::APPLICATION_FOLDER_PATH);
         if (!appDir.exists(Common::LOG_FOLDER_NAME) && !appDir.mkdir(Common::LOG_FOLDER_NAME))
         {
            out << "Error, cannot create log directory : " << Common::APPLICATION_FOLDER_PATH << "/" << Common::LOG_FOLDER_NAME << endl;
         }
         else
         {
            QDir logDir(Common::APPLICATION_FOLDER_PATH + '/' + Common::LOG_FOLDER_NAME);

            QString filename = QDateTime::currentDateTime().toString("yyyy_MM_dd-hh_mm_ss") + ".log";

            QFile* file = new QFile(logDir.absoluteFilePath(filename));
            if (!file->open(QIODevice::WriteOnly))
            {
               out << "Error, cannot create log file : " << logDir.absoluteFilePath(filename) << endl;
               delete file;
            }
            else
            {
               Logger::deleteOldestLog(logDir);
               Logger::out = new QTextStream(file);
               Logger::out->setCodec("UTF-8");
            }
         }
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
