#include <priv/Logger.h>
using namespace LM;

#include <QtDebug>
#include <QThread>

#include <Constants.h>
#include <Common.h>

#include <priv/Entry.h>

Logger::Logger(const QString& name)
   : name(name)
{
   QMutexLocker lock(&Logger::mutex);

   // TODO : catch exception whether folder or file cannot be created.
   if (Logger::nbLogger == 0)
   {
      QTextStream out(stdout);
      static const QString logDirname("log");

      if (!Common::Global::createApplicationFolder())
      {
         out << "Error, cannot create application directory : " << Common::APPLICATION_FOLDER_PATH << endl;
      }
      else
      {
         QDir appDir(Common::APPLICATION_FOLDER_PATH);
         if (!appDir.exists(logDirname) && !appDir.mkdir(logDirname))
         {
            out << "Error, cannot create log directory : " << Common::APPLICATION_FOLDER_PATH << "/" << logDirname << endl;
         }
         else
         {
            QDir LogDir(Common::APPLICATION_FOLDER_PATH + '/' + logDirname);

            QString filename = QDateTime::currentDateTime().toString("yyyy_MM_dd-hh_mm_ss") + ".log";

            QFile* file = new QFile(LogDir.absoluteFilePath(filename));
            if (!file->open(QIODevice::WriteOnly))
            {
               out << "Error, cannot create log file : " << LogDir.absoluteFilePath(filename) << endl;
               delete file;
            }
            else
               Logger::out = new QTextStream(file);
         }
      }
   }

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

void Logger::log(const QString& message, Severity severity)
{
   QMutexLocker lock(&Logger::mutex);
   QString threadName = QThread::currentThread()->objectName();
   const QString& formatedMessage = QString("[%1] (%2) %3: %4").arg(
      Entry::SeverityToStr(severity),
      threadName.isEmpty() ? QString::number((quint32)QThread::currentThreadId()) : threadName,
      this->name,
      message
   );

   if (Logger::out)
      (*Logger::out) << formatedMessage << endl;

#ifdef DEBUG
   qDebug().nospace() << formatedMessage;
#endif
}

void Logger::log(const ILoggable& object, Severity severity)
{
}

QTextStream* Logger::out(0);
QMutex Logger::mutex;
int Logger::nbLogger(0);
