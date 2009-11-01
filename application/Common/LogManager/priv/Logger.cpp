#include <priv/Logger.h>
using namespace LM;

#include <QtDebug>
#include <QThread>

#include <priv/Entry.h>

Logger::Logger(const QString& name)
      : name(name), out(new QTextStream(stdout))
{

}

Logger::Logger(QTextStream* stream, const QString& name)
      : name(name), out(stream)
{
}

void Logger::log(const QString& message, Severity severity)
{
   QMutexLocker lock(&Logger::mutex);
   QString threadName = QThread::currentThread()->objectName();
   /*(*this->out)*/
   qDebug().nospace() << QString("[%1] (%2) %3: %4").arg(
      Entry::SeverityToStr(severity),
      threadName.isEmpty() ? QString::number((quint32)QThread::currentThreadId()) : threadName,
      this->name,
      message);
}

void Logger::log(const ILoggable& object, Severity severity)
{
}

QMutex Logger::mutex;
