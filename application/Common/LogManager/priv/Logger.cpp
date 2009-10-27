#include <priv/Logger.h>
using namespace LM;

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
   quint32 threadId = (quint32)QThread::currentThreadId();
   (*this->out) << "[" << Entry::SeverityToStr(severity) << "] (" << threadId << ") " << name << ": " << message << endl;
}

void Logger::log(const ILoggable& object, Severity severity)
{
}

QMutex Logger::mutex;
