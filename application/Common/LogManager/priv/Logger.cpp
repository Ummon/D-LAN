#include <priv/Logger.h>
using namespace LogManager;

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
   (*this->out) << name << ": " << message << endl;
}

void Logger::log(const ILoggable& object, Severity severity)
{
}
