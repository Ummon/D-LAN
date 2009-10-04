#include <priv/Logger.h>
using namespace LogManager;

Logger::Logger(QTextStream* stream, const QString& name)
   : out(stream)
{
}

void Logger::log(const QString& message, Severity severity)
{
   (*this->out) << message << endl;
}

void Logger::log(const ILoggable& object, Severity severity)
{
}
