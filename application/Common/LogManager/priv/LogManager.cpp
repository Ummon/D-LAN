#include <priv/LogManager.h>
using namespace LogManager;

#include <ILogger.h>
#include <priv/Logger.h>

::LogManager::LogManager()
{
   this->out = new QTextStream(stdout);
}
      
::LogManager::LogManager(QTextStream* stream)
   : out(stream)
{
}

QSharedPointer<ILogger> LogManager::LogManager::newLogger(const QString& name)
{
   return QSharedPointer<ILogger>(new Logger(this->out, name));
}
