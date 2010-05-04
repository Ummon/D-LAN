#include <Builder.h>
using namespace LM;

#include <ILogger.h>
#include <priv/Logger.h>
#include <priv/Entry.h>

/**
  * Create a new logger, the name may correspond to a module name.
  * @exception LoggerAlreadyExistsException
  */
QSharedPointer<ILogger> Builder::newLogger(const QString& name)
{
   return QSharedPointer<ILogger>(new Logger(name));
}

/**
  * Read a log entry given as a string.
  */
QSharedPointer<IEntry> Builder::decode(const QString& line)
{
   return QSharedPointer<IEntry>(new Entry(line));
}
