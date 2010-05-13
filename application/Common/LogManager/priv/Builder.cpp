#include <Builder.h>
using namespace LM;

#include <ILogger.h>
#include <priv/Logger.h>
#include <priv/Entry.h>

#include <priv/QtLogger.h>

/**
  * Create a new logger, the name may correspond to a module name.
  * @exception LoggerAlreadyExistsException
  */
QSharedPointer<ILogger> Builder::newLogger(const QString& name)
{
   QtLogger::init();
   return QSharedPointer<ILogger>(new Logger(name));
}

/**
  * Read a log entry given as a string.
  * @exception MalformedEntryLog
  */
QSharedPointer<IEntry> Builder::decode(const QString& line)
{
   return QSharedPointer<IEntry>(new Entry(line));
}
