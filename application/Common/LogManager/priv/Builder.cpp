#include <Builder.h>
using namespace LM;

#include <Constants.h>

#include <ILogger.h>
#include <priv/Logger.h>
#include <priv/Entry.h>
#include <priv/LoggerHook.h>

#include <priv/QtLogger.h>
#include <priv/StdLogger.h>

void Builder::setLogDirName(const QString& logDirName)
{
   Logger::setLogDirName(logDirName);
}

/**
  * Create a new logger, the name may correspond to a module name.
  * @exception LoggerAlreadyExistsException
  */
QSharedPointer<ILogger> Builder::newLogger(const QString& name)
{
   return QSharedPointer<ILogger>(new Logger(name));
}

/**
  * Return an hook to grap all log message for the given severities.
  */
QSharedPointer<ILoggerHook> Builder::newLoggerHook(Severity severities)
{
   QSharedPointer<LoggerHook> loggerHook(new LoggerHook(severities));
   Logger::addALoggerHook(loggerHook);
   return loggerHook;
}

/**
  * Read a log entry given as a string.
  * @exception MalformedEntryLog
  */
QSharedPointer<IEntry> Builder::decode(const QString& line)
{
   return QSharedPointer<IEntry>(new Entry(line));
}

QSharedPointer<IEntry> Builder::newEntry(const QDateTime& dateTime, Severity severity, const QString& message, const QString& name, const QString& thread, const QString& source)
{
   return QSharedPointer<IEntry>(new Entry(dateTime, severity, name, thread, source, message));
}

/**
  * If the Qt message handler is overridden you can redefine it
  * by calling this function.
  * For example the function 'QTest::qExec(..)' will override the message handler.
  */
void Builder::initMsgHandler()
{
   StdLogger::init();
   QtLogger::initMsgHandler();
}
