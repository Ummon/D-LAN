#include <Builder.h>
using namespace LM;

#include <ILogger.h>
#include <priv/Logger.h>

QSharedPointer<ILogger> Builder::newLogger(const QString& name)
{
   return QSharedPointer<ILogger>(new Logger(name));
}

QSharedPointer<ILogger> Builder::newLogger(QTextStream* stream, const QString& name)
{
   return QSharedPointer<ILogger>(new Logger(stream, name));
}
