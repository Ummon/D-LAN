#include <Builder.h>
using namespace LogManager;

#include <ILogManager.h>
#include <priv/LogManager.h>

QSharedPointer<ILogManager> Builder::createLogManager()
{
   return QSharedPointer<ILogManager>(new LogManager());
}

QSharedPointer<ILogManager> Builder::createLogManager(QTextStream* stream)
{
   return QSharedPointer<ILogManager>(new LogManager(stream));   
}
