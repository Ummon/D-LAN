#ifndef LOGMANAGER_LOGMANAGER_H
#define LOGMANAGER_LOGMANAGER_H

#include <ILogManager.h>

#include <QSharedPointer>

namespace LogManager
{
   class LogManager : public ILogManager
   {
   private:
      /**
        * @exception LoggerAlreadyExistsException
        */
      QSharedPointer<ILogger> newLogger(const QString& name);
   };
}
#endif
