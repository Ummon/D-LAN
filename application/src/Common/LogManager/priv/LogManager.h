#ifndef LOGMANAGER_LOGMANAGER_H
#define LOGMANAGER_LOGMANAGER_H

#include <ILogManager.h>

namespace LogManager
{
   class LogManager : public ILogManager
   {
   private:
      /**
        * @exception LoggerAlreadyExistsException
        */
      ILogger* newLogger(const QString& name);
   };
}
#endif
