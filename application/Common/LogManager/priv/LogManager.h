#ifndef LOGMANAGER_LOGMANAGER_H
#define LOGMANAGER_LOGMANAGER_H

#include <ILogManager.h>

#include <QSharedPointer>
#include <QTextStream>

namespace LogManager
{
   class LogManager : public ILogManager
   {
   public:
      LogManager();
      LogManager(QTextStream* stream);
      
      /**
        * @exception LoggerAlreadyExistsException
        */
      QSharedPointer<ILogger> newLogger(const QString& name);
      
   private:
      QTextStream* out;
   };
}
#endif
