#ifndef LOGMANAGER_BUILDER_H
#define LOGMANAGER_BUILDER_H

#include <QDir>
#include <QSharedPointer>

namespace LM
{
   class ILogger;

   class Builder
   {
   public:
      /**
        * @exception LoggerAlreadyExistsException
        */
      static QSharedPointer<ILogger> newLogger(const QString& name);
   };
}
#endif
