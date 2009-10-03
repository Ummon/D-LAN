#ifndef LOGMANAGER_BUILDER_H
#define LOGMANAGER_BUILDER_H

#include <QTextStream>
#include <QSharedPointer>

#include <ILogManager.h>

namespace LogManager
{
   class Builder
   {
   public:
      /**
        * Output to stdout.
        */
      static QSharedPointer<ILogManager> createLogManager();
      static QSharedPointer<ILogManager> createLogManager(QTextStream& stream);
   };
}
#endif
