#ifndef LOGMANAGER_BUILDER_H
#define LOGMANAGER_BUILDER_H

#include <QTextStream>
#include <ILogManager.h>

namespace LogManager
{
   class Builder
   {
   public:
      /**
        * Output to stdout.
        */
      static ILogManager* createLogManager();
      static ILogManager* createLogManager(QTextStream& stream);
   };
}
#endif
