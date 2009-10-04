#ifndef LOGMANAGER_BUILDER_H
#define LOGMANAGER_BUILDER_H

#include <QTextStream>
#include <QSharedPointer>

#include <LogManagerGlobal.h>
#include <ILogManager.h>

namespace LogManager
{
   class LOGMANAGER_EXPORT Builder
   {
   public:
      /**
        * Output to stdout.
        */
      static QSharedPointer<ILogManager> createLogManager();
      
      /**
        *
        * /!\ The given stream will never be deleted by the LogManager.
        * @param stream The stream where to output all log message.
        */
      static QSharedPointer<ILogManager> createLogManager(QTextStream* stream);
   };
}
#endif
