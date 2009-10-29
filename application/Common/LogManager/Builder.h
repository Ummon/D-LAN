#ifndef LOGMANAGER_BUILDER_H
#define LOGMANAGER_BUILDER_H

#include <QTextStream>
#include <QSharedPointer>

namespace LM
{
   class ILogger;

   class Builder
   {
   public:
      /**
        * Output to stdout.
        */
      static QSharedPointer<ILogger> newLogger(const QString& name);

      /**
        *
        * /!\ The given stream will never be deleted by the LogManager.
        * @param stream The stream where to output all log message.
        * @exception LoggerAlreadyExistsException
        */
      static QSharedPointer<ILogger> newLogger(QTextStream* stream, const QString& name);
   };
}
#endif
