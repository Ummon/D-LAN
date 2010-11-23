#ifndef LOG_MANAGER_ILOGGERHOOK_H
#define LOG_MANAGER_ILOGGERHOOK_H

#include <QObject>
#include <QSharedPointer>

#include "IEntry.h"

namespace LM
{
   class ILoggerHook : public QObject
   {
      Q_OBJECT
   public:
      virtual ~ILoggerHook() {}

   signals:
      void newLogEntry(QSharedPointer<const LM::IEntry> entry);
   };
}

#endif
