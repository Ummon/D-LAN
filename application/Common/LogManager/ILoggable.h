#ifndef LOGMANAGER_ILOGGABLE_H
#define LOGMANAGER_ILOGGABLE_H

#include <QString>

namespace LM
{
   class ILoggable
   {
   public:
      virtual ~ILoggable() {}

      virtual QString toStringLog() const = 0;
   };
}
#endif
