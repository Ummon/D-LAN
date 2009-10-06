#ifndef LOGMANAGER_ILOGGABLE_H
#define LOGMANAGER_ILOGGABLE_H

namespace LogManager
{
   class ILoggable
   {
   public:
      virtual ~ILoggable() {}

      virtual QString toStringLog() const = 0;
   };
}
#endif
