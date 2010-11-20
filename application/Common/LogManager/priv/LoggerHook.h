#ifndef LOG_MANAGER_LOGGERHOOK_H
#define LOG_MANAGER_LOGGERHOOK_H

#include <ILoggerHook.h>

namespace LM
{
   class LoggerHook : public ILoggerHook
   {
   public:
      LoggerHook(Severity severities);
      void newMessage(QSharedPointer<const IEntry> entry);

   private:
      Severity severities; // Only these severities will be caught.
   };
}

#endif
