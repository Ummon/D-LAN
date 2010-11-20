#include <priv/LoggerHook.h>
using namespace LM;

LoggerHook::LoggerHook(Severity severities)
   : severities(severities)
{
}

void LoggerHook::newMessage(QSharedPointer<const IEntry> entry)
{
   if (entry->getSeverity() & this->severities)
      emit newLogEntry(entry);
}
