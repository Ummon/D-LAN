#include <priv/Entry.h>
using namespace LogManager;

#include <QString>

QString Entry::SeverityToStr(Severity severity)
{
   switch (severity)
   {
   case LogManager::FatalError:
      return "Fatal";
   case LogManager::Error:
      return "Error";
   case LogManager::Warning:
      return "Warning";
   case LogManager::Debug:
      return "Debug";
   case LogManager::EndUser:
      return "User";
   };
   return "Unknown";
}
