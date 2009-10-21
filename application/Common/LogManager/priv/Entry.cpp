#include <priv/Entry.h>
using namespace LM;

#include <QString>

QString Entry::SeverityToStr(Severity severity)
{
   switch (severity)
   {
   case FatalError:
      return "Fatal";
   case Error:
      return "Error";
   case Warning:
      return "Warning";
   case Debug:
      return "Debug";
   case EndUser:
      return "User";
   };
   return "Unknown";
}
