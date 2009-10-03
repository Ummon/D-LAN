#ifndef LOGMANAGER_IENTRY_H
#define LOGMANAGER_IENTRY_H

#include <QString>
#include <QDate>

namespace LogManager
{
   enum Severity
   {
      FatalError,
      Error,
      Warning,
      Debug,
      EndUser
   };

   class IEntry
   {
   public:
      virtual QString getName() = 0;
      virtual QString getMessage() = 0;
      virtual QDate getDate() = 0;
      virtual Severity getSeverity() = 0;
   };
}
#endif
