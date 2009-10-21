#ifndef LOGMANAGER_IENTRY_H
#define LOGMANAGER_IENTRY_H

#include <QString>
#include <QDate>

namespace LM
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
      virtual ~IEntry() {}

      virtual QString getName() = 0;
      virtual QString getMessage() = 0;
      virtual QDate getDate() = 0;
      virtual Severity getSeverity() = 0;
   };
}
#endif
