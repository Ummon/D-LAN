#ifndef LOGMANAGER_IENTRY_H
#define LOGMANAGER_IENTRY_H

#include <QString>
#include <QDateTime>

namespace LM
{
   enum Severity
   {
      SV_FATAL_ERROR = 0,
      SV_ERROR,
      SV_WARNING,
      SV_DEBUG,
      SV_END_USER,
      SV_UNKNOWN
   };

   class IEntry
   {
   public:
      virtual ~IEntry() {}

      virtual QDateTime getDate() const = 0;
      virtual QString getDateStr() const = 0;
      virtual Severity getSeverity() const = 0;
      virtual QString getSeverityStr() const = 0;
      virtual QString getName() const = 0;
      virtual QString getThread() const = 0;
      virtual QString getSource() const = 0;
      virtual QString getMessage() const = 0;
   };
}
#endif
