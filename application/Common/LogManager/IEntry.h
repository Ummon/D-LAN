#ifndef LOGMANAGER_IENTRY_H
#define LOGMANAGER_IENTRY_H

#include <QString>
#include <QDateTime>

namespace LM
{
   enum Severity
   {
      SV_FATAL_ERROR = 0x01,
      SV_ERROR = 0x02,
      SV_WARNING = 0x04,
      SV_DEBUG = 0x08,
      SV_END_USER = 0x10,
      SV_UNKNOWN = 0x20
   };

   class IEntry
   {
   public:
      virtual ~IEntry() {}

      virtual QDateTime getDate() const = 0;
      virtual QString getDateStr(bool withMs = true) const = 0;
      virtual Severity getSeverity() const = 0;
      virtual QString getSeverityStr() const = 0;
      virtual QString getName() const = 0;
      virtual QString getThread() const = 0;
      virtual QString getSource() const = 0;
      virtual QString getMessage() const = 0;
   };
}
#endif
