#ifndef LOGMANAGER_ENTRY_H
#define LOGMANAGER_ENTRY_H

#include <IEntry.h>
#include <QString>
#include <QDateTime>
#include <QRegExp>

namespace LM
{
   class Entry : public IEntry
   {
      static const QString DATE_TIME_FORMAT;
      static const QString SEVERITIES_STR[];
      static QRegExp lineRegExp;

   public:
      Entry(const QString& line);
      Entry(const QDateTime& dateTime, Severity severity, const QString& name, const QString& thread, const QString& source, const QString& message);
      virtual ~Entry(){};

      QString toStrLine() const;

      QDateTime getDate() const;
      QString getDateStr(bool withMs = true) const;
      Severity getSeverity() const;
      QString getSeverityStr() const;
      QString getName() const;
      QString getThread() const;
      QString getSource() const;
      QString getMessage() const;

   private:
      static QString severityToStr(Severity severity);
      static Severity severityFromStr(const QString& severity);

      QDateTime date;
      Severity severity;
      QString name;
      QString thread;
      QString source;
      QString message;
   };
}
#endif
