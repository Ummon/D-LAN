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

   public:
      Entry(const QString& line);
      virtual ~Entry(){};

      QDateTime getDate() const;
      QString getDateStr() const;
      Severity getSeverity() const;
      QString getSeverityStr() const;
      QString getName() const;
      QString getThread() const;
      QString getSource() const;
      QString getMessage() const;

      static QString dateToStr(const QDateTime& date);
      static QString severityToStr(Severity severity);
      static Severity severityFromStr(const QString& severity);

   private:
      QRegExp lineRegExp;

      QDateTime date;
      Severity severity;
      QString name;
      QString thread;
      QString source;
      QString message;
   };
}
#endif
