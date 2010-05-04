#include <priv/Entry.h>
using namespace LM;

#include <QString>
#include <QStringList>

const QString Entry::DATE_TIME_FORMAT("dd-MM-yyyy HH:mm:ss");
const QString Entry::SEVERITIES_STR[] = {"Fatal", "Error", "Warning", "Debug", "User", "Unkown"};

Entry::Entry(const QString& line) :
   lineRegExp("(\\S+ \\S+) \\[(\\w+)\\] \\{(\\S+)\\} \\((\\w+)\\) (?:<(\\S+:\\d+)> )?: (.*)"),
   severity(SV_UNKNOWN)
{
   if (!lineRegExp.exactMatch(line))
      return;

   QStringList capturedTexts = lineRegExp.capturedTexts();

   if (capturedTexts.count() < 6)
      return;

   this->date = QDateTime::fromString(capturedTexts[1], Entry::DATE_TIME_FORMAT);
   this->severity = Entry::severityFromStr(capturedTexts[2]);
   this->name = capturedTexts[3];
   this->thread = capturedTexts[4];

   if (capturedTexts.count() == 6)
      this->message = capturedTexts[5];
   else if (capturedTexts.count() == 7)
   {
      this->source = capturedTexts[5];
      this->message = capturedTexts[6];
   }
}

QDateTime Entry::getDate() const
{
   return this->date;
}

QString Entry::getDateStr() const
{
   return Entry::dateToStr(this->date);
}

Severity Entry::getSeverity() const
{
   return this->severity;
}

QString Entry::getSeverityStr() const
{
   return Entry::severityToStr(this->severity);
}

QString Entry::getName() const
{
   return this->name;
}

QString Entry::getThread() const
{
   return this->thread;
}

QString Entry::getSource() const
{
   return this->source;
}

QString Entry::getMessage() const
{
   return this->message;
}

QString Entry::dateToStr(const QDateTime& date)
{
   return date.toString("dd-MM-yyyy HH:mm:ss");
}

QString Entry::severityToStr(Severity severity)
{
   return Entry::SEVERITIES_STR[static_cast<int>(severity)];
}

Severity Entry::severityFromStr(const QString& severity)
{
   for (int i = 0; i < 6; i++)
      if (Entry::SEVERITIES_STR[i] == severity)
         return static_cast<Severity>(i);
   return SV_UNKNOWN;
}
