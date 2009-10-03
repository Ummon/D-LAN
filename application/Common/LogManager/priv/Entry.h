#ifndef LOGMANAGER_ENTRY_H
#define LOGMANAGER_ENTRY_H

#include <IEntry.h>

namespace LogManager
{
   class Entry : public IEntry
   {
   public:
      Entry();
      virtual ~Entry();
      
      QString getName();
      QString getMessage();
      QDate getDate();
      Severity getSeverity();
      
   private:
      QString name;
      QString message;
      QString date;
      QString severity;
   };
}
#endif
