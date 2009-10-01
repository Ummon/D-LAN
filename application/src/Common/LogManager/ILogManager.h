#ifndef LOGMANAGER_ILOGMANAGER_H
#define LOGMANAGER_ILOGMANAGER_H

#include <QObject>

#include <Exceptions.h>
#include <IEntry.h>

namespace LogManager
{
   class ILogger;
   
   /**
     * 
     */
   class ILogManager : public QObject
   {
      Q_OBJECT      
   public:
      virtual ILogger* newLogger(const QString& name) throw(LoggerAlreadyExistsException) = 0;
      
   signals:
      void newEntry(const IEntry& entry);
   };
}

#endif
