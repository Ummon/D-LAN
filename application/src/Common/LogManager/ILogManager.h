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
      /**
        * @exception LoggerAlreadyExistsException
        */
      virtual ILogger* newLogger(const QString& name) = 0;
      
   signals:
      void newEntry(const IEntry& entry);
   };
}

#endif
