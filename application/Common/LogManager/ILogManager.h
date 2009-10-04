#ifndef LOGMANAGER_ILOGMANAGER_H
#define LOGMANAGER_ILOGMANAGER_H

#include <QObject>
#include <QSharedPointer>

#include <LogManagerGlobal.h>
#include <Exceptions.h>
#include <IEntry.h>

namespace LogManager
{
   class ILogger;
   
   /**
     * 
     */
   class LOGMANAGER_EXPORT ILogManager : public QObject
   {
      Q_OBJECT      
   public:      
      /**
        * @exception LoggerAlreadyExistsException
        */
      LOGMANAGER_EXPORT virtual QSharedPointer<ILogger> newLogger(const QString& name) = 0;
      
   signals:
      void newEntry(const IEntry& entry);
   };
}

#endif
