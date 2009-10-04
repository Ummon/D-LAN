#ifndef TESTS_H
#define TESTS_H

#include <QObject>
#include <QSharedPointer>

#include <ILogManager.h>
#include <ILogger.h>
using namespace LogManager;

class Tests : public QObject
{
   Q_OBJECT
public:
   Tests();
    
private slots:
   void initTestCase();
   void createLoggers();
   void logSomeMessages();
   
private :
   QSharedPointer<ILogManager> logManager;
   QSharedPointer<ILogger> logger1;
   QSharedPointer<ILogger> logger2;
};

#endif
