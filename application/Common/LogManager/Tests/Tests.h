#ifndef TESTS_H
#define TESTS_H

#include <QObject>
#include <QSharedPointer>

#include <ILogManager.h>
using namespace LogManager;

class Tests : public QObject
{
   Q_OBJECT
public:
   Tests();
    
private slots:
   void CreateLogManager();
   //void CreateLoggers();
   
private :
   QSharedPointer<ILogManager> logManager;
};

#endif
