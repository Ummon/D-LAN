#ifndef TESTS_H
#define TESTS_H

#include <QObject>
#include <QVector>
#include <QSharedPointer>

#include <ILogger.h>
using namespace LM;

class Tests : public QObject
{
   Q_OBJECT
public:
   Tests();

private slots:
   void initTestCase();
   void createLoggers();
   void logSomeBasicMessages();
   void logSomeMessagesWithSpecialCharacters();
   void logALotOfRandomMessages();

private:
   QVector< QSharedPointer<ILogger> > loggers;
};

#endif
