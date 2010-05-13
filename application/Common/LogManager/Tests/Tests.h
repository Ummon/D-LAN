#ifndef TESTS_H
#define TESTS_H

#include <QObject>
#include <QVector>
#include <QSharedPointer>
#include <QThread>

#include <ILogger.h>
using namespace LM;

class ThreadLogger : public QThread
{
public:
   ThreadLogger(const QString& name, int delta);
   void run();

private:
   QSharedPointer<ILogger> logger;
   int delta;
};

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
   void startTheThreadLoggers();

private:
   QVector< QSharedPointer<ILogger> > loggers;
   QVector< QSharedPointer<ThreadLogger> > threadLoggers;
};

#endif
