#include <Tests.h>

#include <QTest>
#include <QtGlobal>

#include <Builder.h>
#include <IEntry.h>
using namespace LM;

/**
  * @class Logger
  * A thread to logg some random messages.
  */

ThreadLogger::ThreadLogger(const QString& name, int delta) :
   logger(Builder::newLogger(name)), delta(delta)
{
}

void ThreadLogger::run()
{
   const QString mess("A random message from thread");

   qsrand(QTime::currentTime().msec());
   for (int i = 0; i < 300; i++)
   {
      int severity = qrand() % 6;
      switch (severity)
      {
      case 0 :
         LOG_WARN(this->logger, mess);
         break;
      case 1 :
         LOG_USER(this->logger, mess);
         break;
      case 2 :
         LOG_DEBU(this->logger, mess);
         break;
      case 3 :
         LOG_WARN(this->logger, mess);
         break;
      case 4 :
         LOG_ERRO(this->logger, mess);
         break;
      case 5 :
         LOG_FATA(this->logger, mess);
         break;
      }

      QTest::qSleep(this->delta);
   }
}

Tests::Tests()
{
}

void Tests::initTestCase()
{
}

/**
  * Create some classic loggers and thread loggers.
  */
void Tests::createLoggers()
{
   this->loggers << Builder::newLogger("Logger 1");
   this->loggers << Builder::newLogger("Logger 2");
   this->loggers << Builder::newLogger("Logger 3");

   for (int i = 0; i < 8; i++)
      this->threadLoggers << QSharedPointer<ThreadLogger>(new ThreadLogger(QString("Thread logger %1").arg(i), 1000 + i * 100));
}

void Tests::logSomeBasicMessages()
{
   for (int i = 0; i < this->loggers.count(); i++)
   {
      LOG_USER(this->loggers[i], QString("logger%1 user message").arg(i));
      LOG_DEBU(this->loggers[i], QString("logger%1 debug message").arg(i));
      LOG_WARN(this->loggers[i], QString("logger%1 warning message").arg(i));
      LOG_ERRO(this->loggers[i], QString("logger%1 error message").arg(i));
      LOG_FATA(this->loggers[i], QString("logger%1 fatal error message").arg(i));
   }
}

void Tests::logSomeMessagesWithSpecialCharacters()
{
   LOG_USER(this->loggers[0], "line return : \naaa");
   LOG_USER(this->loggers[0], "e-acute : é");
}

/**
  * Start all the thread loggers.
  */
void Tests::startTheThreadLoggers()
{
   connect(this, SIGNAL(destroyed()), this, SLOT(asdasd()));

   foreach (QSharedPointer<ThreadLogger> logger, this->threadLoggers)
   {
      logger->start();
      QTest::qSleep(100);
   }
   foreach (QSharedPointer<ThreadLogger> logger, this->threadLoggers)
   {
      logger->wait();
   }
}
