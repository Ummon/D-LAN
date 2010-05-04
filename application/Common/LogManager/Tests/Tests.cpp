#include <Tests.h>

#include <QTest>

#include <Builder.h>
#include <IEntry.h>
using namespace LM;

Tests::Tests()
{
}

void Tests::initTestCase()
{
}

void Tests::createLoggers()
{
   this->loggers << Builder::newLogger("Logger1");
   this->loggers << Builder::newLogger("Logger2");
   this->loggers << Builder::newLogger("Logger3");
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

void Tests::logALotOfRandomMessages()
{
   for (int i = 0; i < 100; i++)
   {
      LOG_USER(this->loggers[0], "A random message");
      QTest::qSleep(800);
   }
}
