#include <Tests.h>

#include <Builder.h>
#include <IEntry.h>
using namespace LogManager;

Tests::Tests()
{
}

void Tests::initTestCase()
{
   this->logManager = Builder::createLogManager();
}

void Tests::createLoggers()
{
   this->logger1 = this->logManager->newLogger("Logger1");
   this->logger2 = this->logManager->newLogger("Logger2");
}

void Tests::logSomeMessages()
{
   this->logger1->log("This is a message log", EndUser);
}

