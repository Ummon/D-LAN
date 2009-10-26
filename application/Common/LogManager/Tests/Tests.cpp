#include <Tests.h>

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
   this->logger1 = Builder::newLogger("Logger1");
   this->logger2 = Builder::newLogger("Logger2");
}

void Tests::logSomeMessages()
{
   this->logger1->log("This is a message log", EndUser);
}

