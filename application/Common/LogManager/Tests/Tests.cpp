#include <Tests.h>

#include <Builder.h>
using namespace LogManager;

Tests::Tests()
{
}

void Tests::CreateLogManager()
{
   this->logManager = Builder::createLogManager();
}
