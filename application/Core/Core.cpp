#include <Core.h>
using namespace Core;

#include <Common/LogManager/Builder.h>

::Core::Core()
      : logger(LogManager::Builder::newLogger("Core"))
{
   this->logger->log("Core Up", LogManager::EndUser);
}
