#include <priv/FileManager.h>
using namespace FileManager;

#include <Common/LogManager/Builder.h>

::FileManager::FileManager()
      : logger(LogManager::Builder::newLogger("FileManager"))
{
   this->logger->log("Loading ..", LogManager::EndUser);
}

IChunk* ::FileManager::getChunk(const Common::Hash& hash)
{
   throw 1;
}
