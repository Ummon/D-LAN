#include <priv/FileManager.h>
using namespace FileManager;

#include <QSharedPointer>

#include <Common/LogManager/Builder.h>
#include <Common/LogManager/ILogger.h>

::FileManager::FileManager()
   : fileUpdater(this)
{
   FileManager::logger->log("Loading ..", LogManager::EndUser);
}

IChunk* ::FileManager::getChunk(const Common::Hash& hash)
{
   throw 1;
}

Chunks& ::FileManager::getChunks()
{
   return this->chunks;
}

void ::FileManager::addToWordIndex(Entry* entry)
{
   // TODO ;)
}

/*WordIndex<Entry*>& ::FileManager::getWordIndex()
{
   return this->wordIndex;
}*/

QSharedPointer<LogManager::ILogger> FileManager::FileManager::logger(LogManager::Builder::newLogger("FileManager"));
