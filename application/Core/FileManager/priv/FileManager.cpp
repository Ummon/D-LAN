#include <priv/FileManager.h>
using namespace FileManager;

#include <QSharedPointer>

#include <Common/LogManager/Builder.h>
#include <Common/LogManager/ILogger.h>

::FileManager::FileManager()
      //: logger(LogManager::Builder::newLogger("FileManager"))
{
   FileManager::logger->log("Loading ..", LogManager::EndUser);
}

IChunk* ::FileManager::getChunk(const Common::Hash& hash)
{
   throw 1;
}

QSharedPointer<LogManager::ILogger> FileManager::FileManager::logger(LogManager::Builder::newLogger("FileManager"));
