#include <priv/FileManager.h>
using namespace FileManager;

#include <QSharedPointer>
#include <QStringList>
#include <QRegExp>

#include <Common/LogManager/Builder.h>
#include <Common/LogManager/ILogger.h>

#include <priv/Cache/Entry.h>

::FileManager::FileManager()
   : fileUpdater(this)
{
   FileManager::logger->log("Loading ..", LogManager::EndUser);
   this->fileUpdater.run();
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
   const static QRegExp regExp("(\\W+|_)");
   QStringList words = entry->getName().toLower().split(regExp, QString::SkipEmptyParts);

   // Remove all word smaller than MAX_WORD_LENGTH.
   int i = 0;
   while (i < words.length())
   {
      if (words[i].length() < MAX_WORD_LENGTH)
         words.removeAt(i);
      else
         i += 1;
   }

   this->wordIndex.addItem(words, entry);
}

/*WordIndex<Entry*>& ::FileManager::getWordIndex()
{
   return this->wordIndex;
}*/

QSharedPointer<LogManager::ILogger> FileManager::FileManager::logger(LogManager::Builder::newLogger("FileManager"));
