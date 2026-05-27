#include <Utils.h>

#include <QTest>

/**
  * Try find an entry with the given relative path.
  * The first directory in the path is the shared directory or file.
  */
Protos::Common::Entry Utils::tryFindEntry(
   QSharedPointer<IFileManager> fileManager,
   Common::Path entryPath
)
{
   auto dirs = entryPath.getDirs();
   if (entryPath.isFile())
      dirs << entryPath.getFilename();

   Protos::Common::Entry entry;
   auto entries = fileManager->getEntries();
   for (const auto& sharedEntry : entries.entry())
   {
      if (sharedEntry.name() == dirs[0])
      {
         entry = sharedEntry;
         break;
      }
   }

   Protos::Common::SharedEntry sharedEntry = entry.shared_entry();

   for (int i = 1; i < dirs.size(); i++)
   {
      bool entryFound = false;
      auto entries = fileManager->getEntries(entry);
      for (const auto& subEntry : entries.entry())
      {
         if (subEntry.name() == dirs[i])
         {
            entry = subEntry;
            entry.mutable_shared_entry()->CopyFrom(sharedEntry);
            entryFound = true;
            break;
         }
      }

      if (!entryFound)
         return Protos::Common::Entry();
   }

   return entry;
}

/**
  * Try to execute multiple times the given function, wait some time between each executions.
  * Returns true if it succeeds.
  */
bool Utils::retry(int nbTries, int waitBetweenTries_ms, std::function<bool()> fun)
{
   int i = nbTries;
   while (i > 0) {
      if (fun())
         return true;
      QTest::qSleep(200);
      i--;
   }
   return false;
}
