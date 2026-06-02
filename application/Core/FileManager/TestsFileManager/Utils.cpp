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
   for (const auto& sharedEntry : entries.entries())
   {
      auto sharedPath = Common::Path(QString::fromStdString(sharedEntry.shared_entry().path()));
      if (sharedPath.getLastElement() == dirs[0])
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
      for (const auto& subEntry : entries.entries())
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
      QTest::qWait(waitBetweenTries_ms);
      i--;
   }
   return false;
}

bool Utils::tryOpen(QFile& file, QIODeviceBase::OpenModeFlag flags)
{
   for (int i = 0; i < 20; i++)
   {
      if (file.open(flags))
         return true;
      // qDebug() << "Can't open the file " << file.fileName() << ", retrying...";
      QTest::qWait(200);
   }
   return false;
}
