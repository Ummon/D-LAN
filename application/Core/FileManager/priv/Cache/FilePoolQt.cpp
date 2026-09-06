#include <priv/Cache/FilePool.h>
using namespace FM;

QFile* FilePool::openFile(const QString& path, QIODevice::OpenMode mode, bool* fileCreated)
{
   auto file = new QFile(path);
   if (mode.testFlag(QIODevice::WriteOnly) && !mode.testFlag(QIODevice::ExistingOnly))
   {
      // Exclusive creation reports whether we actually created the file, without an exists/open race.
      if (file->open(mode | QIODevice::NewOnly))
      {
         if (fileCreated)
            *fileCreated = true;
         return file;
      }
      // ReadWrite preserves existing download data. ExistingOnly prevents a concurrent removal
      // from turning this fallback into an unreported creation.
      if (mode.testFlag(QIODevice::NewOnly) || !file->open(mode | QIODevice::ExistingOnly))
      {
         delete file;
         return nullptr;
      }
   }
   else if (!file->open(mode))
   {
      delete file;
      return nullptr;
   }
   return file;
}
