#include <priv/Cache/Cache.h>
using namespace FM;

#include <QDir>

#include <priv/Cache/SharedDirectory.h>
#include <priv/FileManager.h>

Cache::Cache(FileManager* fileManager, FileUpdater* fileUpdater)
   : fileManager(fileManager), fileUpdater(fileUpdater)
{
}

QStringList Cache::getSharedDirs(SharedDirectory::Rights rights)
{
   QStringList list;

   for (QListIterator<SharedDirectory*> i(this->sharedDirs); i.hasNext();)
   {
      SharedDirectory* dir = i.next();
      if (dir->getRights() == rights)
         list << dir->getPath();
   }
   return list;
}

void Cache::setSharedDirs(const QStringList& dirs, SharedDirectory::Rights rights)
{
   // Filter the actual shared directories by looking theirs rights.
   QList<SharedDirectory*> sharedDirs;
   for(QListIterator<SharedDirectory*> i(this->sharedDirs); i.hasNext();)
   {
      SharedDirectory* dir = i.next();
      if (dir->getRights() == rights)
         sharedDirs << dir;
   }

   QStringList newDirs;
   QMutableListIterator<SharedDirectory*> j(sharedDirs);

   // O(n^2).
   for(QListIterator<QString> i(dirs); i.hasNext();)
   {
      QString dir =  QDir::cleanPath(i.next());
      j.toFront();
      while(j.hasNext())
      {
         if (j.next()->getPath() == dir)
         {
            j.remove();
            goto next;
         }
      }
      newDirs << dir;
      next:;
   }

   // Remove shared directories.
   for (j.toFront(); j.hasNext();)
   {
      SharedDirectory* dir = j.next();
      LOG_DEBUG("Remove a shared directory : " + dir->getPath());
      this->fileUpdater->rmRoot(dir);
      this->sharedDirs.removeOne(dir);
   }

   // Create new shared directories.
   for (QListIterator<QString> i(newDirs); i.hasNext();)
   {
      QString path = i.next();
      SharedDirectory* dir = new SharedDirectory(this, path);
      LOG_DEBUG("Add a shared directory : " + dir->getPath());
      this->fileUpdater->addRoot(dir);
      this->sharedDirs << dir;
   }
}

void Cache::onEntryAdded(Entry* entry)
{
   emit entryAdded(entry);
}

void Cache::onEntryRemoved(Entry* entry)
{
   emit entryRemoved(entry);
}
