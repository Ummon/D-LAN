#include <priv/Cache/Cache.h>
using namespace FM;

#include <QDir>

#include <Exceptions.h>
#include <priv/Log.h>
#include <priv/FileManager.h>
#include <priv/Exceptions.h>
#include <priv/Constants.h>
#include <priv/Cache/SharedDirectory.h>

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

   // /!\ O(n^2).
   for(QListIterator<QString> i(dirs); i.hasNext();)
   {
      QString dir =  QDir::cleanPath(i.next());
      j.toFront();
      while(j.hasNext())
      {
         if (j.next()->getFullPath() == dir)
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
      LOG_DEBUG("Remove a shared directory : " + dir->getFullPath());
      this->removeSharedDir(dir);
   }

   QStringList dirsNotFound;

   // Create new shared directories.
   for (QListIterator<QString> i(newDirs); i.hasNext();)
   {
      QString path = i.next();
      SharedDirectory* dir = new SharedDirectory(this, path, rights);
      LOG_DEBUG("Add a shared directory : " + dir->getFullPath());
      try
      {
         this->fileUpdater->addRoot(dir);
         this->sharedDirs << dir;
      }
      catch (DirNotFoundException& e)
      {
         delete dir;
         dirsNotFound << e.getPath();
      }
   }

   if (!dirsNotFound.isEmpty())
      throw DirsNotFoundException(dirsNotFound);
}

QList<SharedDirectory*> Cache::retrieveFromFile(const Protos::FileCache::Hashes& hashes)
{
   // Add the shared directories from the file cache.
   for (int i = 0; i < hashes.dir_size(); i++)
   {
      const Protos::FileCache::Hashes_SharedDir& dir = hashes.dir(i);
      new SharedDirectory(
         this,
         dir.path().data(),
         dir.type() == Protos::FileCache::Hashes_SharedDir_Type_READ ? SharedDirectory::READ_ONLY : SharedDirectory::READ_WRITE,
         Common::Hash(dir.id().hash().data())
      );
   }
   return this->sharedDirs;
}

void Cache::saveInFile(Protos::FileCache::Hashes& hashes)
{
   hashes.set_version(1);
   hashes.set_chunksize(CHUNK_SIZE);
   for (QListIterator<SharedDirectory*> i(this->sharedDirs); i.hasNext();)
   {
      SharedDirectory* sharedDir = i.next();
      Protos::FileCache::Hashes_SharedDir* sharedDirMess = hashes.add_dir();

      sharedDirMess->set_path(sharedDir->getFullPath().toStdString());
      sharedDirMess->set_type(
         sharedDir->getRights() == SharedDirectory::READ_ONLY ?
            Protos::FileCache::Hashes_SharedDir_Type_READ :
            Protos::FileCache::Hashes_SharedDir_Type_READ_WRITE
      );
      sharedDirMess->mutable_id()->set_hash(sharedDir->getId().data());

      sharedDir->populateHashesDir(*sharedDirMess->mutable_root());
   }
}

quint64 Cache::getAmount()
{
   quint64 amount = 0;
   for(QListIterator<SharedDirectory*> i(this->sharedDirs); i.hasNext();)
      amount += i.next()->getSize();
   return amount;
}

void Cache::onEntryAdded(Entry* entry)
{
   emit entryAdded(entry);
}

void Cache::onEntryRemoved(Entry* entry)
{
   emit entryRemoved(entry);
}

void Cache::onChunkAdded(Chunk* chunk)
{
   emit chunkAdded(chunk);
}

void Cache::removeSharedDir(SharedDirectory* dir)
{
   dir->setDeleted();
   this->sharedDirs.removeOne(dir);

   // Mark each chunk as deleted and delete it?
   //... TODO

   this->fileUpdater->rmRoot(dir);
}
