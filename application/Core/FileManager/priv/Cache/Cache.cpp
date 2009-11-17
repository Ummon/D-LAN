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
   : fileManager(fileManager), fileUpdater(fileUpdater), lock(QMutex::Recursive)
{
}

QStringList Cache::getSharedDirs(SharedDirectory::Rights rights)
{
   QStringList list;

   for (QListIterator<SharedDirectory*> i(this->sharedDirs); i.hasNext();)
   {
      SharedDirectory* dir = i.next();
      if (dir->getRights() == rights)
         list << dir->getFullPath();
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

   QMutableListIterator<SharedDirectory*> j(sharedDirs);

   // /!\ O(n^2).
   for(QListIterator<QString> i(dirs); i.hasNext();)
   {
      QString dir =  QDir::cleanPath(i.next());
      j.toFront();
      while(j.hasNext())
      {
         SharedDirectory* sharedDir = j.next();
         if (sharedDir->getFullPath() == dir)
         {
            j.remove();
            goto next;
         }
      }
      next:;
   }

   // Remove shared directories.
   for (j.toFront(); j.hasNext();)
   {
      SharedDirectory* dir = j.next();
      LOG_DEBUG("Remove a shared directory : " + dir->getFullPath());
      this->removeSharedDir(dir);
   }

   // The duplicate dirs are found further because we compare only
   // directories with the same rights here,
   this->createSharedDirs(dirs, rights);
}

void Cache::retrieveFromFile(const Protos::FileCache::Hashes& hashes)
{
   this->createSharedDirs(hashes);
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
      sharedDirMess->mutable_id()->set_hash(sharedDir->getId().getData(), Common::Hash::HASH_SIZE);

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

SharedDirectory* Cache::getSuperSharedDirectory(const QString& path)
{
   const QStringList& folders = path.split('/', QString::SkipEmptyParts);

   for (QListIterator<SharedDirectory*> i(this->sharedDirs); i.hasNext();)
   {
      SharedDirectory* sharedDir = i.next();
      const QStringList& foldersShared = sharedDir->getFullPath().split('/', QString::SkipEmptyParts);
      if (folders.size() <= foldersShared.size())
         continue;

      for (int i = 0; i < foldersShared.size(); i++)
         if (folders[i] != foldersShared[i])
            continue;

      return sharedDir;
   }

   return 0;
}

QList<SharedDirectory*> Cache::getSubSharedDirectories(const QString& path)
{
   QList<SharedDirectory*> ret;

   const QStringList& folders = path.split('/', QString::SkipEmptyParts);

   for (QListIterator<SharedDirectory*> i(this->sharedDirs); i.hasNext();)
   {
      SharedDirectory* sharedDir = i.next();
      const QStringList& foldersShared = sharedDir->getFullPath().split('/', QString::SkipEmptyParts);

      if (foldersShared.size() <= folders.size())
         continue;

      for (int i = 0; i < folders.size(); i++)
         if (folders[i] != foldersShared[i])
            goto nextSharedDir;

      ret << sharedDir;

      nextSharedDir:;
   }

   return ret;
}

bool Cache::isShared(const QString& path) const
{
   foreach (SharedDirectory* dir, this->sharedDirs)
      if (dir->getFullPath() == path)
         return true;
   return false;
}

void Cache::onEntryAdded(Entry* entry)
{
   emit entryAdded(entry);
}

void Cache::onEntryRemoved(Entry* entry)
{
   if (SharedDirectory* sharedDir = dynamic_cast<SharedDirectory*>(entry))
      this->sharedDirs.removeOne(sharedDir);

   emit entryRemoved(entry);
}

void Cache::onChunkHashKnown(Chunk* chunk)
{
   emit chunkHashKnown(chunk);
}

void Cache::onChunkRemoved(Chunk* chunk)
{
   emit chunkRemoved(chunk);
}

void Cache::removeSharedDir(SharedDirectory* dir, Directory* dir2)
{
   QMutexLocker locker(&this->lock);

   this->sharedDirs.removeOne(dir);

   this->fileUpdater->rmRoot(dir, dir2);

   // Delete all chunks.
   //dir->eliminate();

   //delete dir; // Directory is deleted by fileUpdater
}

void Cache::createSharedDirs(const QStringList& dirs, const QList<SharedDirectory::Rights>& rights, const QList<Common::Hash>& ids)
{
   QMutexLocker locker(&this->lock);

   QStringList dirsNotFound;

   QListIterator<QString> i(dirs);
   QListIterator<SharedDirectory::Rights> j(rights);
   QListIterator<Common::Hash> k(ids);
   while (i.hasNext())
   {
      QString path = i.next();

      SharedDirectory::Rights currentRights = j.hasNext() ? j.next() : SharedDirectory::READ_ONLY;

      try
      {
         SharedDirectory* dir = k.hasNext() ?
            new SharedDirectory(this, path, currentRights, k.next()) :
            new SharedDirectory(this, path, currentRights);

         LOG_DEBUG(QString("Add a new shared directory : %1").arg(path));
         this->fileUpdater->addRoot(dir);
         this->sharedDirs << dir;
      }
      catch (DirNotFoundException& e)
      {
         LOG_WARN(QString("Directory not found : %1").arg(e.path));
         dirsNotFound << e.path;
      }
      catch (DirAlreadySharedException&)
      {
         LOG_WARN(QString("Directory already shared : %1").arg(path));
      }
   }

   if (!dirsNotFound.isEmpty())
      throw DirsNotFoundException(dirsNotFound);
}

void Cache::createSharedDirs(const QStringList& dirs, SharedDirectory::Rights rights)
{
   this->createSharedDirs(dirs, QList<SharedDirectory::Rights>() << rights);
}

void Cache::createSharedDirs(const Protos::FileCache::Hashes& hashes)
{
   QStringList paths;
   QList<SharedDirectory::Rights> rights;
   QList<Common::Hash> ids;

   // Add the shared directories from the file cache.
   for (int i = 0; i < hashes.dir_size(); i++)
   {
      const Protos::FileCache::Hashes_SharedDir& dir = hashes.dir(i);
      paths << dir.path().data();
      rights << (dir.type() == Protos::FileCache::Hashes_SharedDir_Type_READ ? SharedDirectory::READ_ONLY : SharedDirectory::READ_WRITE)  ;
      ids << Common::Hash(dir.id().hash().data());
   }
   this->createSharedDirs(paths, rights, ids);
}
