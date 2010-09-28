#include <priv/Cache/Cache.h>
using namespace FM;

#include <QDir>

#include <Common/Global.h>
#include <Exceptions.h>
#include <priv/Log.h>
#include <priv/FileManager.h>
#include <priv/Exceptions.h>
#include <priv/Constants.h>
#include <priv/Cache/SharedDirectory.h>
#include <priv/Cache/File.h>

Cache::Cache(FileManager* fileManager)
   : fileManager(fileManager), lock(QMutex::Recursive)
{
}

/**
  * a) Search among their shared directory the one who match the given entry.
  * b) In the shared directory try to find the directory corresponding to 'entry.dir.path'.
  * c) Populate the result with directories and files.
  */
Protos::Core::GetEntriesResult Cache::getEntries(const Protos::Common::Entry& dir) const
{
   Protos::Core::GetEntriesResult result;

   // If we can't find the shared directory..
   if (!dir.has_shared_dir())
      return result;

   QMutexLocker locker(&this->lock);

   foreach (SharedDirectory* sharedDir, this->sharedDirs)
   {
      if (sharedDir->getId() == dir.shared_dir().id().hash().data())
      {
         QStringList folders = QDir::cleanPath(QString(dir.path().data())).split('/', QString::SkipEmptyParts);

         if (!dir.path().empty()) // An empty path means the dir is the root (a SharedDirectory).
            folders << dir.name().data();

         Directory* currentDir = sharedDir;
         foreach (QString folder, folders)
         {
            currentDir = currentDir->getSubDir(folder);
            if (!currentDir)
               break;
         }

         foreach (Directory* dir, currentDir->getSubDirs())
            dir->populateEntry(result.add_entry());

         foreach (File* file, currentDir->getFiles())
            file->populateEntry(result.add_entry());

         break;
      }
   }

   return result;
}

Protos::Core::GetEntriesResult Cache::getEntries() const
{
   QMutexLocker locker(&this->lock);

   Protos::Core::GetEntriesResult result;

   foreach (SharedDirectory* sharedDir, this->sharedDirs)
      sharedDir->populateEntry(result.add_entry());

   return result;
}

/**
  * @path the absolute path to a directory or a file.
  * @return Returns 0 if no entry found.
  */
Entry* Cache::getEntry(const QString& path) const
{
   QMutexLocker locker(&this->lock);

   foreach (SharedDirectory* sharedDir, this->sharedDirs)
   {
      if (path.startsWith(sharedDir->getFullPath()))
      {
         QString relativePath(path);
         relativePath.remove(0, sharedDir->getFullPath().size());
         const QStringList folders = relativePath.split('/', QString::SkipEmptyParts);

         Directory* currentDir = sharedDir;
         for (QStringListIterator i(folders); i.hasNext();)
         {
            QString folder = i.next();
            Directory* dir = currentDir->getSubDir(folder);
            if (!dir)
            {
               if (!i.hasNext())
               {
                  File* file = currentDir->getFile(folder);
                  if (file)
                     return file;
               }
               return 0;
            }
            currentDir = dir;
         }

         return currentDir;
      }
   }

   return 0;
}

/**
  * Try to find the file into the cache from a reference.
  */
File* Cache::getFile(const Protos::Common::Entry& fileEntry) const
{
   QMutexLocker locker(&this->lock);

   if (!fileEntry.has_shared_dir())
   {
      L_WARN(QString("Cache::getFile : 'fileEntry' doesn't have the field 'shared_dir' set!"));
      return 0;
   }

   foreach (SharedDirectory* sharedDir, this->sharedDirs)
   {
      if (sharedDir->getId() == fileEntry.shared_dir().id().hash().data())
      {
         QString relativePath(fileEntry.path().data());
         const QStringList folders = relativePath.split('/', QString::SkipEmptyParts);

         Directory* dir = sharedDir;
         QStringListIterator i(folders);
         forever
         {
            if (dir)
            {
               if (!i.hasNext())
               {
                  File* file = dir->getFile(fileEntry.name().data());

                  if (file)
                     return file;
                  return 0;
               }
            }
            else
               return 0;

            if (!i.hasNext())
               return 0;

            dir = dir->getSubDir(i.next());
         }

         return 0;
      }
   }

   return 0;
}

QList< QSharedPointer<IChunk> > Cache::newFile(const Protos::Common::Entry& remoteEntry)
{
   QMutexLocker locker(&this->lock);

   Directory* dir = this->getWriteableDirectory(QDir::cleanPath(remoteEntry.path().data()), remoteEntry.size() + MINIMUM_FREE_SPACE);
   if (!dir)
      throw UnableToCreateNewFileException();

   Common::Hashes hashes;
   for (int i = 0; i < remoteEntry.chunk_size(); i++)
      hashes << (remoteEntry.chunk(i).has_hash() ? Common::Hash(remoteEntry.chunk(i).hash().data()) : Common::Hash());

   File* file = new File(
      dir,
      remoteEntry.name().data(),
      remoteEntry.size(),
      QDateTime::currentDateTime(),
      hashes,
      true
   );

   // TODO : is there a better way to up cast ?
   QList< QSharedPointer<IChunk> > chunks;
   foreach (QSharedPointer<Chunk> chunk, file->getChunks())
      chunks << chunk;
   return chunks;
}

/**
  * Access the chunks of a file in an atomic way.
  */
QList< QSharedPointer<Chunk> > Cache::getChunks(const Protos::Common::Entry& fileEntry)
{
   QMutexLocker locker(&this->lock);

   File* file = this->getFile(fileEntry);
   if (!file)
      return QList< QSharedPointer<Chunk> >();

   return file->getChunks();
}

QStringList Cache::getSharedDirs(SharedDirectory::Rights rights) const
{
   QMutexLocker locker(&this->lock);
   QStringList list;

   for (QListIterator<SharedDirectory*> i(this->sharedDirs); i.hasNext();)
   {
      SharedDirectory* dir = i.next();
      if (dir->getRights() == rights)
         list << dir->getFullPath();
   }
   return list;
}

/**
  * @exception DirsNotFoundException
  */
void Cache::setSharedDirs(const QStringList& dirs, SharedDirectory::Rights rights)
{
   QMutexLocker locker(&this->lock);

   // Filter the actual shared directories by looking theirs rights.
   QList<SharedDirectory*> sharedDirsFiltered;
   for(QListIterator<SharedDirectory*> i(this->sharedDirs); i.hasNext();)
   {
      SharedDirectory* dir = i.next();
      if (dir->getRights() == rights)
         sharedDirsFiltered << dir;
   }

   QMutableListIterator<SharedDirectory*> j(sharedDirsFiltered);

   // Remove already shared directories from 'sharedDirsFiltered'.
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
      L_DEBU("Remove a shared directory : " + dir->getFullPath());
      this->removeSharedDir(dir);
   }

   // The duplicate dirs are found further because we compare only
   // directories with the same rights here.
   this->createSharedDirs(dirs, rights);
}

/**
  * Will inform the fileUpdater and delete 'dir'.
  * If 'dir2' is given 'dir' content (sub dirs + files) will be give to 'dir2'.
  * The directory is deleted by fileUpdater.
  */
void Cache::removeSharedDir(SharedDirectory* dir, Directory* dir2)
{
   QMutexLocker locker(&this->lock);

   this->sharedDirs.removeOne(dir);

   emit sharedDirectoryRemoved(dir, dir2);
}

SharedDirectory* Cache::getSuperSharedDirectory(const QString& path) const
{
   QMutexLocker locker(&this->lock);
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

QList<SharedDirectory*> Cache::getSubSharedDirectories(const QString& path) const
{
   QMutexLocker locker(&this->lock);
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

/**
  * If path matches a shared directory or one of its sub directories then true is returned.
  */
bool Cache::isShared(const QString& path) const
{
   QMutexLocker locker(&this->lock);
   foreach (SharedDirectory* dir, this->sharedDirs)
      if (dir->getFullPath() == path)
         return true;
   return false;
}

/**
  * Returns the directory that best matches to the given path.
  * For example, path = /home/peter/linux/distrib/debian/etch
  *  This directory exists in cache : /home/peter/linux/distrib
  *  Thus, this directory 'distrib' will be returned.
  * @param path An absolute path.
  * @return If no directory can be match 0 is returned.
  */
Directory* Cache::getFittestDirectory(const QString& path) const
{
   QMutexLocker locker(&this->lock);

   foreach (SharedDirectory* sharedDir, this->sharedDirs)
   {
      if (path.startsWith(sharedDir->getFullPath()))
      {
         QString relativePath(path);
         relativePath.remove(0, sharedDir->getFullPath().size());
         const QStringList folders = relativePath.split('/', QString::SkipEmptyParts);

         Directory* currentDir = sharedDir;
         foreach (QString folder, folders)
         {
            Directory* nextdir = currentDir->getSubDir(folder);
            if (!nextdir)
               break;
            currentDir = nextdir;
         }
         return currentDir;
      }
   }

   return 0;
}

/**
  * Define the shared directories from the persisted given data.
  * The directories and files are not created here but later by the fileUpdater, see the FileManager ctor.
  */
void Cache::retrieveFromFile(const Protos::FileCache::Hashes& hashes)
{
   this->createSharedDirs(hashes);
}

/**
  * Populate the given structure to be persisted later.
  */
void Cache::saveInFile(Protos::FileCache::Hashes& hashes) const
{
   hashes.set_version(1);
   hashes.set_chunksize(CHUNK_SIZE);

   for (QListIterator<SharedDirectory*> i(this->sharedDirs); i.hasNext();)
   {
      SharedDirectory* sharedDir = i.next();
      Protos::FileCache::Hashes_SharedDir* sharedDirMess = hashes.add_shareddir();

      sharedDirMess->mutable_id()->set_hash(sharedDir->getId().getData(), Common::Hash::HASH_SIZE);

      sharedDirMess->set_type(
         sharedDir->getRights() == SharedDirectory::READ_ONLY ?
            Protos::FileCache::Hashes_SharedDir_Type_READ :
            Protos::FileCache::Hashes_SharedDir_Type_READ_WRITE
      );

      sharedDirMess->set_path(sharedDir->getFullPath().toStdString());

      sharedDir->populateHashesDir(*sharedDirMess->mutable_root());
   }
}

quint64 Cache::getAmount() const
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
   if (SharedDirectory* sharedDir = dynamic_cast<SharedDirectory*>(entry))
      this->sharedDirs.removeOne(sharedDir);

   emit entryRemoved(entry);
}

void Cache::onChunkHashKnown(QSharedPointer<Chunk> chunk)
{
   emit chunkHashKnown(chunk);
}

void Cache::onChunkRemoved(QSharedPointer<Chunk> chunk)
{
   emit chunkRemoved(chunk);
}

/**
  * Create new shared directories and inform the fileUpdater.
  * @exception DirsNotFoundException
  */
void Cache::createSharedDirs(const QStringList& dirs, const QList<SharedDirectory::Rights>& rights, const QList<Common::Hash>& ids)
{
   QMutexLocker locker(&this->lock);

   QStringList dirsNotFound;

   SharedDirectory::Rights currentRights = SharedDirectory::READ_ONLY;

   QListIterator<QString> i(dirs);
   QListIterator<SharedDirectory::Rights> j(rights);
   QListIterator<Common::Hash> k(ids);
   while (i.hasNext())
   {
      QString path = i.next();

      if (j.hasNext())
         currentRights = j.next();

      try
      {
         SharedDirectory* dir = k.hasNext() ?
            new SharedDirectory(this, path, currentRights, k.next()) :
            new SharedDirectory(this, path, currentRights);

         L_DEBU(QString("Add a new shared directory : %1").arg(path));
         emit newSharedDirectory(dir);
         this->sharedDirs << dir;
      }
      catch (DirNotFoundException& e)
      {
         L_WARN(QString("Directory not found : %1").arg(e.path));
         dirsNotFound << e.path;
      }
      catch (DirAlreadySharedException&)
      {
         L_WARN(QString("Directory already shared : %1").arg(path));
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
   for (int i = 0; i < hashes.shareddir_size(); i++)
   {
      const Protos::FileCache::Hashes_SharedDir& dir = hashes.shareddir(i);
      paths << dir.path().data();
      rights << (dir.type() == Protos::FileCache::Hashes_SharedDir_Type_READ ? SharedDirectory::READ_ONLY : SharedDirectory::READ_WRITE)  ;
      ids << Common::Hash(dir.id().hash().data());
   }
   this->createSharedDirs(paths, rights, ids);
}

/**
  * Returns a directory wich matches to the path, it will choose the shared directory which :
  *  - Has at least the needed space.
  *  - Has the most directories in common with 'path'.
  *  - Can be written.
  *
  * The missing directories will be automatically created.
  *
  * @param path A relative path to a shared directory. Must be a cleaned path (QDir::cleanPath).
  * @return The directory, 0 if unkown error.
  * @exception NoReadWriteSharedDirectoryException
  * @exception InsufficientStorageSpaceException
  */
Directory* Cache::getWriteableDirectory(const QString& path, qint64 spaceNeeded) const
{
   QMutexLocker locker(&this->lock);

   const QStringList folders = path.split('/', QString::SkipEmptyParts);

   QList<SharedDirectory*> sharedDirsReadWrite;
   foreach (SharedDirectory* dir, this->sharedDirs)
      if (dir->getRights() == SharedDirectory::READ_WRITE)
         sharedDirsReadWrite << dir;

   if (sharedDirsReadWrite.isEmpty())
      throw NoReadWriteSharedDirectoryException();

   // Search for the best fitted shared directory.
   SharedDirectory* currentSharedDir = 0;
   int currentNbDirsInCommon = -1;

   foreach (SharedDirectory* dir, sharedDirsReadWrite)
   {
      if (Common::Global::availableDiskSpace(dir->getFullPath()) < spaceNeeded)
         continue;

      Directory* currentDir = dir;
      int nbDirsInCommon = 0;
      foreach (QString folder, folders)
      {
         currentDir = currentDir->getSubDir(folder);
         if (currentDir)
            nbDirsInCommon += 1;
         else
            break;
      }
      if (nbDirsInCommon > currentNbDirsInCommon)
      {
         currentNbDirsInCommon = nbDirsInCommon;
         currentSharedDir = dir;
      }
   }

   if (!currentSharedDir)
      throw InsufficientStorageSpaceException();

   // Create the missing directories.
   Directory* currentDir = currentSharedDir;
   foreach (QString folder, folders)
   {
      currentDir = currentDir->physicallyCreateSubDirectory(folder);
      if (!currentDir)
         return 0;
   }
   return currentDir;
}
