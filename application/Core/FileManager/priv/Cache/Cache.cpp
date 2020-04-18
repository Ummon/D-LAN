/**
  * D-LAN - A decentralized LAN file sharing software.
  * Copyright (C) 2010-2012 Greg Burri <greg.burri@gmail.com>
  *
  * This program is free software: you can redistribute it and/or modify
  * it under the terms of the GNU General Public License as published by
  * the Free Software Foundation, either version 3 of the License, or
  * (at your option) any later version.
  *
  * This program is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  *
  * You should have received a copy of the GNU General Public License
  * along with this program.  If not, see <http://www.gnu.org/licenses/>.
  */

#include <priv/Cache/Cache.h>
using namespace FM;

#include <QDir>
#include <QQueue>

#include <Common/Global.h>
#include <Common/Settings.h>
#include <Common/ProtoHelper.h>
#include <Common/Path.h>
#include <Common/Constants.h>

#include <Exceptions.h>
#include <priv/Log.h>
#include <priv/Exceptions.h>
#include <priv/Constants.h>
#include <priv/Cache/SharedEntry.h>
#include <priv/Cache/Directory.h>
#include <priv/Cache/File.h>

/**
  * @class FM::Cache
  *
  * Owns all the shared items (roots), their content (directories and file) and the chunks.
  * Here are the main capabilities:
  *  - Browse directories and files.
  *  - Create a new file.
  *  - Add or remove a shared item (root).
  *  - Give or retrieve hashes from hash cache (namespace HC).
  */

Cache::Cache(QSharedPointer<HC::IHashCache> hashCache) :
   hashCache(hashCache),
   MINIMUM_FREE_SPACE(SETTINGS.get<quint32>("minimum_free_space")),
   mutex(QMutex::Recursive)
{
   qRegisterMetaType<Entry*>("Entry*");
}

Cache::~Cache()
{
   for (auto i = this->sharedEntries.begin(); i != this->sharedEntries.end(); ++i)
      (*i)->del();
}

/**
  * Call the given lambda for each entries owned by the cache.
  * It can be a a directory or a file.
  */
void Cache::forall(std::function<void(Entry*)> fun) const
{
   QQueue<Entry*> entries;
   foreach (SharedEntry* entry, this->sharedEntries)
      entries.enqueue(entry->getRootEntry());

   while (!entries.isEmpty())
   {
      Entry* current = entries.dequeue();
      fun(current);

      Directory* dir = dynamic_cast<Directory*>(current);
      if (dir)
      {
         foreach (File* file, dir->getFiles())
            fun(file);
         foreach (Directory* subDir, dir->getSubDirs())
            entries.enqueue(subDir);
      }
   }
}

/**
  * Gets the roots shared entries (it can be a mix of files and directories).
  */
Protos::Common::Entries Cache::getProtoSharedEntries() const
{
   QMutexLocker locker(&this->mutex);

   Protos::Common::Entries result;

   foreach (SharedEntry* sharedEntry, this->sharedEntries)
   {
      Protos::Common::Entry* entry = result.add_entry();
      sharedEntry->populateEntry(entry);
   }

   return result;
}

Protos::Common::Entries Cache::getProtoEntries(const Protos::Common::Entry& dir, int maxNbHashesPerEntry) const
{
   Protos::Common::Entries result;

   if (Directory* directory = this->getDirectory(dir))
   {
      foreach (Directory* dir, directory->getSubDirs())
         dir->populateEntry(result.add_entry());

      foreach (File* file, directory->getFiles())
         if (file->isComplete())
            file->populateEntry(result.add_entry(), false, maxNbHashesPerEntry);
   }

   return result;
}

/**
  * a) Search among their shared directory the one who match the given directory.
  * b) In the shared directory try to find the directory corresponding to 'entry.dir.path'.
  */
Directory* Cache::getDirectory(const Protos::Common::Entry& dir) const
{
   // If we can't find the shared directory . . .
   if (!dir.has_shared_entry())
      return nullptr;

   QMutexLocker locker(&this->mutex);

   foreach (SharedEntry* sharedEntry, this->sharedEntries)
   {
      if (sharedEntry->getId() == dir.shared_entry().id().hash())
      {
         QStringList folders = QDir::cleanPath(Common::ProtoHelper::getStr(dir, &Protos::Common::Entry::path)).split('/', QString::SkipEmptyParts);

         if (!dir.path().empty()) // An empty path means the dir is the root (a SharedDirectory).
            folders << Common::ProtoHelper::getStr(dir, &Protos::Common::Entry::name);

         Directory* currentDir = dynamic_cast<Directory*>(sharedEntry->getRootEntry());

         if (currentDir != nullptr)
         {
            foreach (QString folder, folders)
            {
               currentDir = currentDir->getSubDir(folder);
               if (!currentDir)
                  return nullptr;
            }
         }

         return currentDir;
      }
   }

   return nullptr;
}

/**
  * @param path The absolute path to a directory or a file.
  * @return Returns a directory or a file, it can be a shared file or a shared directory. Returns 'nullptr' if no entry found.
  */
Entry* Cache::getEntry(const Common::Path& path) const
{
   Q_ASSERT(path.isAbsolute());

   QMutexLocker locker(&this->mutex);

   foreach (SharedEntry* sharedEntry, this->sharedEntries)
   {
      const Common::Path sharedEntryPath = sharedEntry->getFullPath();

      // Cover the case where 'sharedEntry' is a file.
      if (path == sharedEntryPath)
         return sharedEntry->getRootEntry();

      if (path.isSuperOf(sharedEntryPath))
      {
         QStringList relativeDirs = path.getDirs();
         if (!sharedEntryPath.getDirs().isEmpty())
            relativeDirs.erase(relativeDirs.begin(), relativeDirs.begin() + sharedEntryPath.getDirs().length());

         Directory* currentDirectory = dynamic_cast<Directory*>(sharedEntry->getRootEntry());

         for (QStringListIterator i(relativeDirs); i.hasNext();)
            currentDirectory = currentDirectory->getSubDir(i.next());

         if (path.isFile())
            return dynamic_cast<File*>(currentDirectory->getFile(path.getFilename()));
         else
            return currentDirectory;
      }

      /*
       TODO: Old code -> to remove.

      if (path.startsWith(currentPath) && (path.size() == currentPath.size() || path[currentPath.size()] == '/'))
      {
         QString relativePath(path);
         relativePath.remove(0, currentPath.size());
         const QStringList folders = relativePath.split('/', QString::SkipEmptyParts);

         Directory* currentDir = sharedEntry;
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
               return nullptr;
            }
            currentDir = dir;
         }

         return currentDir;
      }
      */
   }

   return nullptr;
}

SharedEntry* Cache::getSharedEntry(const Common::Path& path) const
{
   QMutexLocker locker(&this->mutex);

   foreach (SharedEntry* sharedEntry, this->sharedEntries)
      if (sharedEntry->getFullPath() == path)
         return sharedEntry;

   return nullptr;
}

/**
  * Try to find the file from the cache with the provided reference.
  * @return Returns 'nullptr' if the file hasn't be found.
  */
File* Cache::getFile(const Protos::Common::Entry& fileEntry) const
{
   if (fileEntry.type() == Protos::Common::Entry_Type_DIR)
   {
      L_WARN(QString("Cache::getFile: 'fileEntry' must be a file (and not a directory)"));
      return nullptr;
   }

   if (!fileEntry.has_shared_entry())
   {
      L_WARN(QString("Cache::getFile: 'fileEntry' doesn't have the field 'shared_dir' set!"));
      return nullptr;
   }

   return dynamic_cast<File*>(this->getEntry(Common::ProtoHelper::getPath(fileEntry, Common::EntriesToAppend::FILE, true)));


   /*
   TODO: Old code -> to remove.

   QMutexLocker locker(&this->mutex);

   if (!fileEntry.has_shared_entry())
   {
      L_WARN(QString("Cache::getFile: 'fileEntry' doesn't have the field 'shared_dir' set!"));
      return nullptr;
   }

   foreach (SharedDirectory* sharedDir, this->sharedDirs)
   {
      if (sharedDir->getId() == fileEntry.shared_dir().id().hash())
      {
         const QString& relativePath = Common::ProtoHelper::getStr(fileEntry, &Protos::Common::Entry::path);
         const QStringList folders = relativePath.split('/', QString::SkipEmptyParts);

         Directory* dir = sharedDir;
         QStringListIterator i(folders);
         forever
         {
            if (dir)
            {
               if (!i.hasNext())
               {
                  File* file = dir->getFile(Common::ProtoHelper::getStr(fileEntry, &Protos::Common::Entry::name));

                  if (file)
                     return file;
                  return nullptr;
               }
            }
            else
               return nullptr;

            if (!i.hasNext())
               return nullptr;

            dir = dir->getSubDir(i.next());
         }

         return nullptr;
      }
   }

   return nullptr;
   */
}

/**
  * Create a new file in the path defined in 'fileEntry' and return its chunks.
  *
  * @exception NoWriteableDirectoryException
  * @exception InsufficientStorageSpaceException
  * @exception UnableToCreateNewFileException
  * @exception UnableToCreateNewDirException
  */
QList<QSharedPointer<IChunk>> Cache::newFile(Protos::Common::Entry& fileEntry)
{
   QMutexLocker locker(&this->mutex);

   const QString& dirPath = QDir::cleanPath(Common::ProtoHelper::getStr(fileEntry, &Protos::Common::Entry::path));
   const qint64 spaceNeeded = fileEntry.size() + this->MINIMUM_FREE_SPACE;

   // If we know where to put the file.
   Directory* dir = nullptr;
   if (fileEntry.has_shared_entry())
   {
      SharedDirectory* sharedDir = this->getSharedEntry(fileEntry.shared_dir().id().hash());

      if (sharedDir)
      {
         if (Common::Global::availableDiskSpace(sharedDir->getFullPath()) < spaceNeeded)
            throw InsufficientStorageSpaceException();

         dir = sharedDir->createSubDirs(dirPath.split('/', QString::SkipEmptyParts), true);
      }
      else
         fileEntry.clear_shared_dir(); // The shared directory is invalid.
   }

   if (!dir)
      dir = this->getWriteableDirectory(dirPath, spaceNeeded);

   if (!dir)
      throw UnableToCreateNewFileException();

   Common::Hashes hashes;
   for (int i = 0; i < fileEntry.chunk_size(); i++)
      hashes << fileEntry.chunk(i).hash();

   const QString& name = Common::ProtoHelper::getStr(fileEntry, &Protos::Common::Entry::name);

   // If a file with the same name already exists we will compare its hashes with the given entry.
   File* file = dir->getFile(name);
   if (file != nullptr)
   {
      bool resetExistingFile = false;
      const QVector<QSharedPointer<Chunk>>& existingChunks = file->getChunks();
      if (existingChunks.size() != fileEntry.chunk_size())
         resetExistingFile = true;
      else
         for (int i = 0; i < existingChunks.size(); i++)
            if (existingChunks[i]->getHash() != Common::Hash(fileEntry.chunk(i).hash()))
            {
               resetExistingFile = true;
               break;
            }

      if (resetExistingFile)
         file->setToUnfinished(fileEntry.size(), hashes);
   }
   else
   {
      file = new File(
         dir,
         name,
         fileEntry.size(),
         QDateTime::currentDateTime(),
         hashes,
         true
      );
   }

   fileEntry.set_exists(true); // File has been physically created.
   dir->populateEntrySharedDir(&fileEntry); // We set the shared directory.

   // Is there a better way to up cast? An other method is shown below that uses 'reinterpret_cast'.
   QList<QSharedPointer<IChunk>> ichunks;
   const QVector<QSharedPointer<Chunk>>& chunks = file->getChunks();
   ichunks.reserve(chunks.size());
   for (QVectorIterator<QSharedPointer<Chunk>> i(chunks); i.hasNext();)
      ichunks << i.next();
   return ichunks;

   // This method works but 'reinterpret_cast' is too dangerous. (only if 'File::getChunks()' return a QList).
   // QList<QSharedPointer<Chunk>> chunks = file->getChunks();
   // return *(reinterpret_cast<QList<QSharedPointer<IChunk>>*>(&chunks));
}

/**
  * @exception ScanningException The entry or one of their parents is currently being scanned
  * @exception NoWriteableDirectoryException
  * @exception UnableToCreateNewDirException
  */
void Cache::newDirectory(Protos::Common::Entry& dirEntry)
{
   TODO: ScanningException

   QMutexLocker locker(&this->mutex);

   const QString& dirPath = QDir::cleanPath(Common::ProtoHelper::getStr(dirEntry, &Protos::Common::Entry::path)) + '/' + Common::ProtoHelper::getStr(dirEntry, &Protos::Common::Entry::name);

   // If we know where to create the directory.
   Directory* dir = nullptr;
   if (dirEntry.has_shared_dir())
   {
      SharedDirectory* sharedDir = this->getSharedDirectory(dirEntry.shared_dir().id().hash());
      if (sharedDir)
         dir = sharedDir->createSubDirs(dirPath.split('/', QString::SkipEmptyParts), true);
      else
         dirEntry.clear_shared_dir(); // The shared directory is invalid.
   }

   if (!dir)
      dir = this->getWriteableDirectory(dirPath);

   if (!dir)
      throw UnableToCreateNewDirException();
}

QList<Common::SharedDir> Cache::getSharedDirs() const
{
   QMutexLocker locker(&this->mutex);

   QList<Common::SharedDir> list;

   for (QListIterator<SharedDirectory*> i(this->sharedDirs); i.hasNext();)
      list << makeSharedEntry(i.next());

   return list;
}

SharedEntry* Cache::getSharedEntry(const Common::Hash& ID) const
{
   for (QListIterator<SharedEntry*> i(this->sharedEntries); i.hasNext();)
   {
      SharedEntry* entry = i.next();
      if (entry->getId() == ID)
         return entry;
   }
   return nullptr;
}

/**
  * @exception ItemsNotFoundException
  */
void Cache::setSharedPaths(const QList<Common::Path>& path)
{
   QMutexLocker locker(&this->mutex);

   QStringList dirsNotFound;

   int j = 0; // currentDirs.
   for (int i = 0; i < dirs.size(); i++) // dirs.
   {
      for (int j2 = j; j2 < this->sharedDirs.size(); j2++)
      {
         const QString dir = Common::Path::cleanDirPath(dirs[i]);
         if (dir == this->sharedDirs[j2]->getFullPath())
         {
            this->sharedDirs.move(j2, j++);
            goto nextDir;
         }
      }
      try
      {
         // dirs[i] not found -> we create a new one.
         if (this->createSharedDir(dirs[i], Common::Hash(), j))
            j++;
      }
      catch (SharedEntryNotFoundException& e)
      {
         dirsNotFound << e.path;
      }
   nextDir:;
   }

   while (j < this->sharedDirs.size())
      this->removeSharedDir(this->sharedDirs[j]);

   for (int k = 0; k < this->sharedDirs.size(); k++)
      this->sharedDirs[k]->mergeSubSharedEntries();

   if (!dirsNotFound.isEmpty())
      throw ItemsNotFoundException(dirsNotFound);
}

/**
  * @exception ItemsNotFoundException
  */
QPair<Common::SharedEntry, QString> Cache::addASharedEntry(const Protos::Common::SharedEntry& sharedEntry)
{
   QMutexLocker locker(&this->mutex);

   QString absoluteDirCleaned = Common::Path::cleanDirPath(absoluteDir);

   // If the given directory is already a shared directory
   for (QListIterator<SharedDirectory*> i(this->sharedDirs); i.hasNext();)
   {
      SharedDirectory* current = i.next();
      if (absoluteDirCleaned == current->getFullPath())
         return qMakePair(makeSharedEntry(current), QString("/"));
   }

   // If the given directory is a sub directory to an existing shared directory
   SharedDirectory* superDir = this->getSuperSharedDirectory(absoluteDirCleaned);
   if (superDir && absoluteDirCleaned.indexOf(superDir->getFullPath()) == 0)
   {
      QString relativeDir(absoluteDirCleaned);
      relativeDir.remove(0, superDir->getFullPath().length());
      relativeDir.prepend('/');
      return qMakePair(makeSharedEntry(superDir), relativeDir);
   }

   // Else we create a new shared directory
   try
   {
      SharedDirectory* dir = this->createSharedDir(absoluteDirCleaned);
      if (dir)
      {
         dir->mergeSubSharedDirectories();
         return qMakePair(makeSharedEntry(dir), QString("/"));
      }
      else
         throw UnableToCreateSharedDirectory();
   }
   catch (SharedEntryNotFoundException& e)
   {
      throw ItemsNotFoundException(QStringList() << e.path);
   }
}

/**
  * Will inform the fileUpdater and delete 'item'.
  * If 'dir' is given 'item' content (sub dirs + files) will be given to 'dir'.
  * The item is deleted by 'fileUpdater'.
  */
void Cache::removeSharedEntry(SharedEntry* entry, Directory* dir)
{
   QMutexLocker locker(&this->mutex);

   if (this->sharedEntries.contains(entry))
   {
      this->sharedEntries.removeOne(entry);
      emit sharedEntryRemoved(entry, dir);
   }
}

SharedItem* Cache::getSuperSharedItem(const Common::Path& path) const
{
   QMutexLocker locker(&this->mutex);

   for (QListIterator<SharedItem*> i(this->sharedItems); i.hasNext();)
   {
      SharedItem* sharedItem = i.next();
      if (sharedItem->getFullPath().isSuperOf(path))
         return sharedItem;
   }

   return nullptr;
}

QList<SharedItem*> Cache::getSubSharedItems(const Common::Path& path) const
{
   QMutexLocker locker(&this->mutex);
   QList<SharedItem*> ret;

   for (QListIterator<SharedItem*> i(this->sharedItems); i.hasNext();)
   {
      SharedItem* sharedItem = i.next();
      if (sharedItem->getFullPath().isSubOf(path))
         ret << sharedItem;
   }

   return ret;
}

/**
  * If path matches a shared directory or one of its sub directories then true is returned.
  */
bool Cache::isShared(const Common::Path& path) const
{
   QMutexLocker locker(&this->mutex);
   foreach (SharedItem* item, this->sharedItems)
      if (item->getFullPath() == path)
         return true;
   return false;
}

/**
  * Returns the directory that best matches to the given path.
  * For example, path = "/home/peter/linux/distrib/debian/etch/"
  *  This directory exists in cache : "/home/peter/linux/distrib/"
  *  Thus, this directory 'distrib' will be returned.
  * @param path An absolute path.
  * @return If no directory can be match 'nullptr' is returned.
  */
Directory* Cache::getFittestDirectory(const Common::Path& path) const
{
   QMutexLocker locker(&this->mutex);

   foreach (SharedEntry* sharedEntry, this->sharedEntries)
   {
      const Common::Path& sharedPath = sharedEntry->getFullPath();

      if (sharedPath == path)
         return sharedEntry->getEntry();

      if (sharedPath.isSuperOf(path))
      {
         const QStringList& pathDirs = path.getDirs();
         Directory* currentDir = dynamic_cast<Directory*>(sharedEntry->getEntry());
         for (int i = sharedPath.getDirs().size(); i < pathDirs.size(); i++)
         {
            Directory* nextdir = currentDir->getSubDir(pathDirs[i]);
            if (!nextdir)
               return currentDir;
            currentDir = nextdir;
         }
      }
   }

   return nullptr;
}

/**
  * Defines the shared items from the persisted given data.
  * The directories and files are not created here but later by the FileUpdater, see the FileManager ctor.
  */
void Cache::createSharedItems(const google::protobuf::RepeatedPtrField<Protos::Common::SharedItem>& items)
{
   QStringList paths;
   QList<Common::Hash> ids;

   // Add the shared directories from the file cache.
   for (int i = 0; i < items.size(); i++)
   {
      paths << Common::ProtoHelper::getStr(items.Get(i), &Protos::Common::SharedItem::path);
      ids << items.Get(i).id().hash();
   }
   this->createSharedItems(paths, ids);
}

/**
  * Populates the given structure to be persisted later.
  */
/*
void Cache::populateHashes(Protos::FileCache::Hashes& hashes) const
{
   // TODO during hash cache implementation.

   QMutexLocker locker(&this->mutex);

   hashes.set_version(FILE_CACHE_VERSION);
   hashes.set_chunksize(Common::Constants::CHUNK_SIZE);

   for (QListIterator<SharedDirectory*> i(this->sharedDirs); i.hasNext();)
   {
      SharedDirectory* sharedDir = i.next();
      Protos::FileCache::Hashes_SharedDir* sharedDirMess = hashes.add_shareddir();
      sharedDirMess->mutable_id()->set_hash(sharedDir->getId().getData(), Common::Hash::HASH_SIZE);
      Common::ProtoHelper::setStr(*sharedDirMess, &Protos::FileCache::Hashes_SharedDir::set_path, sharedDir->getFullPath());

      sharedDir->populateHashesDir(*sharedDirMess->mutable_root());
   }
}
*/

quint64 Cache::getAmount() const
{
   QMutexLocker locker(&this->mutex);

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

void Cache::onEntryRenamed(Entry* entry, const QString& oldName)
{
   emit entryRenamed(entry, oldName);
}

void Cache::onEntryResizing(Entry* entry)
{
   emit entryResizing(entry);
}

void Cache::onEntryResized(Entry* entry, qint64 oldSize)
{
   emit entryResized(entry, oldSize);
}

void Cache::onChunkHashKnown(const QSharedPointer<Chunk>& chunk)
{
   emit chunkHashKnown(chunk);
}

void Cache::onChunkRemoved(const QSharedPointer<Chunk>& chunk)
{
   emit chunkRemoved(chunk);
}

void Cache::onScanned(Directory* dir)
{
   emit directoryScanned(dir);
}

void Cache::deleteEntry(Entry* entry)
{
   delete entry;
}

Common::SharedEntry Cache::makeSharedEntry(const SharedEntry* entry)
{
   return Common::SharedEntry{ entry->getId(), entry->getFullPath(), entry->getSize(), Common::Global::availableDiskSpace(entry->getFullPath()) };
}

/**
  * Creates a new shared item.
  * The other shared items may not be merged with the new one, use 'SharedItems::mergeSubSharedItems' to do that after this call.
  *
  * @exception SharedEntryNotFoundException
  */
SharedItem* Cache::createSharedEntry(const Common::Path& path, const Common::Hash& ID, int pos)
{
   try
   {
      SharedItem* item = !ID.isNull() ?
         new SharedItem(this, path, ID) :
         new SharedItem(this, path);

      L_DEBU(QString("Add a new shared item: %1").arg(path));

      if (pos == -1 || pos > this->sharedItems.size())
         this->sharedItems << item;
      else
         this->sharedItems.insert(pos, item);

      emit newSharedItem(dir);

      return item;
   }
   catch (SharedEntryAlreadySharedException&)
   {
      L_DEBU(QString("Shared entry already shared: %1").arg(path));
   }
   catch (SuperDirectoryExistsException& e)
   {
      L_WARN(QString("There is already a super directory: %1 for this directory: %2").arg(e.superDirectory).arg(e.subDirectory));
   }

   return nullptr;
}

/**
  * Create new shared items.
  *
  * @exception ItemsNotFoundException
  */
void Cache::createSharedPaths(const QList<Common::Path>& paths, const QList<Common::Hash>& ids)
{
   QMutexLocker locker(&this->mutex);

   QStringList itemsNotFound;

   QListIterator<QString> i(paths);
   QListIterator<Common::Hash> k(ids);
   while (i.hasNext())
   {
      QString path = i.next();

      try
      {
         SharedItem* item = k.hasNext() ?
            this->createSharedItem(path, k.next()) :
            this->createSharedItem(path);

         if (item)
            item->mergeSubSharedItems();
      }
      catch (DirNotFoundException& e)
      {
         itemsNotFound << e.path;
      }
   }

   if (!itemsNotFound.isEmpty())
      throw ItemsNotFoundException(itemsNotFound);
}

/**
  * Returns a directory which matches to the path, it will choose the shared item which :
  *  - Is a directory
  *  - Has at least the needed space.
  *  - Has the most directories in common with 'path'.
  *
  * The missing directories will be automatically created.
  *
  * @param path A relative path to a directory. Must be a cleaned path (QDir::cleanPath).
  * @param spaceNeeded The number of storage space needed, if no directory can be found the exception 'InsufficientStorageSpaceException' is thrown.
  * @return The directory, 0 if unknown error.
  * @exception InsufficientStorageSpaceException (only if 'spaceNeeded' > 0)
  * @exception NoWriteableDirectoryException
  * @exception UnableToCreateNewDirException
  */
Directory* Cache::getWriteableDirectory(const Common::Path& path, qint64 spaceNeeded) const
{
   QMutexLocker locker(&this->mutex);

   TODO...

   QList<SharedItem*> sharedDirs;
   for (auto i = this->sharedItems.begin(); i != this->sharedItems.end(); ++i)
      if ((*i)->getKind() == SharedItem::Kind::DIR)
         sharedDirs << *i;

   if (sharedDirs.isEmpty())
      throw NoWriteableDirectoryException();

   SharedItem* currentSharedDir = nullptr;
   int currentNbDirsInCommon = -1;

   for (SharedItem* dir : sharedDirs)
   {
      if (spaceNeeded > 0 && Common::Global::availableDiskSpace(dir->getFullPath()) < spaceNeeded)
         continue;

      Directory* currentDir = dynamic_cast<Directory*>(dir->getEntry());
      int nbDirsInCommon = 0;
      for (QString dirToSearch : path.getDirs())
      {
         currentDir = currentDir->getSubDir(dirToSearch);
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
      throw InsufficientStorageSpaceException(); // Not executed if 'spaceNeeded' equals 0.

   // Create the missing directories.
   return dynamic_cast<Directory*>(currentSharedDir->getEntry())->createSubDirs(folders, true);
}
