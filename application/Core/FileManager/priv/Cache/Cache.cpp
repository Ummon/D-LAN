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
#include <Common/Hash.h>
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
   MINIMUM_FREE_SPACE(SETTINGS.get<quint32>("minimum_free_space"))
{
   qRegisterMetaType<Entry*>("Entry*");
}

Cache::~Cache()
{
   // The event loop won't run anymore for this object: the trees are deleted synchronously, each root entry
   // deletes its shared entry.
   for (SharedEntry* sharedEntry : this->sharedEntries)
      delete sharedEntry->getRootEntry();
   this->sharedEntries.clear();
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
      Protos::Common::Entry* entry = result.add_entries();
      sharedEntry->populateEntry(entry);
   }

   return result;
}

Protos::Common::Entries Cache::getProtoEntries(const Protos::Common::Entry& dir, int maxNbHashesPerEntry) const
{
   QMutexLocker locker(&this->mutex);

   Protos::Common::Entries result;

   if (Directory* directory = this->getDirectory(dir))
   {
      foreach (Directory* dir, directory->getSubDirs())
         dir->populateEntry(result.add_entries(), true);

      foreach (File* file, directory->getFiles())
         if (file->isComplete())
            file->populateEntry(result.add_entries(), false, maxNbHashesPerEntry);
   }

   return result;
}

/**
  * a) Search among their shared directory the one who match the given directory.
  * b) In the shared directory try to find the directory corresponding to 'entry.dir.path'.
  */
Directory* Cache::getDirectory(const Protos::Common::Entry& dir) const
{
   Q_ASSERT(dir.type() == Protos::Common::Entry::Type::Entry_Type_DIR);

   // If we can't find the shared directory . . .
   if (!dir.has_shared_entry())
      return nullptr;

   QMutexLocker locker(&this->mutex);

   foreach (SharedEntry* sharedEntry, this->sharedEntries)
   {
      if (sharedEntry->getId() == dir.shared_entry().id().hash())
      {
         QStringList folders = QDir::cleanPath(QString::fromStdString(dir.path())).split('/', Qt::SkipEmptyParts);
         if (!dir.name().empty()) // An empty name means the dir is the root (a SharedDirectory).
            folders << QString::fromStdString(dir.name());

         Directory* currentDir = dynamic_cast<Directory*>(sharedEntry->getRootEntry());

         if (currentDir)
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
  * @return Returns a directory or a file. Returns 'nullptr' if no entry found.
  */
Entry* Cache::getEntry(const Common::Path& path) const
{
   if (path.isNull())
      return nullptr;

   Q_ASSERT(path.isAbsolute());

   QMutexLocker locker(&this->mutex);

   foreach (SharedEntry* sharedEntry, this->sharedEntries)
   {
      const Common::Path sharedEntryPath = sharedEntry->getPath();

      // Cover the case where 'sharedEntry' is a file.
      if (path == sharedEntryPath)
         return sharedEntry->getRootEntry();

      if (path.isSubOf(sharedEntryPath))
      {
         QStringList relativeDirs = path.getDirs();
         if (!sharedEntryPath.getDirs().isEmpty())
            relativeDirs.erase(relativeDirs.begin(), relativeDirs.begin() + sharedEntryPath.getDirs().length());

         Directory* directory =
            dynamic_cast<Directory*>(sharedEntry->getRootEntry()->getEntry(Common::Path(relativeDirs)));

         if (directory && path.isFile())
            return dynamic_cast<File*>(directory->getFile(path.getFilename()));
         else
            return directory;
      }
   }

   return nullptr;
}

SharedEntry* Cache::getSharedEntry(const Common::Path& path) const
{
   QMutexLocker locker(&this->mutex);

   for (SharedEntry* sharedEntry : this->sharedEntries)
      if (sharedEntry->getPath() == path)
         return sharedEntry;

   return nullptr;
}

/**
  * Try to find the file from the cache with the provided reference.
  * @return Returns 'nullptr' if the file hasn't be found.
  */
File* Cache::getFile(const Protos::Common::Entry& fileEntry) const
{
   const auto relativePath = Common::ProtoHelper::getPath(fileEntry);

   if (fileEntry.type() == Protos::Common::Entry_Type_DIR)
   {
      L_WARN(QString("Cache::getFile: 'fileEntry' must be a file (and not a directory): %1").arg(relativePath.toString()));
      return nullptr;
   }

   if (!fileEntry.has_shared_entry())
   {
      L_WARN(QString("Cache::getFile: 'fileEntry' doesn't have the field 'shared_dir' set: %1").arg(relativePath.toString()));
      return nullptr;
   }

   const auto sharedEntry = this->getSharedEntry(fileEntry.shared_entry().id().hash());   
   if (!sharedEntry)
   {
      L_WARN(QString("Cache::getFile: Unable to find the shared directory of the file: %1").arg(relativePath.toString()));

      return nullptr;
   }

   return dynamic_cast<File*>(sharedEntry->getRootEntry()->getEntry(relativePath));
}

/**
  * Create a new file in the path defined in 'fileEntry' and return its chunks.
  *
  * @exception NoWriteableDirectoryException
  * @exception InsufficientStorageSpaceException
  * @exception UnableToCreateNewFileException
  * @exception UnableToCreateNewDirException
  */
// TODO: Check the code in details, do we need to check the fileEntry type?
QList<QSharedPointer<IChunk>> Cache::newFile(Protos::Common::Entry& fileEntry)
{
   Q_ASSERT(fileEntry.type() == Protos::Common::Entry_Type_FILE);

   QMutexLocker locker(&this->mutex);

   const Common::Path dirPath(QString::fromStdString(fileEntry.path()));
   const qint64 spaceNeeded = fileEntry.size() + this->MINIMUM_FREE_SPACE;

   // If we know where to put the file.
   Directory* dir = nullptr;
   if (fileEntry.has_shared_entry())
   {
      SharedDirectory* sharedDirectory =
         dynamic_cast<SharedDirectory*>(this->getSharedEntry(fileEntry.shared_entry().id().hash()));

      if (sharedDirectory)
      {
         if (Common::Global::availableDiskSpace(sharedDirectory->getPath()) < spaceNeeded)
            throw InsufficientStorageSpaceException();

         dir = sharedDirectory->createSubDirs(dirPath.getDirs(), true);
      }
      else
         fileEntry.clear_shared_entry(); // The shared directory is invalid.
   }

   if (!dir)
      dir = this->getWriteableDirectory(dirPath, spaceNeeded);

   if (!dir)
      throw UnableToCreateNewFileException();
   else
      dir->populateSharedEntry(&fileEntry);

   QList<Common::Hash> hashes;
   for (int i = 0; i < fileEntry.chunks_size(); i++)
      hashes << fileEntry.chunks(i).hash();

   const QString name = QString::fromStdString(fileEntry.name());

   // If a file with the same name already exists we will compare its hashes with the given entry.
   File* file = dir->getFile(name);
   if (file != nullptr)
   {
      bool resetExistingFile = false;
      const QVector<QSharedPointer<Chunk>>& existingChunks = file->getChunks();
      if (existingChunks.size() != fileEntry.chunks_size())
         resetExistingFile = true;
      else
         for (int i = 0; i < existingChunks.size(); i++)
            if (existingChunks[i]->getHash() != Common::Hash(fileEntry.chunks(i).hash()))
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
         dir->getRoot(),
         name,
         fileEntry.size(),
         fileEntry.hidden(),
         QDateTime::currentDateTime(),
         dir,
         hashes,
         true
      );
   }

   fileEntry.set_exists(true); // File has been physically created.

   // TODO: Old code, remove.
   // dir->populateEntrySharedDir(&fileEntry); // We set the shared directory.

   // Is there a better way to up cast? An other method is shown below that uses 'reinterpret_cast'.
   QList<QSharedPointer<IChunk>> ichunks;
   const QVector<QSharedPointer<Chunk>>& chunks = file->getChunks();
   ichunks.reserve(chunks.size());
   for (QListIterator<QSharedPointer<Chunk>> i(chunks); i.hasNext();)
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
  *
  * TODO: throw ScanningException if needed.
  */
void Cache::newDirectory(Protos::Common::Entry& dirEntry)
{
   QMutexLocker locker(&this->mutex);

   const QString& dirPath =
      QDir::cleanPath(QString::fromStdString(dirEntry.path())) +
      '/' +
      QString::fromStdString(dirEntry.name());

   // If we know where to create the directory.
   Directory* dir = nullptr;
   if (dirEntry.has_shared_entry())
   {
      SharedDirectory* sharedDir =
         dynamic_cast<SharedDirectory*>(this->getSharedEntry(dirEntry.shared_entry().id().hash()));

      if (sharedDir)
         dir = sharedDir->createSubDirs(dirPath.split('/', Qt::SkipEmptyParts), true);
      else
         dirEntry.clear_shared_entry(); // The shared entry is invalid.
   }

   if (!dir)
      dir = this->getWriteableDirectory(dirPath);

   if (!dir)
      throw UnableToCreateNewDirException();
}

QList<Common::SharedEntry> Cache::getSharedEntries() const
{
   QMutexLocker locker(&this->mutex);

   QList<Common::SharedEntry> list;

   for (SharedEntry* sharedEntry : this->sharedEntries)
      list << makeSharedEntry(sharedEntry);

   return list;
}

SharedEntry* Cache::getSharedEntry(const Common::Hash& ID) const
{
   QMutexLocker locker(&this->mutex);

   for (QListIterator<SharedEntry*> i(this->sharedEntries); i.hasNext();)
   {
      SharedEntry* entry = i.next();
      if (entry->getId() == ID)
         return entry;
   }
   return nullptr;
}

/**
  * @exception EntriesNotFoundException
  */
void Cache::setSharedPaths(const QList<std::pair<QString, Common::Path>>& paths)
{
   QList<std::pair<QString, Common::Path>> pathsWithoutDuplicates;
   // Remove duplicates in paths (O(n^2)).
   for (const auto& path : paths)
   {
      for (const auto& path2 : pathsWithoutDuplicates)
         if (path.second == path2.second)
            goto nextPath;
      pathsWithoutDuplicates << path;
   nextPath:;
   }

   QMutexLocker locker(&this->mutex);

   QStringList pathsNotFound;

   int j = 0;
   for (int i = 0; i < pathsWithoutDuplicates.size(); i++) {
      for (int j2 = j; j2 < this->sharedEntries.size(); j2++) {
         if (pathsWithoutDuplicates[i].second == this->sharedEntries[j2]->getPath())
         {
            const QString trimmedName = pathsWithoutDuplicates[i].first.trimmed();
            this->sharedEntries[j2]->setUserName(trimmedName);
            this->sharedEntries.move(j2, j++);
            goto nextEntry;
         }
      }
      try
      {
         // dirs[i] not found -> we create a new one.
         if (this->createSharedEntry(pathsWithoutDuplicates[i].second, Common::Hash(), j, pathsWithoutDuplicates[i].first))
            j++;
      }
      catch (PathNotFoundException& e)
      {
         pathsNotFound << e.path;
      }
   nextEntry:;
   }

   while (j < this->sharedEntries.size())
      this->removeSharedEntry(this->sharedEntries[j]);

   for (int k = 0; k < this->sharedEntries.size(); k++)
      this->sharedEntries[k]->mergeSubSharedEntries();

   if (!pathsNotFound.isEmpty())
      throw EntriesNotFoundException(pathsNotFound);

   this->saveSharedEntries();
}

/**
  * @exception EntriesNotFoundException
  * @exception UnableToCreateSharedEntry
  */
QPair<Common::SharedEntry, QString> Cache::addASharedPath(const QString& absolutePath)
{
   QMutexLocker locker(&this->mutex);

   const Common::Path absolutePathCleaned = Common::Path(absolutePath);

   // If the given entry is already a shared entry.
   for (SharedEntry*& current : this->sharedEntries)
   {
      if (absolutePathCleaned == current->getPath())
         return qMakePair(makeSharedEntry(current), QString("/"));
   }

   // If the given entry is a sub item to an existing shared directory ('getSuperSharedDirectory(..)' only
   // returns a directory which strictly contains the given path).
   if (SharedDirectory* superDir = this->getSuperSharedDirectory(absolutePathCleaned))
   {
      QString relativePath(absolutePathCleaned);
      // TODO: Does it work in all cases?
      relativePath.remove(0, superDir->getPath().toString().length());
      relativePath.prepend('/');
      return qMakePair(makeSharedEntry(superDir), relativePath);
   }

   // Else we create a new shared entry.
   try
   {
      SharedEntry* entry = this->createSharedEntry(absolutePathCleaned);
      if (entry)
      {
         entry->mergeSubSharedEntries();
         this->saveSharedEntries();
         return qMakePair(makeSharedEntry(entry), QString("/"));
      }
      else
         throw UnableToCreateSharedEntry();
   }
   catch (PathNotFoundException& e)
   {
      throw EntriesNotFoundException(QStringList() << e.path);
   }
}

/**
  * @exception EntriesNotFoundException
  */
void Cache::addExistingSharedEntry(const Protos::Common::SharedEntry& sharedEntry)
{
   const QString path = QString::fromStdString(sharedEntry.path());

   try
   {
      if (!QFileInfo::exists(path))
         throw EntriesNotFoundException(QStringList{ path });

      const Common::Path commonPath = Common::Path(path);
      const QString name = QString::fromStdString(sharedEntry.shared_name());
      const Common::Hash id = Common::Hash(sharedEntry.id().hash());

      SharedEntry* entry = SharedEntry::create(this, path, id, name);

      L_DEBU(QString("Add an existing shared entry: %1").arg(path));

      this->sharedEntries << entry;

      emit newSharedEntry(entry);
   }
   catch (SharedEntryAlreadySharedException&)
   {
      L_DEBU(QString("Shared entry already shared: %1").arg(path));
   }
   catch (SuperDirectoryExistsException& e)
   {
      L_WARN(
         QString("There is already a super directory: %1 for this entry: %2").arg(e.superDirectory, e.subPath)
      );
   }
}

/**
  * Will inform the fileUpdater and delete 'entry'.
  * If 'dir' is given (not null) 'entry' content (sub dirs + files) will be given to 'dir'.
  * The item is deleted by 'fileUpdater'.
  */
void Cache::removeSharedEntry(SharedEntry* entry, Directory* dir)
{
   QMutexLocker locker(&this->mutex);

   if (this->sharedEntries.contains(entry))
   {
      this->sharedEntries.removeOne(entry);
      this->saveSharedEntries();
      emit sharedEntryRemoved(entry, dir);
   }
}

/**
  * Returns 'nullptr' if there is no super shared directory.
  */
SharedDirectory* Cache::getSuperSharedDirectory(const Common::Path& path) const
{
   QMutexLocker locker(&this->mutex);

   for (SharedEntry* entry : this->sharedEntries)
   {
      if (auto dir = dynamic_cast<SharedDirectory*>(entry))
      {
         if (dir->getPath().isSuperOf(path))
            return dir;
      }
   }

   return nullptr;
}

/**
  * Returns all shared entries contained in the given path.
  * 'path' must be a directory.
  */
QList<SharedEntry*> Cache::getSubSharedEntries(const Common::Path& path) const
{
   Q_ASSERT(!path.isFile());

   QMutexLocker locker(&this->mutex);

   QList<SharedEntry*> ret;
   for (SharedEntry* sharedEntry : this->sharedEntries)
      if (sharedEntry->getPath().isSubOf(path))
         ret << sharedEntry;
   return ret;
}

/**
  * If path matches a shared directory or one of its sub directories then true is returned.
  */
bool Cache::isShared(const Common::Path& path) const
{
   QMutexLocker locker(&this->mutex);

   for (auto entry: this->sharedEntries)
      if (entry->getPath() == path)
         return true;

   return false;
}

/**
  * Returns the directory that best matches to the given path.
  * For example, path = "/home/peter/linux/distrib/debian/etch/"
  *  This directory exists in cache : "/home/peter/linux/distrib/"
  *  Thus, this directory 'distrib' will be returned.
  * @param path An absolute path, it can be a file or a directory.
  * @return If no directory can be match 'nullptr' is returned.
  */
Directory* Cache::getFittestDirectory(const Common::Path& path) const
{
   QMutexLocker locker(&this->mutex);

   for (SharedEntry* sharedEntry : this->sharedEntries)
   {
      auto sharedDir = dynamic_cast<SharedDirectory*>(sharedEntry);
      if (!sharedDir)
         continue;

      const Common::Path& sharedPath = sharedDir->getPath();

      if (sharedPath == path)
         return sharedDir->getRootDir();

      if (sharedPath.isSuperOf(path))
      {
         QStringList pathDirs = path.getDirs();
         if (path.isFile())
            pathDirs << path.getFilename();

         Directory* currentDir = sharedDir->getRootDir();
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

qint64 Cache::getAmount() const
{
   QMutexLocker locker(&this->mutex);

   qint64 amount = 0;
   for (auto sharedEntry : this->sharedEntries)
      amount += sharedEntry->getRootEntry()->getSize();
   return amount;
}

QString entryAsStringDebug(Entry* entry)
{
   QString result;

   auto name = entry->getName();
   auto size = entry->getSize();
   auto indent = QString(" ").repeated(entry->getDepth() * 3);

   if (Directory* dir = dynamic_cast<Directory*>(entry))
   {
      result.append(QString("%4[%1] : %2 Bytes (%3)\n").arg(name).arg(size).arg(Common::Global::formatByteSize(size)).arg(indent));
      for (auto subFile : dir->getFiles())
         result += entryAsStringDebug(subFile);
      for (auto subDir : dir->getSubDirs())
         result += entryAsStringDebug(subDir);
   }
   else
   {
      result.append(QString("%4<%1> : %2 Bytes (%3)\n").arg(name).arg(size).arg(Common::Global::formatByteSize(size)).arg(indent));
   }

   return result;
}

QString Cache::getTree_debug() const
{
   QMutexLocker locker(&this->mutex);

   QString result;
   for (auto sharedDir : this->sharedEntries)
   {
      if (!result.isEmpty())
         result.append("---------------\n");
      result += entryAsStringDebug(sharedDir->getRootEntry());

   }
   return result;
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

void Cache::onFileResizing(File* file)
{
   emit fileResizing(file);
}

void Cache::onFileResized(File* file, qint64 oldSize)
{
   emit fileResized(file, oldSize);
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

/**
  * The location of a shared entry has changed (moved or renamed on the file system): persist the new one.
  */
void Cache::onSharedEntryPathChanged(SharedEntry* entry)
{
   QMutexLocker locker(&this->mutex);

   if (this->sharedEntries.contains(entry))
      this->saveSharedEntries();
}

void Cache::deleteEntry(Entry* entry)
{
   delete entry;
}

Common::SharedEntry Cache::makeSharedEntry(const SharedEntry* entry)
{
   return Common::SharedEntry {
      entry->getId(),
      entry->getPath(),
      entry->getUserName(),
      entry->getRootEntry()->getSize(),
      Common::Global::availableDiskSpace(entry->getPath())
   };
}

/**
  * Creates a new shared entry.
  * The other shared entries may not be merged with the new one,
  * use 'SharedEntry::mergeSubSharedEntries' to do that after this call.
  *
  * @exception PathNotFoundException
  */
SharedEntry* Cache::createSharedEntry(
   const Common::Path& path,
   const Common::Hash& id,
   int pos,
   const QString& name
)
{
   try
   {
      SharedEntry* entry = SharedEntry::create(this, path, id, name);

      L_DEBU(QString("Add a new shared entry: %1,").arg(path));

      if (pos == -1 || pos > this->sharedEntries.size())
         this->sharedEntries << entry;
      else
         this->sharedEntries.insert(pos, entry);

      emit newSharedEntry(entry);

      return entry;
   }
   catch (SharedEntryAlreadySharedException&)
   {
      L_DEBU(QString("Shared entry already shared: %1").arg(path));
   }
   catch (SuperDirectoryExistsException& e)
   {
      L_WARN(
         QString("There is already a super directory: %1 for this entry: %2").arg(e.superDirectory, e.subPath)
      );
   }

   return nullptr;
}

void Cache::saveSharedEntries() const
{
   QList<Protos::Common::SharedEntry> sharedEntries;

   for (auto sharedEntry : this->sharedEntries)
   {
      auto protoSharedEntry = Protos::Common::SharedEntry();
      protoSharedEntry.mutable_id()->set_hash(sharedEntry->getId().getData(), Common::Hash::HASH_SIZE);
      protoSharedEntry.set_shared_name(sharedEntry->getUserName().toStdString());
      protoSharedEntry.set_path(sharedEntry->getPath().toString().toStdString());

      sharedEntries << protoSharedEntry;
   }

   SETTINGS.set("shared_entries", sharedEntries);
   SETTINGS.save();
}

/**
  * Returns a directory which matches the given path, it will choose the shared item which:
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

   QList<SharedDirectory*> sharedDirs;
   for (auto i = this->sharedEntries.begin(); i != this->sharedEntries.end(); ++i)
      if (auto sharedDir = dynamic_cast<SharedDirectory*>(*i))
         sharedDirs << sharedDir;

   if (sharedDirs.isEmpty())
      throw NoWriteableDirectoryException();

   SharedDirectory* currentSharedDir = nullptr;
   int currentNbDirsInCommon = -1;

   for (SharedDirectory*& sharedDir : sharedDirs)
   {
      if (spaceNeeded > 0 && Common::Global::availableDiskSpace(sharedDir->getPath()) < spaceNeeded)
         continue;

      Directory* currentDir = sharedDir->getRootDir();
      int nbDirsInCommon = 0;
      for (QString& dirToSearch : path.getDirs())
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
         currentSharedDir = sharedDir;
      }
   }

   if (!currentSharedDir)
      throw InsufficientStorageSpaceException(); // Not executed if 'spaceNeeded' equals 0.

   // Create the missing directories.
   return currentSharedDir->getRootDir()->createSubDirs(path.getDirs(), true);
}
