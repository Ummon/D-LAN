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

#include <priv/Cache/SharedEntry.h>
using namespace FM;

#include <QDir>

#include <Common/ProtoHelper.h>
#include <Common/Global.h>
#include <Common/Path.h>

#include <Exceptions.h>
#include <priv/Log.h>
#include <priv/Exceptions.h>
#include <priv/Cache/Cache.h>
#include <priv/Cache/Directory.h>
#include <priv/Cache/File.h>

/**
  * If an existing shared entry is a sub directory or sub file then it will be merged with the new one.
  * @exception SuperDirectoryExistsException Thrown when a super shared directory already exists.
  * @exception SharedEntryAlreadySharedException
  * @exception FileNotFoundException
  * @exception DirNotFoundException
  */
/**
  * @param fullPath The path of the shared entry itself (file or directory), it's checked against the cache and the
  *                 file system. Only the path of its parent directory is kept, see 'SharedEntry::path'.
  */
SharedEntry::SharedEntry(
   Cache* cache,
   const Common::Path& fullPath,
   const Common::Hash& id,
   const QString& userName
) :
   cache(cache),
   id(id.isNull() ? Common::Hash::rand() : id),
   path(fullPath.removeLastElement()),
   userName(userName)
{
   const QString& pathStr = fullPath.toString();

   // Avoid two same entries.
   if (this->getCache()->isShared(fullPath))
      throw SharedEntryAlreadySharedException();

   // First of all check is the entry physically exists.
   if (fullPath.isFile() && !QFile(pathStr).exists())
      throw FileNotFoundException(pathStr);

   if (!fullPath.isFile() && !QDir(pathStr).exists())
      throw DirNotFoundException(pathStr);

   if (SharedDirectory* dir = this->cache->getSuperSharedDirectory(fullPath))
      throw SuperDirectoryExistsException(dir->getPath().toString(), pathStr);
}

/**
  * A factory to create a shared entry (file or directory) depending of the given path.
  */
SharedEntry* SharedEntry::create(
   Cache* cache,
   const QString& pathStr,
   const Common::Hash& id,
   const QString& userName
)
{
   return SharedEntry::create(cache, Common::Path(pathStr), id, userName);
}

/**
  * @exception PathNotFoundException
  */
SharedEntry* SharedEntry::create(
   Cache* cache,
   const Common::Path& path,
   const Common::Hash& id,
   const QString& userName
)
{
   const auto pathStr = path.toString();
   if (!QFileInfo::exists(pathStr))
      throw PathNotFoundException(pathStr);

   if (path.isFile())
      return new SharedFile(cache, path, id, userName);
   else
      return new SharedDirectory(cache, path, id, userName);
}

SharedEntry::~SharedEntry()
{
   L_DEBU(QString("SharedEntry deleted: %1").arg(this->getUserName()));
}

void SharedEntry::populateEntry(Protos::Common::Entry* entry) const
{
   this->getRootEntry()->populateEntry(entry, true);
}

void SharedEntry::del(bool invokeDelete)
{
   this->getRootEntry()->del(invokeDelete);
}

/**
  * Moves the content of this shared entry into the given directory and removes the shared entry from the cache.
  * For a shared directory, a sub directory with the same name is created in 'directory' and receives the content.
  * For a shared file, the file entry is only removed: the caller must rescan 'directory' to pick it up.
  * The shared entry and its root entry are deleted later by the 'FileUpdater'.
  */
void SharedEntry::moveInto(Directory* directory)
{
   // A shared entry can't be moved into its own tree.
   if (directory->getRoot() == this)
      return;

   Directory* rootDirectory = dynamic_cast<Directory*>(this->getRootEntry());
   this->getCache()->removeSharedEntry(
      this,
      rootDirectory ? directory->createSubDir(rootDirectory->getName()) : nullptr
   );
}

void SharedEntry::setPath(const Common::Path& path)
{
   {
      QMutexLocker locker(&this->metadataMutex);
      this->path = path;
   }
   this->getCache()->onSharedEntryPathChanged(this);
}

Common::Path SharedEntry::getParentPath() const
{
   QMutexLocker locker(&this->metadataMutex);
   return this->path;
}

Cache* SharedEntry::getCache() const
{
   return this->cache;
}

Common::Hash SharedEntry::getId() const
{
   return this->id;
}

QString SharedEntry::getUserName() const
{
   QMutexLocker locker(&this->metadataMutex);
   return this->userName;
}

void SharedEntry::setUserName(const QString& name)
{
   QString oldName;
   {
      QMutexLocker locker(&this->metadataMutex);
      if (this->userName == name)
         return;
      oldName = this->userName;
      this->userName = name;
   }
   this->getCache()->onEntryRenamed(this->getRootEntry(), oldName);
}

/////

SharedDirectory::SharedDirectory(
   Cache* cache,
   const Common::Path& path,
   const Common::Hash& id,
   const QString& userName
) :
   SharedEntry(
      cache,
      path,
      id,
      userName.isEmpty()
         ? (path.getDirs().isEmpty() ? path.getRoot() : path.getDirs().constLast())
         : userName
   ),
   directory(new Directory(this, path.getLastDir()))
{
}

SharedDirectory::~SharedDirectory()
{
}

/**
  * Try to merge other shared directories or files with this one.
  * For exemple /sharing/folder1/ and /sharing/folder2/a.txt can be merged with /sharing.
  * Should be called after each new SharedDirectory created.
  */
void SharedDirectory::mergeSubSharedEntries()
{
   // Merges the sub-entries (directories and files) of each directory found.
   foreach (SharedEntry* subEntry, this->getCache()->getSubSharedEntries(this->getPath()))
   {
      // Create the missing directories.
      const QStringList& parentFolders = this->getPath().getDirs();
      const QStringList& childFolders = subEntry->getPath().getDirs();
      Directory* current = this->directory;
      for (int i = parentFolders.size(); i < childFolders.size(); i++)
         current = current->createSubDir(childFolders[i]);

      this->getCache()->removeSharedEntry(subEntry, current);
   }
}

Directory* SharedDirectory::createSubDirs(const QStringList& names, bool physically)
{
   return this->directory->createSubDirs(names, physically);
}

Entry* SharedDirectory::getRootEntry() const
{
   return this->directory;
}

Common::Path SharedDirectory::getPath() const
{
   return this->getParentPath().appendDir(this->directory->getName());
}

Directory* SharedDirectory::getRootDir() const
{
   return this->directory;
}

/////

SharedFile::SharedFile(
   Cache* cache,
   const Common::Path& path,
   const Common::Hash& id,
   const QString& userName
) :
   SharedEntry(
      cache,
      path,
      id,
      userName.isNull() ? path.getFilename() : userName
   )
{
   auto fileInfo = QFileInfo(path.toString());
   this->file = new File(this, path.getFilename(), fileInfo.size(), fileInfo.isHidden(), fileInfo.lastModified());
}

SharedFile::~SharedFile()
{
}

void SharedFile::mergeSubSharedEntries()
{
   // We can't merge another shared entry into a file.
}

Entry* SharedFile::getRootEntry() const
{
   return this->file;
}

Common::Path SharedFile::getPath() const
{
   return this->getParentPath().setFilename(this->file->getName());
}

File* SharedFile::getRootFile() const
{
   return this->file;
}