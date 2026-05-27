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
SharedEntry::SharedEntry(
   Cache* cache,
   const Common::Path& path,
   const Common::Hash& id,
   const QString& userName
) :
   cache(cache),
   path(path),
   id(id.isNull() ? Common::Hash::rand() : id),
   userName(userName)
{
   const QString& pathStr = path.toString();

   // if (userName.isNull())
   //    this->userName = entryName(path);
   // else
   //    this->userName = userName;

   // Avoid two same directories.
   if (this->getCache()->isShared(pathStr))
      throw SharedEntryAlreadySharedException();

   // First of all check is the directory physically exists.
   if (path.isFile() && !QFile(pathStr).exists())
      throw FileNotFoundException(pathStr);

   if (!path.isFile() && !QDir(pathStr).exists())
      throw DirNotFoundException(pathStr);

   if (SharedDirectory* dir = this->cache->getSuperSharedDirectory(pathStr))
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
   entry->set_path(""); // The path of a shared directory is private (we don't want the other peers to see absolute paths).
}

void SharedEntry::del(bool invokeDelete)
{
   // The question is: why we don't let 'Directory::del()' destroys its sub directories?
   // This is because a concurrent access to 'Directory::getRoot()' during a delete of a shared directory must be
   // able to access the shared director.
   // this->deleteSubDirs();

   this->getRootEntry()->del(invokeDelete);
}

/**
  * Moves the content of this shared entry (file or directory) into the given directory.
  */
// void SharedEntry::moveInto(Directory* directory)
// {
//    // A directory can't be move in its own tree.
//    if (this->getRootEntry()->getRoot() == this)
//       return;

//    this->getCache()->removeSharedEntry(this, directory->createSubDir(this->getRootEntry()->getName()));
// }

// void SharedEntry::moveInto(const QString& path)
// {
//    this->path = Common::Path(path);
// }

void SharedEntry::setPath(const Common::Path& path)
{
   this->path = path;
}

Cache* SharedEntry::getCache() const
{
   return this->cache;
}

// Common::Path SharedEntry::getPath() const
// {
//    return this->path;
// }

Common::Hash SharedEntry::getId() const
{
   return this->id;
}

QString SharedEntry::getUserName() const
{
   return this->userName;
}

/**
  * Extract the entry name. The entry name is a user name and will not be used in a real path.
  * 'C:/User/Paul/Movies/' -> 'Movies'
  * 'C:/User/Paul/Movies/movie.avi' -> 'movie.avi'
  * '/' -> '/'
  * 'C:/' -> 'C:/'
  */
// QString SharedEntry::entryName(const Common::Path& path)
// {
//    if (path.isFile())
//       return path.getFilename();

//    if (path.getDirs().isEmpty())
//       return path.getRoot();
//    else
//       return path.getDirs().constLast();
// }

// Common::Path SharedEntry::pathWithoutEntryName(const Common::Path& path)
// {
//    if (path.isFile())
//       return path.removeFilename();
//    else
//       return path.removeLastDir();
// }

/////

SharedDirectory::SharedDirectory(
   Cache* cache,
   const Common::Path& path,
   const Common::Hash& id,
   const QString& userName
) :
   SharedEntry(
      cache,
      path.removeLastDir(),
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
   delete this->directory;
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
   return this->path.appendDir(this->directory->getName());
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
      path.removeFilename(),
      id,
      userName.isNull() ? path.getFilename() : userName
   )
{
   auto fileInfo = QFileInfo(path.toString());
   this->file = new File(this, path.getFilename(), fileInfo.size(), fileInfo.lastModified());
}

SharedFile::~SharedFile()
{
   delete this->file;
}

void SharedFile::mergeSubSharedEntries()
{
   // We can't merge another shared entry into a file.
}

// Directory* SharedFile::createSubDirs(const QStringList& names, bool physically)
// {
//    return this->file->createSubDirs(names, physically);
// }

Entry* SharedFile::getRootEntry() const
{
   return this->file;
}

Common::Path SharedFile::getPath() const
{
   return this->path.setFilename(this->file->getName());
}

File* SharedFile::getRootFile() const
{
   return this->file;
}