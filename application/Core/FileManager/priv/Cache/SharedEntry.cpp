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
SharedEntry::SharedEntry(Cache* cache, const Common::Path& path, const Common::Hash& id) :
   cache(cache), path(pathWithoutEntryName(path)), id(id.isNull() ? Common::Hash::rand() : id), userName(entryName(path))
{
   const QString& pathStr = path.getPath();

   // Avoid two same directories.
   if (this->cache->isShared(pathStr))
      throw SharedEntryAlreadySharedException();

   // First of all check is the directory physically exists.
   if (path.isFile() && !QFile(pathStr).exists())
      throw FileNotFoundException(pathStr);

   if (!path.isFile() && !QDir(pathStr).exists())
      throw DirNotFoundException(pathStr);

   if (SharedDirectory* dir = this->cache->getSuperSharedEntry(pathStr))
      throw SuperDirectoryExistsException(dir->getFullPath(), pathStr);
}

/**
  * A factory to create a shared entry (file or directory) depending of the given path.
  */
SharedEntry* SharedEntry::create(Cache* cache, const QString& pathStr, const Common::Hash& id = Common::Hash())
{
   Common::Path path(pathStr);
   if (path.isFile())
      return new SharedFile(cache, path, id);
   else
      return new SharedDirectory(cache, path, id);
}

SharedEntry::~SharedEntry()
{
   L_DEBU(QString("SharedEntry deleted: %1").arg(this->getUserName()));
}

void SharedEntry::del(bool invokeDelete)
{
   // The question is: why we don't let 'Directory::del()' destroys its sub directories?
   // This is because a concurrent access to 'Directory::getRoot()' during a delete of a shared directory must be
   // able to access the shared director.
   // this->deleteSubDirs();

   this->getRootEntry()->del(invokeDelete);
}

void SharedEntry::moveInto(Directory* directory)
{
   // A directory can't be move in its own tree.
   if (this->getRootEntry()->getRoot() == this)
      return;

   this->getCache()->removeSharedEntry(this, directory->createSubDir(this->getRootEntry()->getName()));
}

void SharedEntry::moveInto(const QString& path)
{
   this->path = Common::Path(path);
}
Cache* SharedEntry::getCache() const
{
   return this->cache;
}

Common::Path SharedEntry::getPath() const
{
   return this->path;
}

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
QString SharedEntry::entryName(const Common::Path& path)
{
   if (path.isFile())
      return path.filename;

   if (path.getDirs().isEmpty())
      return path.getRoot();
   else
      return path.getDirs().last();
}

Common::Path SharedEntry::pathWithoutEntryName(const Common::Path& path)
{
   if (path.isFile())
      return path.removeFilename();
   else
      return path.removeLastDir();
}

/////

SharedDirectory::SharedDirectory(Cache* cache, const QString& path, const Common::Hash& id) :
   SharedEntry(cache, path, id)
{

}

void SharedDirectory::mergeSubSharedEntries()
{
   // Merges the sub-directories of each directory found.
   foreach (SharedEntry* subEntry, this->cache->getSubSharedEntrys(this->getFullPath()))
   {
      // Create the missing directories.
      const QStringList& parentFolders = this->getFullPath().getDirs();
      const QStringList& childFolders = subEntry->getFullPath().getDirs();
      Directory* current = this;
      for (int i = parentFolders.size(); i < childFolders.size(); i++)
         current = current->createSubDir(childFolders[i]);

      this->getCache()->removeSharedDir(subDir, current);
   }
}

Entry* SharedDirectory::getRootEntry() const
{
   return this->directory;
}

Common::Path SharedDirectory::getFullPath() const
{
   return this->path.appendDir(this->directory->getName());
}

/////

SharedFile::SharedFile(Cache* cache, const QString& path, const Common::Hash& id) :
   SharedEntry(cache, path, id)
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

Common::Path SharedFile::getFullPath() const
{
   return this->path.setFilename(this->file->getName());
}
