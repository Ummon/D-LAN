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
  
#include <priv/Cache/SharedDirectory.h>
using namespace FM;

#include <QDir>

#include <Common/ProtoHelper.h>
#include <Common/Global.h>

#include <Exceptions.h>
#include <priv/Log.h>
#include <priv/Exceptions.h>
#include <priv/Cache/Cache.h>

/**
  * Create from a saved shared directory (file cache).
  * If a existing shared directory is a sub directory then it will be merged.
  * @exception SuperDirectoryExistsException Thrown when a super shared directory already exists.
  * @exception DirAlreadySharedException
  * @exception DirNotFoundException
  */
SharedDirectory::SharedDirectory(Cache* cache, const QString& path) :
   Directory(cache, dirName(path)), path(this->pathWithoutDirName(path)), id(Common::Hash::rand())
{
   this->init();
}

/**
  * @exception SuperDirectoryExistsException Thrown when a super shared directory already exists.
  * @exception DirAlreadySharedException
  * @exception DirNotFoundException
  */
SharedDirectory::SharedDirectory(Cache* cache, const QString& path, const Common::Hash& id) :
   Directory(cache, dirName(path)), path(this->pathWithoutDirName(path)), id(id)
{
   this->init();
}

/**
  * Try to merge other shared directory with this one.
  * For example /sharing/folder1 can be merged with /sharing.
  * Should be called after each new SharedDirectory created.
  */
void SharedDirectory::mergeSubSharedDirectories()
{
   // Merges the sub-directories of each directory found.
   foreach (SharedDirectory* subDir, this->cache->getSubSharedDirectories(this->getFullPath()))
   {
      // Create the missing directories.
      const QStringList& parentFolders = this->getFullPath().split('/', QString::SkipEmptyParts);
      const QStringList& childFolders = subDir->getFullPath().split('/', QString::SkipEmptyParts);
      Directory* current = this;
      for (int i = parentFolders.size(); i < childFolders.size(); i++)
         current = current->createSubDir(childFolders[i]);

      this->getCache()->removeSharedDir(subDir, current);
   }
}

void SharedDirectory::populateEntry(Protos::Common::Entry* entry, bool setSharedDir) const
{
   // The 'shared_dir' field is always filled for a 'SharedDirectory', it doesn't depend of 'setSharedDir'.
   Directory::populateEntry(entry, true);
   entry->set_path(""); // The path of a shared directory is private (we don't want the other peers to see absolute paths).
}

void SharedDirectory::init()
{
   // Avoid two same directories.
   if (this->cache->isShared(this->getFullPath()))
   {
      Directory::del(false);
      throw DirAlreadySharedException();
   }

   // First of all check is the directory physically exists.
   if (!QDir(this->getFullPath()).exists())
   {
      Directory::del(false);
      throw DirNotFoundException(this->getFullPath());
   }

   if (SharedDirectory* dir = this->cache->getSuperSharedDirectory(this->getFullPath()))
   {
      Directory::del(false);
      throw SuperDirectoryExistsException(dir->getFullPath(), this->getFullPath());
   }
}

SharedDirectory::~SharedDirectory()
{
   L_DEBU(QString("SharedDirectory deleted: %1").arg(this->getName()));
}

void SharedDirectory::del(bool invokeDelete)
{
   // The question is: why we don't let 'Directory::del()' destroys its sub directories?
   // This is because a concurrent access to 'Directory::getRoot()' during a delete of a shared directory must be
   // able to access the shared director.
   // this->deleteSubDirs();

   Directory::del(invokeDelete);
}

void SharedDirectory::moveInto(Directory* directory)
{
   // A directory can't be move in its own tree.
   if (directory->getRoot() == this)
      return;
   this->getCache()->removeSharedDir(this, directory->createSubDir(this->name));
}

void SharedDirectory::moveInto(const QString& path)
{
   this->path = Common::Global::cleanDirPath(path);
}

QString SharedDirectory::getPath() const
{
   return this->path;
}

QString SharedDirectory::getFullPath() const
{
   return this->path + (!this->name.isEmpty() && !Common::Global::isWindowsRootPath(this->name) ? this->name + '/' : "");
}

SharedDirectory* SharedDirectory::getRoot() const
{
   return const_cast<SharedDirectory*>(this);
}

Common::Hash SharedDirectory::getId() const
{
   return this->id;
}

/**
  * Special for Windows root:
  * name of "C:\" is "C:".
  */
QString SharedDirectory::dirName(const QString& path)
{
   if (Common::Global::isWindowsRootPath(path))
      return path.left(2);

   return QDir(path).dirName();
}

QString SharedDirectory::pathWithoutDirName(const QString& path)
{
   if (Common::Global::isWindowsRootPath(path))
      return path;

   const QString& cleanedPath(QDir::cleanPath(path));
   return cleanedPath.left(cleanedPath.size() - this->name.size());
}
