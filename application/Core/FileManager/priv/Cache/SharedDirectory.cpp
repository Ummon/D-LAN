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
   Directory(cache, QDir(path).dirName()), path(this->cleanAndRemoveName(path)), id(Common::Hash::rand())
{
   this->init();
}

/**
  * @exception SuperDirectoryExistsException Thrown when a super shared directory already exists.
  * @exception DirAlreadySharedException
  * @exception DirNotFoundException
  */
SharedDirectory::SharedDirectory(Cache* cache, const QString& path, const Common::Hash& id) :
   Directory(cache, QDir(path).dirName()), path(this->cleanAndRemoveName(path)), id(id)
{
   this->init();
}

/**
  * Try to merge other shared directory with this one.
  * For exemple /sharing/folder1 can be merged with /sharing.
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
   Directory::populateEntry(entry, setSharedDir);
   entry->set_path("");
   if (this->name.isEmpty())
      Common::ProtoHelper::setStr(*entry, &Protos::Common::Entry::set_name, this->path);
   entry->mutable_shared_dir()->mutable_id()->set_hash(static_cast<SharedDirectory*>(this->getRoot())->getId().getData(), Common::Hash::HASH_SIZE);
}

void SharedDirectory::init()
{
   // Avoid two same directories.
   if (this->cache->isShared(this->getFullPath()))
      throw DirAlreadySharedException();

   // First of all check is the directory physically exists.
   if (!QDir(this->getFullPath()).exists())
      throw DirNotFoundException(this->getFullPath());

   if (SharedDirectory* dir = this->cache->getSuperSharedDirectory(this->getFullPath()))
      throw SuperDirectoryExistsException(dir->getFullPath(), this->getFullPath());
}

SharedDirectory::~SharedDirectory()
{
   L_DEBU(QString("SharedDirectory deleted : %1").arg(this->getFullPath()));
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
   return this->path + (!this->name.isEmpty() ? this->name + '/' : "");
}

SharedDirectory* SharedDirectory::getRoot() const
{
   return const_cast<SharedDirectory*>(this);
}

Common::Hash SharedDirectory::getId() const
{
   return this->id;
}

QString SharedDirectory::cleanAndRemoveName(const QString& path)
{
   const QString& cleanedPath(QDir::cleanPath(path));
   return cleanedPath.left(cleanedPath.size() - this->name.size());
}
