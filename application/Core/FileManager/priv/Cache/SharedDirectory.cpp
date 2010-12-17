/**
  * Aybabtu - A decentralized LAN file sharing software.
  * Copyright (C) 2010-2011 Greg Burri <greg.burri@gmail.com>
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

#include <Exceptions.h>
#include <priv/Log.h>
#include <priv/Exceptions.h>
#include <priv/Cache/Cache.h>


/**
  * Create from a saved shared directory (file cache).
  * If a existing shared directory is a sub directory then it will be merged.
  * @exception SuperDirectoryExistsException Thrown when a super shared directory already exists.
  * @exception SubDirectoriesWithDifferentRightsExistsException Thrown when one or more sub directory already exists with different rights.
  * @exception SuperDirectoryExistsException Thrown when a super directory already exists regardless of the rights.
  */
SharedDirectory::SharedDirectory(Cache* cache, const QString& path, Rights rights)
   : Directory(cache, QDir(path).dirName()), path(QDir::cleanPath(path)), rights(rights), id(Common::Hash::rand())
{
   this->init();
}

SharedDirectory::SharedDirectory(Cache* cache, const QString& path, Rights rights, const Common::Hash& id)
   : Directory(cache, QDir(path).dirName()), path(QDir::cleanPath(path)), rights(rights), id(id)
{
   this->init();
}

void SharedDirectory::populateEntry(Protos::Common::Entry* entry, bool setSharedDir) const
{
   Directory::populateEntry(entry, setSharedDir);
   entry->mutable_shared_dir()->mutable_id()->set_hash(static_cast<SharedDirectory*>(this->getRoot())->getId().getData(), Common::Hash::HASH_SIZE);
}

void SharedDirectory::init()
{
   // Avoid two same directories.
   if (this->cache->isShared(this->path))
      throw DirAlreadySharedException();

   // First of all check is the directory physically exists.
   if (!QDir(this->path).exists())
      throw DirNotFoundException(this->path);

   if (SharedDirectory* dir = this->cache->getSuperSharedDirectory(this->path))
      throw SuperDirectoryExistsException(dir->getFullPath(), this->getFullPath());

   // Gets the sub directories and checks the rights matches.
   QList<SharedDirectory*> subDirs = this->cache->getSubSharedDirectories(this->path);
   foreach (SharedDirectory* subDir, subDirs)
   {
      QStringList subs;
      if (subDir->rights != this->rights)
         subs << subDir->getFullPath();
      if (!subs.isEmpty())
         throw SubDirectoriesWithDifferentRightsExistsException(this->path, subs);
   }

   // Merges the sub-directories of each directory found.
   foreach (SharedDirectory* subDir, subDirs)
   {
      // Create the missing directories.
      const QStringList& parentFolders = this->getFullPath().split('/', QString::SkipEmptyParts);
      const QStringList& childFolders = subDir->getFullPath().split('/', QString::SkipEmptyParts);
      Directory* current = this;
      for (int i = parentFolders.size(); i < childFolders.size(); i++)
         current = new Directory(current, childFolders[i]);

      this->getCache()->removeSharedDir(subDir, current);
   }
}

SharedDirectory::~SharedDirectory()
{
   L_DEBU(QString("SharedDirectory deleted : %1").arg(this->path));
}

QList<File*> SharedDirectory::restoreFromFileCache(const Protos::FileCache::Hashes& hashes)
{
   QList<File*> ret;

   // Give each root to each sub directory. We don't match the full path.
   for (int i = 0; i < hashes.shareddir_size(); i++)
      ret << Directory::restoreFromFileCache(hashes.shareddir(i).root());

   return ret;
}

QString SharedDirectory::getPath() const
{
   return "";
}

QString SharedDirectory::getFullPath() const
{
   return this->path;
}

SharedDirectory::Rights SharedDirectory::getRights() const
{
   return this->rights;
}

Common::Hash SharedDirectory::getId() const
{
   return this->id;
}
