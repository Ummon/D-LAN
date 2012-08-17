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
  
#include <priv/Cache/Directory.h>
using namespace FM;

#include <QDir>

#include <Common/ProtoHelper.h>
#include <Common/Global.h>

#include <priv/Global.h>
#include <priv/Constants.h>
#include <priv/Log.h>
#include <priv/FileManager.h>
#include <priv/Cache/File.h>
#include <priv/Cache/SharedDirectory.h>

/**
  * @exception UnableToCreateNewDirException (may be thrown only if 'createPhysically' is true).
  */
Directory::Directory(Directory* parent, const QString& name, bool createPhysically) :
   Entry(parent->cache, name),
   parent(parent),
   subDirs(&Directory::entrySortingFun),
   files(&Directory::entrySortingFun),
   scanned(true),
   mutex(QMutex::Recursive)
{
   QMutexLocker locker(&this->mutex);
   L_DEBU(QString("New Directory : %1, createPhysically = %2").arg(this->getFullPath()).arg(createPhysically));

   if (createPhysically)
      if (!QDir(this->parent->getFullPath()).mkdir(this->name))
      {
         L_ERRO(QString("Unable to create the directory : %1").arg(this->getFullPath()));
         throw UnableToCreateNewDirException();
      }

   this->parent->add(this);
}

/**
  * Called by the root (SharedDirectory) which will not have parent and name.
  */
Directory::Directory(Cache* cache, const QString& name) :
   Entry(cache, name), parent(0), subDirs(&Directory::entrySortingFun), files(&Directory::entrySortingFun), mutex(QMutex::Recursive)
{
}

Directory::~Directory()
{   
   this->deleteSubDirs();

   QMutexLocker locker(&this->mutex);

   foreach (File* f, this->files.getList())
      delete f;

   if (this->parent)
      this->parent->subDirDeleted(this);

   L_DEBU(QString("Directory deleted : %1").arg(this->getFullPath()));
}

/**
  * Retore the hashes from the cache.
  * All file which are not complete and not in the cache are physically removed.
  * Only files ending with the setting "unfinished_suffix_term" will be removed.
  * @return The files which have all theirs hashes (complete).
  */
QList<File*> Directory::restoreFromFileCache(const Protos::FileCache::Hashes::Dir& dir)
{
   QMutexLocker locker(&this->mutex);

   QList<File*> ret;

   if (Common::ProtoHelper::getStr(dir, &Protos::FileCache::Hashes_Dir::name) == this->getName())
   {
      // Sub directories . . .
      for (int i = 0; i < dir.dir_size(); i++)
         for (QLinkedListIterator<Directory*> d(this->subDirs.getList()); d.hasNext();)
            ret << d.next()->restoreFromFileCache(dir.dir(i));

      // . . . And files.
      QLinkedList<File*> filesNotInDir = this->files.getList();
      for (int i = 0; i < dir.file_size(); i++)
         for (QLinkedListIterator<File*> j(this->files.getList()); j.hasNext();)
         {
            File* f = j.next();
            if (f->restoreFromFileCache(dir.file(i)) && f->hasAllHashes())
            {
               filesNotInDir.removeOne(f);
               ret << f;
            }
         }

      // Remove unfinished files not in 'dir'.
      for (QLinkedListIterator<File*> i(filesNotInDir); i.hasNext();)
      {
         File* file = i.next();
         if (!file->isComplete())
         {
            file->removeUnfinishedFiles();
            delete file;
         }
      }
   }

   return ret;
}

void Directory::populateHashesDir(Protos::FileCache::Hashes::Dir& dirToFill) const
{
   QLinkedList<Directory*> subDirsCopy;
   QLinkedList<File*> filesCopy;

   {
      QMutexLocker locker(&this->mutex);
      Common::ProtoHelper::setStr(dirToFill, &Protos::FileCache::Hashes_Dir::set_name, this->getName());
      subDirsCopy = this->subDirs.getList();
      filesCopy = this->files.getList();
   }

   for (QLinkedListIterator<File*> i(filesCopy); i.hasNext();)
   {
      File* f = i.next();

      if (f->hasOneOrMoreHashes())
      {
         Protos::FileCache::Hashes_File* file = dirToFill.add_file();
         f->populateHashesFile(*file);
      }
   }

   for (QLinkedListIterator<Directory*> dir(subDirsCopy); dir.hasNext();)
   {
      dir.next()->populateHashesDir(*dirToFill.add_dir());
   }
}

void Directory::populateEntry(Protos::Common::Entry* dir, bool setSharedDir) const
{
   QMutexLocker locker(&this->mutex);

   Entry::populateEntry(dir, setSharedDir);

   // Do not count the unfinished files.
   bool isEmpty = true;
   for (QLinkedListIterator<File*> i(this->files.getList()); i.hasNext();)
      if (i.next()->isComplete())
      {
         isEmpty = false;
         break;
      }

   dir->set_is_empty(this->subDirs.getList().isEmpty() && isEmpty);
   dir->set_type(Protos::Common::Entry_Type_DIR);
}

/**
  * Remove physically all unfinished file.
  */
void Directory::removeUnfinishedFiles()
{
   QMutexLocker locker(&this->mutex);

   // Removes incomplete file we don't know.
   foreach (File* f, this->files.getList())
      f->removeUnfinishedFiles();

   foreach (Directory* d, this->subDirs.getList())
      d->removeUnfinishedFiles();
}

void Directory::moveInto(Directory* directory)
{
   QMutexLocker locker(&this->mutex);

   if (directory == this->parent)
      return;

   // A directory can't be move in its own tree.
   Directory* parentDestination = directory;
   do
   {
      if (parentDestination == this)
         return;
   } while (parentDestination = parentDestination->parent);

   this->parent->subDirDeleted(this);
   directory->add(this);
   this->parent = directory;
}

/**
  * Called from one of its file.
  */
void Directory::fileDeleted(File* file)
{
   L_DEBU(QString("Directory::fileDeleted() remove %1").arg(file->getFullPath()));

   (*this) -= file->getSize();
   this->files.removeOne(file);
}

void Directory::subDirDeleted(Directory* dir)
{
   QMutexLocker locker(&this->mutex);
   this->subDirs.removeOne(dir);
}

QString Directory::getPath() const
{
   QString path('/');

   const Directory* dir = this;
   while (dir->parent && dir->parent->parent) // We don't care about the name of the root (SharedDirectory).
   {
      dir = dir->parent;
      path.prepend(dir->getName());
      path.prepend('/');
   }
   return path;
}

/**
  * We use "this->name" instead of "this->getName()" to improve a bit the performance during searching (See 'QSort(..)' in 'FileManager::find(..)').
  */
QString Directory::getFullPath() const
{
   // In case of a partially constructed ShareDirectory.
   // (When a exception is thrown from the SharedDirectory ctor).
   if (!this->parent)
      return this->getName().append('/');

   return this->parent->getFullPath().append(this->name).append('/');
}

SharedDirectory* Directory::getRoot() const
{
   QMutexLocker locker(&this->mutex);
   return this->parent->getRoot(); // A directory MUST have a parent.
}

void Directory::rename(const QString& newName)
{
   QMutexLocker locker(&this->mutex);
   Entry::rename(newName);
   if (this->parent)
      this->parent->subdirNameChanged(this);
}

bool Directory::isAChildOf(const Directory* dir) const
{
   if (this->parent)
   {
      if (this->parent == dir)
         return true;
      else
         return this->parent->isAChildOf(dir);
   }
   return false;
}

/**
  * @return Returns 0 if no one match.
  */
Directory* Directory::getSubDir(const QString& name) const
{
   QMutexLocker locker(&this->mutex);

   for (QLinkedListIterator<Directory*> i(this->subDirs.getList()); i.hasNext();)
   {
      Directory* d = i.next();
      if (d->getName() == name)
         return d;
   }

   return 0;
}

QLinkedList<Directory*> Directory::getSubDirs() const
{
   QMutexLocker locker(&this->mutex);
   return this->subDirs.getList();
}

QLinkedList<File*> Directory::getFiles() const
{
   QMutexLocker locker(&this->mutex);
   return this->files.getList();
}

QList<File*> Directory::getCompleteFiles() const
{
   QMutexLocker locker(&this->mutex);
   QList<File*> completeFiles;
   foreach (File* file, this->files.getList())
   {
      if (file->isComplete())
         completeFiles << file;
   }
   return completeFiles;
}

/**
  * Creates a new sub-directory if none exists already otherwise
  * returns an already existing.
  * @exception UnableToCreateNewDirException
  */
Directory* Directory::createSubDir(const QString& name, bool physically)
{
   QMutexLocker locker(&this->mutex);
   if (Directory* subDir = this->getSubDir(name))
      return subDir;
   return new Directory(this, name, physically);
}

/**
  * Create the all sub-directories, sub-dirs may already exist.
  * @return the last directory.
  * @exception UnableToCreateNewDirException
  */
Directory* Directory::createSubDirs(const QStringList& names, bool physically)
{
   Directory* currentDir = this;
   foreach (QString name, names)
   {
      currentDir = currentDir->createSubDir(name, physically);
      if (!currentDir)
         return 0;
   }
   return currentDir;
}

File* Directory::getFile(const QString& name) const
{
   QMutexLocker locker(&this->mutex);
   foreach (File* f, this->files.getList())
      if (f->getName() == name)
         return f;

   return 0;
}

/**
  * Only called by the class File.
  */
void Directory::add(File* file)
{
   QMutexLocker locker(&this->mutex);
   this->files.insert(file);
   (*this) += file->getSize();
}

void Directory::fileSizeChanged(qint64 oldSize, qint64 newSize)
{
   QMutexLocker locker(&this->mutex);
   (*this) += newSize - oldSize;
}

/**
  * Steal the sub directories and files from 'dir'.
  * The sub dirs and files will be removed from 'dir'.
  */
void Directory::stealContent(Directory* dir)
{
   QMutexLocker locker(&this->mutex);
   if (dir == this)
   {
      L_ERRO("Directory::stealSubDirs(..) : dir == this");
      return;
   }

   // L_DEBU(QString("this = %1, dir = %2").arg(this->getFullPath()).arg(dir->getFullPath()));

   this->subDirs.insert(dir->subDirs.getList());
   this->files.insert(dir->files.getList());

   foreach (Directory* d, dir->subDirs.getList())
   {
      d->parent = this;
      (*this) += d->getSize();
      (*dir) -= d->getSize();
   }

   foreach (File* f, dir->files.getList())
      f->changeDirectory(this);

   dir->subDirs.clear();
   dir->files.clear();
}

void Directory::add(Directory* dir)
{
   QMutexLocker locker(&this->mutex);
   this->subDirs.insert(dir);
}

bool Directory::isScanned() const
{
   QMutexLocker locker(&this->mutex);
   return this->scanned;
}

void Directory::setScanned(bool value)
{
   QMutexLocker locker(&this->mutex);

   if (value == this->scanned)
      return;

   this->scanned = value;
   if (this->scanned)
      this->cache->onScanned(this);
}

/**
  * Must be called only by a file.
  */
void Directory::fileNameChanged(File* file)
{
   QMutexLocker locker(&this->mutex);
   this->files.itemChanged(file);
}

void Directory::deleteSubDirs()
{
   foreach (Directory* d, this->subDirs.getList())
      delete d;
}

void Directory::subdirNameChanged(Directory* dir)
{
   QMutexLocker locker(&this->mutex);
   this->subDirs.itemChanged(dir);
}

/**
  * When a new file is added to a directory this method is called
  * to add its size.
  */
Directory& Directory::operator+=(qint64 size)
{
   QMutexLocker locker(&this->mutex);
   this->size += size;
   if (this->parent)
      (*this->parent) += size;

   return *this;
}

Directory& Directory::operator-=(qint64 size)
{
   QMutexLocker locker(&this->mutex);
   this->size -= size;
   if (this->parent)
      (*this->parent) -= size;

   return *this;
}

/**
  * @class FM::DirIterator
  *
  * Iterate recursively over a directory tree structure.
  * @param includeRoot If true then include the given dir into the iterations.
  */

DirIterator::DirIterator(Directory* dir, bool includeRoot)
{
   if (includeRoot)
      this->dirsToVisit << dir;
   else
      this->dirsToVisit = dir->subDirs.getList();
}

/**
  * Return the next directory, 0 if there is no more directory.
  */
Directory* DirIterator::next()
{
   if (this->dirsToVisit.isEmpty())
      return 0;

   Directory* dir = this->dirsToVisit.front();
   this->dirsToVisit.removeFirst();
   this->dirsToVisit << dir->subDirs.getList();
   return dir;
}
