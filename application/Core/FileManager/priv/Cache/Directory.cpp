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
#include <priv/Cache/SharedEntry.h>

/**
  * @exception UnableToCreateNewDirException (may be thrown only if 'createPhysically' is true).
  */
Directory::Directory(
   SharedEntry* root,
   const QString& name,
   Directory* parentDirectory,
   bool createPhysically
) :
   Entry(root, name, parentDirectory),
   subDirs(&Directory::entrySortingFun),
   files(&Directory::entrySortingFun),
   scanned(true)
{
   QMutexLocker locker(&this->mutex);
   L_DEBU(
      QString("New Directory: %1, createPhysically = %2")
         .arg(this->Directory::getAbsolutePath())
         .arg(createPhysically)
   );

   if (createPhysically)
      if (!QDir(this->Directory::getAbsolutePath().removeLastDir()).mkdir(this->name))
      {
         L_ERRO(QString("Unable to create the directory: %1").arg(this->Directory::getAbsolutePath()));
         Entry::del(false);
         throw UnableToCreateNewDirException();
      }

   if (this->parentDirectory)
      this->parentDirectory->add(this);
}

Directory::~Directory()
{
   this->deleteSubDirs();

   QMutexLocker locker(&this->mutex);

   foreach (File* f, this->files.getList())
      delete f;

   if (this->parentDirectory)
      this->parentDirectory->subDirDeleted(this);

   L_DEBU(QString("Directory deleted: %1").arg(this->getName()));
}

void Directory::del(bool invokeDelete)
{
   {
      QMutexLocker locker(&this->mutex);

      this->deleteSubDirs();

      foreach (File* f, this->files.getList())
         f->del();

      if (this->parentDirectory)
         this->parentDirectory->subDirDeleted(this);
   }

   Entry::del(invokeDelete);
}

/**
  * Restore the hashes from the cache.
  * All file which are not complete and not in the cache are physically removed.
  * Only files ending with the setting "unfinished_suffix_term" will be removed.
  * @return The files which have all theirs hashes (complete).
  */
// QList<File*> Directory::restoreFromFileCache(const Protos::FileCache::Hashes::Dir& dir)
// {
//    QMutexLocker locker(&this->mutex);

//    QList<File*> ret;

//    if (Common::ProtoHelper::getStr(dir, &Protos::FileCache::Hashes_Dir::name) == this->getName())
//    {
//       // Sub directories . . .
//       for (int i = 0; i < dir.dir_size(); i++)
//          for (QListIterator<Directory*> d(this->subDirs.getList()); d.hasNext();)
//             ret << d.next()->restoreFromFileCache(dir.dir(i));

//       // . . . And files.
//       QList<File*> filesNotInDir = this->files.getList();
//       for (int i = 0; i < dir.file_size(); i++)
//          for (QListIterator<File*> j(this->files.getList()); j.hasNext();)
//          {
//             File* f = j.next();
//             if (f->restoreFromFileCache(dir.file(i)) && f->hasAllHashes())
//             {
//                filesNotInDir.removeOne(f);
//                ret << f;
//             }
//          }

//       // Remove unfinished files not in 'dir'.
//       for (QListIterator<File*> i(filesNotInDir); i.hasNext();)
//       {
//          File* file = i.next();
//          if (!file->isComplete())
//          {
//             file->removeUnfinishedFiles();
//             file->del();
//          }
//       }
//    }

//    return ret;
// }

// void Directory::populateHashesDir(Protos::FileCache::Hashes::Dir& dirToFill) const
// {
//    QList<Directory*> subDirsCopy;
//    QList<File*> filesCopy;

//    {
//       QMutexLocker locker(&this->mutex);
//       dirToFill.set_name(this->getName().toStdString());
//       subDirsCopy = this->subDirs.getList();
//       filesCopy = this->files.getList();
//    }

//    for (QListIterator<File*> i(filesCopy); i.hasNext();)
//    {
//       File* f = i.next();

//       if (f->hasOneOrMoreHashes())
//       {
//          Protos::FileCache::Hashes_File* file = dirToFill.add_file();
//          f->populateHashesFile(*file);
//       }
//    }

//    for (QListIterator<Directory*> dir(subDirsCopy); dir.hasNext();)
//    {
//       dir.next()->populateHashesDir(*dirToFill.add_dir());
//    }
// }

void Directory::populateEntry(Protos::Common::Entry* dir, bool setSharedDir) const
{
   QMutexLocker locker(&this->mutex);

   Entry::populateEntry(dir, setSharedDir);

   // Do not count the unfinished files.
   bool noFiles = true;
   for (QListIterator<File*> i(this->files.getList()); i.hasNext();)
      if (i.next()->isComplete())
      {
         noFiles = false;
         break;
      }

   dir->set_is_empty(this->subDirs.getList().isEmpty() && noFiles);
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

   if (directory == this->parentDirectory)
      return;

   // A directory can't be move in its own tree.
   Directory* parentDestination = directory;
   do
   {
      if (parentDestination == this)
         return;
   } while (parentDestination = parentDestination->parentDirectory);

   if (this->parentDirectory)
      this->parentDirectory->subDirDeleted(this);

   directory->add(this);
   this->parentDirectory = directory;
}

/**
  * Called from one of its file.
  */
void Directory::fileDeleted(File* file)
{
   L_DEBU(QString("Directory::fileDeleted() remove %1").arg(file->getAbsolutePath()));

   (*this) -= file->getSize();
   this->files.removeOne(file);
}

void Directory::subDirDeleted(Directory* dir)
{
   QMutexLocker locker(&this->mutex);
   this->subDirs.removeOne(dir);
}

Common::Path Directory::getRelativePath() const
{
   if (!this->parentDirectory)
      return Common::Path();
   else
      return this->parentDirectory->getRelativePath().appendDir(this->name);
}

/**
  * TODO: benchmark the use of Common::Path instead of QString during searching
  *   (See 'QSort(..)' in 'FileManager::find(..)').
  */
Common::Path Directory::getAbsolutePath() const
{
   if (!this->parentDirectory)
      return this->getRoot()->path.appendDir(this->name);
   else
      return this->parentDirectory->getAbsolutePath().appendDir(this->name);
}

Entry* Directory::getEntry(const Common::Path& path)
{
   QMutexLocker locker(&this->mutex);

   Directory* currentDirectory = this;
   for (QStringListIterator i(path.getDirs()); i.hasNext();)
   {
      currentDirectory = currentDirectory->getSubDir(i.next());
      if (!currentDirectory)
         break;
   }

   if (currentDirectory && path.isFile())
      return currentDirectory->getFile(path.getFilename());

   return currentDirectory;
}

void Directory::rename(const QString& newName)
{
   QMutexLocker locker(&this->mutex);
   Entry::rename(newName);
   if (this->parentDirectory)
      this->parentDirectory->subdirNameChanged(this);
}

bool Directory::isAChildOf(const Directory* dir) const
{
   if (this->parentDirectory)
   {
      if (this->parentDirectory == dir)
         return true;
      else
         return this->parentDirectory->isAChildOf(dir);
   }
   return false;
}

/**
  * @return Returns 0 if no one match.
  */
Directory* Directory::getSubDir(const QString& name) const
{
   QMutexLocker locker(&this->mutex);

   for (QListIterator<Directory*> i(this->subDirs.getList()); i.hasNext();)
   {
      Directory* d = i.next();
      if (d->getName() == name)
         return d;
   }

   return nullptr;
}

QList<Directory*> Directory::getSubDirs() const
{
   QMutexLocker locker(&this->mutex);
   return this->subDirs.getList();
}

QList<File*> Directory::getFiles() const
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
   return new Directory(this->getRoot(), name, this, physically);
}

/**
  * Create all sub-directories, sub-dirs may already exist.
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
         return nullptr;
   }
   return currentDir;
}

File* Directory::getFile(const QString& name) const
{
   QMutexLocker locker(&this->mutex);
   foreach (File* f, this->files.getList())
      if (f->getName() == name)
         return f;

   return nullptr;
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
      L_ERRO("Directory::stealSubDirs(..): dir == this");
      return;
   }

   // L_DEBU(QString("this = %1, dir = %2").arg(this->getFullPath()).arg(dir->getFullPath()));

   const QList<Directory*> directoriesToSteal = dir->subDirs.getList();
   const QList<File*> filesToSteal = dir->files.getList();

   this->subDirs.insert(directoriesToSteal);
   this->files.insert(filesToSteal);

   foreach (Directory* d, directoriesToSteal)
   {
      d->setParentDirectory(this);
      (*this) += d->getSize();
      (*dir) -= d->getSize();
   }

   foreach (File* f, filesToSteal)
   {
      f->setParentDirectory(this);
      (*this) += f->getSize();
      (*dir) -= f->getSize();
   }

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
      this->getCache()->onScanned(this);
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
      d->del();
}

void Directory::setRootRecursively(SharedEntry* sharedEntry)
{
   QMutexLocker locker(&this->mutex);

   if (this->root != sharedEntry)
   {
      this->root = sharedEntry;

      for (auto file : this->files.getList())
         file->setRootRecursively(sharedEntry);

      for (auto dir : this->subDirs.getList())
         dir->setRootRecursively(sharedEntry);
   }
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

   this->setSize(this->getSize() + size);

   if (this->parentDirectory)
      (*this->parentDirectory) += size;

   return *this;
}

Directory& Directory::operator-=(qint64 size)
{
   QMutexLocker locker(&this->mutex);

   this->setSize(this->getSize() - size);

   if (this->parentDirectory)
      (*this->parentDirectory) -= size;

   return *this;
}

/////

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
      return nullptr;

   Directory* dir = this->dirsToVisit.front();
   this->dirsToVisit.removeFirst();
   this->dirsToVisit << dir->subDirs.getList();
   return dir;
}
