#include <priv/Cache/Directory.h>
using namespace FM;

#include <QDir>

#include <Common/ProtoHelper.h>

#include <priv/Constants.h>
#include <priv/Log.h>
#include <priv/FileManager.h>
#include <priv/Cache/File.h>
#include <priv/Cache/SharedDirectory.h>

Directory::Directory(Directory* parent, const QString& name, bool createPhysically)
   : Entry(parent->cache, name), parent(parent)
{
   if (createPhysically)
      QDir(this->parent->getFullPath()).mkdir(this->name);

   this->parent->subDirs.append(this);
}

/**
  * Called by the root (SharedDirectory) which will not have parent and name.
  */
Directory::Directory(Cache* cache, const QString& name)
   : Entry(cache, name), parent(0)
{
}

Directory::~Directory()
{
   // QMutexLocker lock(&this->cache->getMutex()); // TODO : it creates a deadlock

   foreach (File* f, this->files)
      delete f;
   foreach (Directory* d, this->subDirs)
      delete d;

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
QList<File*> Directory::restoreFromFileCache(const Protos::FileCache::Hashes_Dir& dir)
{
   QList<File*> ret;

   if (Common::ProtoHelper::getStr(dir, &Protos::FileCache::Hashes_Dir::name) == this->getName())
   {
      // Sub directories..
      for (int i = 0; i < dir.dir_size(); i++)
         for (QListIterator<Directory*> d(this->subDirs); d.hasNext();)
            ret << d.next()->restoreFromFileCache(dir.dir(i));

      // .. And files.
      QList<File*> filesExistPhysically;
      for (int i = 0; i < dir.file_size(); i++)
         for (QListIterator<File*> j(this->files); j.hasNext();)
         {
            File* f = j.next();
            if (f->restoreFromFileCache(dir.file(i)))
            {
               filesExistPhysically << f;
               if (f->hasAllHashes())
                  ret << f;
            }
         }

      // Removes incomplete file we don't know.
      foreach (File* f, this->files)
         if (!filesExistPhysically.contains(f) && !f->isComplete())
         {
            f->physicallyRemoveUnfinished();
            delete f;
         }

   }

   return ret;
}

void Directory::populateHashesDir(Protos::FileCache::Hashes_Dir& dirToFill) const
{
   Common::ProtoHelper::setStr(dirToFill, &Protos::FileCache::Hashes_Dir::set_name, this->getName());

   for (QListIterator<File*> i(this->files); i.hasNext();)
   {
      File* f = i.next();

      if (f->hasOneOrMoreHashes())
      {
         Protos::FileCache::Hashes_File* file = dirToFill.add_file();
         f->populateHashesFile(*file);
      }
   }

   for (QListIterator<Directory*> dir(this->subDirs); dir.hasNext();)
   {
      dir.next()->populateHashesDir(*dirToFill.add_dir());
   }
}

void Directory::populateEntry(Protos::Common::Entry* dir) const
{
   Entry::populateEntry(dir);
   dir->set_type(Protos::Common::Entry_Type_DIR);
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

QString Directory::getFullPath() const
{
   // In case of a partially constructed ShareDirectory.
   // (When a exception is thrown from the SharedDirectory ctor).
   if (!this->parent)
      return this->getName();

   return this->parent->getFullPath().append('/').append(this->getName());
}

Directory* Directory::getRoot() const
{
   if (this->parent)
      return this->parent->getRoot();
   return const_cast<Directory*>(this);
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
   foreach (Directory* d, this->subDirs)
      if (d->getName() == name)
         return d;

   return 0;
}

QList<Directory*> Directory::getSubDirs() const
{
   // TODO : it create a deadlock, rethink serously about the concurency problems ..
   // - main thread (MT) : setSharedDirsReadOnly(..) with a super shared directory -> Cache::lock
   // - FileUpdater thread (FT) : Scan some directories and be locked by the call currentDir->getSubDirs() -> Cache::lock;
   // - (MT) : SharedDirectory::init() call this->getCache()->removeSharedDir(subDir, current); and emit sharedDirectoryRemoved
   //          which will call FileUpdater::rmRoot which will try to stop scanning -> deadlock
   // QMutexLocker locker(&this->cache->getMutex());
   return this->subDirs;
}

QList<File*> Directory::getFiles() const
{
   // TODO : it create a deadlock, rethink serously about the concurency problems ..
   // Same problem as above.
   // QMutexLocker locker(&this->cache->getMutex());
   return this->files;
}

QList<File*> Directory::getCompleteFiles() const
{
   QList<File*> completeFiles;
   foreach (File* file, this->files)
   {
      if (file->isComplete())
         completeFiles << file;
   }
   return completeFiles;
}

/**
  * Creates a new sub-directory if none exists already otherwise
  * returns an already existing.
  */
Directory* Directory::createSubDirectory(const QString& name)
{
   if (Directory* subDir = this->getSubDir(name))
      return subDir;
   return new Directory(this, name);
}

/**
  * Creates a new sub-directory if none exists already otherwise
  * returns an already existing.
  */
Directory* Directory::physicallyCreateSubDirectory(const QString& name)
{
   if (Directory* subDir = this->getSubDir(name))
      return subDir;

   return new Directory(this, name, true);
}

/**
  * Creates a new file if none exists already otherwise
  * checks if the size and the modification date match, if not then delete the
  * file and create a new one.
  */
//File* Directory::createFile(const QFileInfo& fileInfo, File** oldFile)
//{
//   *oldFile = 0;
//   foreach (File* f, this->files)
//   {
//      if (f->getName() == fileInfo.fileName())
//      {
//         // If the file is uncompleted its size and date may change.
//         if (!f->isComplete() || f->correspondTo(fileInfo))
//            return f;

//         *oldFile = f;
//         delete f;
//         break;
//      }
//   }

//   return new File(this, fileInfo.fileName(), fileInfo.size(), fileInfo.lastModified());
//}

File* Directory::getFile(const QString& name) const
{
   foreach (File* f, this->files)
      if (f->getName() == name)
         return f;

   return 0;
}

/**
  * Only called by the class File.
  */
void Directory::addFile(File* file)
{
   if (this->files.contains(file))
      return;

   this->files << file;

   (*this) += file->getSize();
}

void Directory::fileSizeChanged(qint64 oldSize, qint64 newSize)
{
   (*this) += newSize - oldSize;
}

/**
  * Steal the sub directories and files from 'dir'.
  * The sub dirs and files will be removed from 'dir'.
  */
void Directory::stealContent(Directory* dir)
{
   if (dir == this)
   {
      L_ERRO("Directory::stealSubDirs(..) : dir == this");
      return;
   }

   // L_DEBU(QString("this = %1, dir = %2").arg(this->getFullPath()).arg(dir->getFullPath()));

   this->subDirs.append(dir->subDirs);
   this->files.append(dir->files);

   foreach (Directory* d, dir->subDirs)
   {
      d->parent = this;
      (*this) += d->getSize();
      (*dir) -= d->getSize();
   }

   foreach (File* f, dir->files)
      f->changeDirectory(this);

   dir->subDirs.clear();
   dir->files.clear();
}

/**
  * When a new file is added to a directory this method is called
  * to add its size.
  */
Directory& Directory::operator+=(qint64 size)
{
   this->size += size;
   if (this->parent)
      (*this->parent) += size;

   return *this;
}

Directory& Directory::operator-=(qint64 size)
{
   this->size -= size;
   if (this->parent)
      (*this->parent) -= size;

   return *this;
}

/**
  * @class DirIterator
  * Iterate recursively over a directory tree structure.
  */

DirIterator::DirIterator(Directory* dir) :
   dirsToVisit(dir->subDirs)
{
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
   this->dirsToVisit.append(dir->subDirs);
   return dir;
}

