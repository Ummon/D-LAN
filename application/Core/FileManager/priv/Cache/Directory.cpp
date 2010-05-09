#include <priv/Cache/Directory.h>
using namespace FM;

#include <QDir>

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

Directory::Directory(Cache* cache)
   : Entry(cache, ""), parent(0)
{
}

Directory::~Directory()
{
   foreach (File* f, this->files)
      delete f;
   foreach (Directory* d, this->subDirs)
      delete d;

   L_DEBU(QString("Directory deleted : %1").arg(this->getFullPath()));
}

QList<File*> Directory::restoreFromFileCache(const Protos::FileCache::Hashes_Dir& dir)
{
   QList<File*> ret;

   if (dir.name().data() == this->getName())
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
   //dirToFill.set_name(this->getName().toStdString());

   for (QListIterator<File*> i(this->files); i.hasNext();)
   {
      File* f = i.next();

      if (f->hasOneOrMoreHashes())
      {
         Protos::FileCache::Hashes_File* file = dirToFill.add_file();
         f->populateHashesFile(*file);
      }
   }

   return;
   for (QListIterator<Directory*> dir(this->subDirs); dir.hasNext();)
   {
      dir.next()->populateHashesDir(*dirToFill.add_dir());
   }
}

void Directory::populateDirEntry(Protos::Common::DirEntry* entry) const
{
   this->populateEntry(entry->mutable_dir());
}

void Directory::fileDeleted(File* file)
{
   L_DEBU(QString("Directory::fileDeleted() remove %1 from %2").arg(file->getFullPath()).arg(this->getFullPath()));
   this->files.removeOne(file);
}

void Directory::subDirDeleted(Directory* dir)
{
   this->subDirs.removeOne(dir);
}

QString Directory::getPath() const
{
   QString path;
   const Directory* dir = this;
   while (dir->parent)
   {
      dir = dir->parent;
      if (dir->parent)
         path.prepend('/');
      path.prepend(dir->getName());
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

Directory* Directory::getSubDir(const QString& name) const
{
   foreach (Directory* d, this->subDirs)
      if (d->getName() == name)
         return d;
   return 0;
}

QList<Directory*> Directory::getSubDirs() const
{
   QMutexLocker(&this->cache->getMutex());
   return this->subDirs;
}

QList<File*> Directory::getFiles() const
{
   QMutexLocker(&this->cache->getMutex());
   return this->files;
}

Directory* Directory::createSubDirectory(const QString& name)
{
   if (Directory* subDir = this->getSubDir(name))
      return subDir;
   return new Directory(this, name);
}

Directory* Directory::physicallyCreateSubDirectory(const QString& name)
{
   if (Directory* subDir = this->getSubDir(name))
      return subDir;

   return new Directory(this, name, true);
}

File* Directory::createFile(const QFileInfo& fileInfo)
{
   foreach (File* f, this->files)
   {
      if (f->getName() == fileInfo.fileName())
      {
         // If the file is uncompleted its size and date may change.
         if (!f->isComplete() || f->getSize() == fileInfo.size() && f->getDateLastModified() == fileInfo.lastModified())
            return f;

         delete f;
         break;
      }
   }

   return new File(this, fileInfo.fileName(), fileInfo.size(), fileInfo.lastModified());
}

File* Directory::getFile(const QString& name) const
{
   foreach (File* f, this->files)
      if (f->getName() == name)
         return f;
   return 0;
}

void Directory::addFile(File* file)
{
   if (this->files.contains(file))
      return;

   this->files.append(file);

   (*this) += file->getSize();
}

void Directory::stealContent(Directory* dir)
{
   if (dir == this)
   {
      L_ERRO("Directory::stealSubDirs(..) : dir == this");
      return;
   }

   //LOG_DEBUG(QString("this = %1, dir = %2").arg(this->getFullPath()).arg(dir->getFullPath()));

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

