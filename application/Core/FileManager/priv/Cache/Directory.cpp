#include <priv/Cache/Directory.h>
using namespace FM;

#include <QDir>

#include <priv/Log.h>
#include <priv/FileManager.h>
#include <priv/Cache/File.h>
#include <priv/Cache/SharedDirectory.h>

Directory::Directory(Directory* parent, const QString& name)
   : Entry(parent->cache, name), parent(parent)
{
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

   LOG_DEBUG(QString("Directory deleted : %1").arg(this->getFullPath()));
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

      // .. And files.s
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
   dirToFill.set_name(this->getName().toStdString());

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

void Directory::populateDirEntry(Protos::Common::DirEntry* entry) const
{
   entry->mutable_dir()->set_path(this->getPath().toStdString());
   entry->mutable_dir()->set_name(this->getName().toStdString());
}

void Directory::fileDeleted(File* file)
{
   LOG_DEBUG(QString("Directory::fileDeleted() remove %1 from %2").arg(file->getFullPath()).arg(this->getFullPath()));
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

QList<Directory*> Directory::getSubDirs() const
{
   return this->subDirs;
}

QList<File*> Directory::getFiles() const
{
   return this->files;
}

Directory* Directory::createSubDirectory(const QString& name)
{
   foreach (Directory* d, this->subDirs)
   {
      if (d->getName() == name)
         return d;
   }

   return new Directory(this, name);
}

File* Directory::createFile(const QFileInfo& fileInfo)
{
   foreach (File* f, this->files)
   {
      if (f->getName() == fileInfo.fileName())
      {
         if (f->getSize() == fileInfo.size() && f->getDateLastModified() == fileInfo.lastModified())
            return f;

         delete f;
         break;
      }
   }

   return new File(this, fileInfo.fileName(), fileInfo.size(), fileInfo.lastModified());
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
      return;
      LOG_ERR("Directory::stealSubDirs(..) : dir == this");
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

