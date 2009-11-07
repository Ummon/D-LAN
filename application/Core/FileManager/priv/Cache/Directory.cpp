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
{}

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
      for (int i = 0; i < dir.file_size(); i++)
         for (QListIterator<File*> f(this->files); f.hasNext();)
            ret << f.next()->restoreFromFileCache(dir.file(i));
   }

   return ret;
}

void Directory::populateHashesDir(Protos::FileCache::Hashes_Dir& dirToFill) const
{
   dirToFill.set_name(this->getName().toStdString());

   for (QListIterator<File*> f(this->files); f.hasNext();)
   {
      Protos::FileCache::Hashes_File* file = dirToFill.add_file();
      f.next()->populateHashesFile(*file);
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

void Directory::addFile(File* file)
{
   if (this->files.contains(file))
      return;

   this->files.append(file);

   this->addSize(file->getSize());
}

void Directory::stealSubDirs(Directory* dir)
{
   this->subDirs.append(dir->subDirs);

   foreach (Directory* d, dir->subDirs)
   {
      d->parent = this;
      this->addSize(d->getSize());
      dir->size -= d->getSize();
   }

   dir->subDirs.clear();
}

void Directory::addSize(qint64 size)
{
   this->size += size;

   if (this->parent)
      this->parent->addSize(this->size);
}

