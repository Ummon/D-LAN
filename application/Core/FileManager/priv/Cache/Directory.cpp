#include <priv/Cache/Directory.h>
using namespace FM;

#include <QDir>

#include <priv/FileManager.h>
#include <priv/Cache/File.h>
#include <priv/Cache/SharedDirectory.h>

Directory::Directory(Directory* parent, const QString& name)
   : Entry(name), parent(parent)
{
   this->parent->subDirs.append(this);

   // Same as a new file (see the File ctor).
   static_cast<SharedDirectory*>(this->getRoot())->getCache()->onEntryAdded(this);
}

Directory::Directory()
   : Entry(""), parent(0)
{
}

QString Directory::getPath()
{
   QString path;
   Directory* dir = this;
   while (dir->parent)
   {
      dir = dir->parent;
      if (dir->parent)
         path.prepend('/');
      path.prepend(dir->getName());
   }
   return path;
}

QString Directory::getFullPath()
{
   return this->parent->getFullPath().append('/').append(this->name);
}

Directory* Directory::getRoot()
{
   if (this->parent)
      return this->parent->getRoot();
   return this;
}

void Directory::populateDirEntry(Protos::Common::DirEntry* entry)
{
   entry->mutable_dir()->set_path(this->getPath().toStdString());
   entry->mutable_dir()->set_name(this->getName().toStdString());
}

void Directory::populateHashesDir(Protos::FileCache::Hashes_Dir& dirToFill)
{
   dirToFill.set_name(this->name.toStdString());

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

void Directory::addFile(File* file)
{
   if (this->files.contains(file))
      return;

   this->files.append(file);

   this->addSize(file->getSize());
}

void Directory::addSize(qint64 size)
{
   this->size += size;

   if (this->parent)
      this->parent->addSize(this->size);
}

