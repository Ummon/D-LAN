#include <priv/Directory.h>
using namespace FileManager;

#include <priv/File.h>

Directory::Directory(Directory* parent, const QString& name)
   : Entry(name), parent(parent)
{
   this->parent->subDirs.append(this);
}

Directory::Directory(const QString& name)
   : Entry(name), parent(0)
{
}

QString Directory::getPath()
{
   return this->parent->getPath() + "/" + this->name;
}

void Directory::addFile(File* file)
{
   if (this->files.contains(file))
      return;

   this->files.append(file);
   this->size += file->getSize();
}

