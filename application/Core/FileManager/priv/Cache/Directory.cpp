#include <priv/Cache/Directory.h>
using namespace FileManager;

#include <QDir>

#include <priv/FileManager.h>
#include <priv/Cache/File.h>
#include <priv/Cache/SharedDirectory.h>

Directory::Directory(Directory* parent, const QString& name)
   : Entry(name), parent(parent)
{
   this->parent->subDirs.append(this);

   // Same as a new file (see the File ctor).
   static_cast<SharedDirectory*>(this->getRoot())->getFileManager()->addToWordIndex(this);
}

Directory::Directory(const QString& name)
   : Entry(name), parent(0)
{
}

QString Directory::getPath()
{
   return this->parent->getPath() + QDir::separator() + this->name;
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

