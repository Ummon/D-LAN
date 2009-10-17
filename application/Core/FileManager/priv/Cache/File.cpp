#include <priv/Cache/File.h>
using namespace FileManager;

#include <QString>

#include <IChunk.h>

#include <priv/FileManager.h>
#include <priv/Cache/Directory.h>
#include <priv/Cache/SharedDirectory.h>

File::File(Directory* dir, const QString& name, qint64 size)
   : Entry(name), dir(dir)
{
   this->name = name;
   this->size = size;
   this->dir->addFile(this);

   // The root must be a shared directory. If not, someone will be fired !
   static_cast<SharedDirectory*>(this->getRoot())->getFileManager()->addToWordIndex(this);
}

QString File::getPath()
{
   return this->dir->getPath() + "/" + this->name;
}

Directory* File::getRoot()
{
   return this->dir->getRoot();
}

void File::computeHashes()
{
   // TODO
}

QList<IChunk*> File::getChunks()
{
   return QList<IChunk*>();
}
