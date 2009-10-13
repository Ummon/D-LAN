#include <priv/File.h>
using namespace FileManager;

#include <QString>

#include <IChunk.h>

#include <priv/Directory.h>

File::File(Directory* dir, const QString& name, qint64 size)
   : Entry(name), dir(dir)
{
   this->name = name;
   this->size = size;
   this->dir->addFile(this);
}

QString File::getPath()
{
   return this->dir->getPath() + "/" + this->name;
}

QList<IChunk*> File::getChunks()
{
   return QList<IChunk*>();
}
