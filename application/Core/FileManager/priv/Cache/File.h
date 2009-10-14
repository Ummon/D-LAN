#ifndef FILEMANAGER_FILE_H
#define FILEMANAGER_FILE_H

#include <QString>
#include <QList>

#include <IFile.h>
#include <priv/Cache/Entry.h>

namespace FileManager
{
   class Chunk;
   class Directory;
   class IChunk;

   class File : public Entry, public IFile
   {
   public:
      File(Directory* dir, const QString& name, qint64 size);
      virtual ~File() {};

      QString getPath();

      QList<IChunk*> getChunks();

   private:
      Directory* dir;

      QList<Chunk*> chunk;
   };
}
#endif
