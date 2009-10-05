#ifndef FILEMANAGER_FILE_H
#define FILEMANAGER_FILE_H

#include <QString>
#include <QList>

#include <IFile.h>

namespace FileManager
{
   class Chunk;

   class File : public IFile
   {
   public:
      virtual ~File();

   private:
      QString name;
      quint64 size;
      QList<Chunk*> chunk;
   };
}
#endif
