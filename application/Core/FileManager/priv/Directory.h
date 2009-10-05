#ifndef FILEMANAGER_DIRECTORY_H
#define FILEMANAGER_DIRECTORY_H

#include <QString>
#include <QList>

namespace FileManager
{
   class File;

   class Directory
   {
   private:
      QString name;

      QList<Directory*> subDirs;
      QList<File*> files;
   };
}
#endif
