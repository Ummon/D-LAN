#ifndef FILEMANAGER_DIRECTORY_H
#define FILEMANAGER_DIRECTORY_H

#include <QString>
#include <QList>

#include <priv/Entry.h>

namespace FileManager
{
   class File;

   class Directory : public Entry
   {
   public:
      Directory(Directory* parent, const QString& name);
      virtual ~Directory() {};
      virtual QString getPath();

      /**
        * Only called by the class File.
        */
      void addFile(File* file);

   protected:
      Directory(const QString& name);

   private:
      Directory* parent;

      QList<Directory*> subDirs;
      QList<File*> files;
   };
}
#endif
