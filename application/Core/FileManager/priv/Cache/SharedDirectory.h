#ifndef FILEMANAGER_SHAREDDIRECTORY_H
#define FILEMANAGER_SHAREDDIRECTORY_H

#include <QString>

#include <priv/Cache/Directory.h>

namespace FileManager
{
   class FileManager;

   class SharedDirectory : public Directory
   {
   public:
      SharedDirectory(FileManager* file, const QString& path);

      /**
        * Return the full path to the shared directory.
        * For exemple : '/home/paul/movies'. (no slash at the end).
        */
      QString getPath();

      FileManager* getFileManager();

   private:
      FileManager* fileManager;
   };

}
#endif
