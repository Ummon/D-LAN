#ifndef FILEMANAGER_SHAREDDIRECTORY_H
#define FILEMANAGER_SHAREDDIRECTORY_H

#include <QString>

#include <Common/Hash.h>
#include <priv/Cache/Directory.h>

namespace FileManager
{
   class FileManager;

   class SharedDirectory : public Directory
   {
   public:
      enum Rights {
         READ_ONLY,
         READ_WRITE
      };

      SharedDirectory(FileManager* file, const QString& path);

      /**
        * Return the full path to the shared directory.
        * For exemple : '/home/paul/movies'. (no slash at the end).
        */
      QString getPath();

      FileManager* getFileManager();

      Rights getRights();

   private:
      FileManager* fileManager;
      Common::Hash id;
      Rights rights;
   };

}
#endif
