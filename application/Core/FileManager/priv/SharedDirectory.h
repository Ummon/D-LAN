#ifndef FILEMANAGER_SHAREDDIRECTORY_H
#define FILEMANAGER_SHAREDDIRECTORY_H

#include <QString>

#include <priv/Directory.h>

namespace FileManager
{
   class SharedDirectory : public Directory
   {
   private:
      QString path;   
   };

}
#endif
