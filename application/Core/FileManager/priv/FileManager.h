#ifndef FILEMANAGER_FILEMANAGER_H
#define FILEMANAGER_FILEMANAGER_H

#include <IFileManager.h>

namespace FileManager
{
   class Chunks;
   class FileUpdater;
   class WordIndex;
   class SharedDirectory;

   class FileManager : public IFileManager
   {
   public :
      virtual ~FileManager();

   private:
      Chunks* chunks;
      FileUpdater* fileUpdater;
      WordIndex* wordIndex;
      QList<SharedDirectory*> sharedDirReadWrite;
      QList<SharedDirectory*> sharedDirReadOnly;
   };
}
#endif
