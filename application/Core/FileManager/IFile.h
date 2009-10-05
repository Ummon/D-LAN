#ifndef FILEMANAGER_IFILE_H
#define FILEMANAGER_IFILE_H

namespace FileManager
{
   class IChunk;
   
   class IFile
   {
   public:
      virtual QList<IChunk*> getChunks() = 0;   
   };
}
#endif
