#ifndef FILEMANAGER_IFILE_H
#define FILEMANAGER_IFILE_H

namespace FM
{
   class IChunk;

   class IFile
   {
   public:
      virtual ~IFile() {}

      virtual QList<IChunk*> getChunks() = 0;
   };
}
#endif
