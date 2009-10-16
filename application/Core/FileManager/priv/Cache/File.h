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
      Directory* getRoot();

      /**
        * It will open the file, read it and calculate all theirs chunk hashes.
        * If it already owns some chunks, there are destroyed first.
        * This method can be called from an another thread than the main one. For example,
        * from 'FileUpdated' thread.
        */
      void computeHashes();

      QList<IChunk*> getChunks();

   private:
      Directory* dir;
      QList<Chunk*> chunk;
   };
}
#endif
