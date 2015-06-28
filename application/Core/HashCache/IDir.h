#ifndef HASHCACHE_IDIR_H
#define HASHCACHE_IDIR_H

#include <QList>

#include <IEntry.h>

namespace HC
{
   class IFile;
   class IDir : public IEntry
   {
   public:
      virtual ~IDir() {}

      virtual QList<IFile> getFiles() = 0;
      virtual QList<IDir> getDirs() = 0;
   };
}

#endif
