#ifndef FILEMANAGER_BUILDER_H
#define FILEMANAGER_BUILDER_H

#include <QSharedPointer>

#include "priv/FileManager_global.h"

namespace FM
{
   class IFileManager;

   class FILEMANAGER_EXPORT Builder
   {
   public:
      static QSharedPointer<IFileManager> newFileManager();
   };
}
#endif
