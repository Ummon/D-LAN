#ifndef FILEMANAGER_BUILDER_H
#define FILEMANAGER_BUILDER_H

#include <QSharedPointer>

namespace FM
{
   class IFileManager;

   class Builder
   {
   public:
      static QSharedPointer<IFileManager> newFileManager();
   };
}
#endif
