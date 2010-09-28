#ifndef UPLOADMANAGER_IUPLOADMANAGER_H
#define UPLOADMANAGER_IUPLOADMANAGER_H

#include <QList>

namespace UM
{
   class IUpload;
   class IUploadManager : public QObject
   {
   public:
      virtual ~IUploadManager() {}

      QList<IUpload*> getUploads();
   };
}
#endif
