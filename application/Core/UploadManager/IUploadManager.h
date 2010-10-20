#ifndef UPLOADMANAGER_IUPLOADMANAGER_H
#define UPLOADMANAGER_IUPLOADMANAGER_H

#include <QList>

namespace UM
{
   class IUpload;
   class IUploadManager
   {
   public:
      virtual ~IUploadManager() {}

      virtual QList<IUpload*> getUploads() = 0;

      /**
        * @return Byte/s.
        */
      virtual int getUploadRate() const = 0;
   };
}
#endif
