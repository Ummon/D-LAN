#ifndef UPLOADMANAGER_UPLOADER_H
#define UPLOADMANAGER_UPLOADER_H

#include <QThread>

namespace UM
{
   class Upload;

   class Uploader : public QThread
   {
   private:
      Upload* upload;
   };
}
#endif
