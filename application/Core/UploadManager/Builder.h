#ifndef UPLOADMANAGER_BUILDER_H
#define UPLOADMANAGER_BUILDER_H

#include <QSharedPointer>

namespace FM { class IFileManager; }
namespace PM { class IPeerManager; }

namespace UM
{
   class IUploadManager;

   class Builder
   {
   public:
      static QSharedPointer<IUploadManager> newFileManager(QSharedPointer<FM::IFileManager> fileManager, QSharedPointer<PM::IPeerManager> peerManager);
   };
}
#endif
