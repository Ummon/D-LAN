#ifndef DOWNLOADMANAGER_BUILDER_H
#define DOWNLOADMANAGER_BUILDER_H

#include <QSharedPointer>

#include <Core/FileManager/IFileManager.h>
#include <Core/PeerManager/IPeerManager.h>

namespace DM
{
   class IDownloadManager;

   class Builder
   {
   public:
      static QSharedPointer<IDownloadManager> newDownloadManager(QSharedPointer<FM::IFileManager> fileManager, QSharedPointer<PM::IPeerManager> peerManager);
   };
}
#endif
