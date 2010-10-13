#ifndef NETWORKLISTENER_BUILDER_H
#define NETWORKLISTENER_BUILDER_H

#include <QSharedPointer>

#include <Core/FileManager/IFileManager.h>
#include <Core/PeerManager/IPeerManager.h>
#include <Core/DownloadManager/IDownloadManager.h>

#include <Core/NetworkListener/INetworkListener.h>

namespace NL
{
   class Builder
   {
   public:
      static QSharedPointer<INetworkListener> newNetworkListener(
         QSharedPointer<FM::IFileManager> fileManager,
         QSharedPointer<PM::IPeerManager> peerManager,
         QSharedPointer<DM::IDownloadManager> downloadManager
      );
   };
}
#endif
