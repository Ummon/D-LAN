#ifndef REMOTECONTROLMANAGER_BUILDER_H
#define REMOTECONTROLMANAGER_BUILDER_H

#include <QSharedPointer>

#include <Core/FileManager/IFileManager.h>
#include <Core/PeerManager/IPeerManager.h>
#include <Core/UploadManager/IUploadManager.h>
#include <Core/DownloadManager/IDownloadManager.h>
#include <Core/NetworkListener/INetworkListener.h>

#include <Core/RemoteControlManager/IRemoteControlManager.h>

namespace RCM
{
   class Builder
   {
   public:
      static QSharedPointer<IRemoteControlManager> newRemoteControlManager(
         QSharedPointer<FM::IFileManager> fileManager,
         QSharedPointer<PM::IPeerManager> peerManager,
         QSharedPointer<UM::IUploadManager> uploadManager,
         QSharedPointer<DM::IDownloadManager> downloadManager,
         QSharedPointer<NL::INetworkListener> networkListener
      );
   };
}

#endif
