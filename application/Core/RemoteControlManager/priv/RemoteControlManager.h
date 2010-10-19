#ifndef REMOTECONTROLMANAGER_REMOTECONTROLMANAGER_H
#define REMOTECONTROLMANAGER_REMOTECONTROLMANAGER_H

#include <QObject>
#include <QSharedPointer>
#include <QList>
#include <QTcpServer>

#include <IRemoteControlManager.h>

#include <Core/FileManager/IFileManager.h>
#include <Core/PeerManager/IPeerManager.h>
#include <Core/UploadManager/IUploadManager.h>
#include <Core/DownloadManager/IDownloadManager.h>
#include <Core/NetworkListener/INetworkListener.h>

#include <priv/RemoteConnection.h>

namespace RCM
{
   class RemoteControlManager : public QObject, public IRemoteControlManager
   {
      Q_OBJECT
   public:
      RemoteControlManager(
         QSharedPointer<FM::IFileManager> fileManager,
         QSharedPointer<PM::IPeerManager> peerManager,
         QSharedPointer<UM::IUploadManager> uploadManager,
         QSharedPointer<DM::IDownloadManager> downloadManager,
         QSharedPointer<NL::INetworkListener> networkListener
      );

   private slots:
      void newConnection();
      void connectionDeleted(RemoteConnection* sender);

   private:
      QSharedPointer<FM::IFileManager> fileManager;
      QSharedPointer<PM::IPeerManager> peerManager;
      QSharedPointer<UM::IUploadManager> uploadManager;
      QSharedPointer<DM::IDownloadManager> downloadManager;
      QSharedPointer<NL::INetworkListener> networkListener;

      QTcpServer tcpServer;
      QList<RemoteConnection*> connections;
   };
}
#endif
