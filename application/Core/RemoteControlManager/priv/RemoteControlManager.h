#ifndef REMOTECONTROLMANAGER_REMOTECONTROLMANAGER_H
#define REMOTECONTROLMANAGER_REMOTECONTROLMANAGER_H

#include <QObject>
#include <QSharedPointer>
#include <QList>
#include <QTcpServer>

#include <Common/Uncopyable.h>
#include <Core/FileManager/IFileManager.h>
#include <Core/PeerManager/IPeerManager.h>
#include <Core/UploadManager/IUploadManager.h>
#include <Core/DownloadManager/IDownloadManager.h>
#include <Core/NetworkListener/INetworkListener.h>

#include <IRemoteControlManager.h>
#include <priv/RemoteConnection.h>

namespace RCM
{
   class RemoteControlManager : public QObject, public IRemoteControlManager, Common::Uncopyable
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

      ~RemoteControlManager();

   private slots:
      void newConnection();
      void connectionDeleted(RemoteConnection* sender);
      void chatMessageSent(const QString& message);

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
