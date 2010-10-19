#ifndef REMOTECONTROLMANAGER_REMOTECONNECTION_H
#define REMOTECONTROLMANAGER_REMOTECONNECTION_H

#include <QObject>
#include <QSharedPointer>
#include <QTcpSocket>
#include <QTimer>

#include <google/protobuf/message.h>

#include <Core/FileManager/IFileManager.h>
#include <Core/PeerManager/IPeerManager.h>
#include <Core/UploadManager/IUploadManager.h>
#include <Core/DownloadManager/IDownloadManager.h>
#include <Core/NetworkListener/INetworkListener.h>

namespace RCM
{
   class RemoteConnection : public QObject
   {
      Q_OBJECT
   public:
      RemoteConnection(
         QSharedPointer<FM::IFileManager> fileManager,
         QSharedPointer<PM::IPeerManager> peerManager,
         QSharedPointer<UM::IUploadManager> uploadManager,
         QSharedPointer<DM::IDownloadManager> downloadManager,
         QSharedPointer<NL::INetworkListener> networkListener,
         QTcpSocket* socket
      );
      ~RemoteConnection();

   signals:
      void deleted(RemoteConnection*);

   private slots:
      void refresh();
      void dataReceived();
      void disconnected();

   private:
      void send(quint32 type, const google::protobuf::Message& message);

      QSharedPointer<FM::IFileManager> fileManager;
      QSharedPointer<PM::IPeerManager> peerManager;
      QSharedPointer<UM::IUploadManager> uploadManager;
      QSharedPointer<DM::IDownloadManager> downloadManager;
      QSharedPointer<NL::INetworkListener> networkListener;

      QTcpSocket* socket;
      QTimer timerRefresh;
   };
}
#endif
