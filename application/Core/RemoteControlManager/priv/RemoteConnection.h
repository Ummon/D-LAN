#ifndef REMOTECONTROLMANAGER_REMOTECONNECTION_H
#define REMOTECONTROLMANAGER_REMOTECONNECTION_H

#include <QObject>
#include <QSharedPointer>
#include <QTcpSocket>
#include <QTimer>
#include <QList>

#include <Libs/MersenneTwister.h>

#include <google/protobuf/message.h>

#include <Protos/common.pb.h>

#include <Common/Network.h>
#include <Core/FileManager/IFileManager.h>
#include <Core/PeerManager/IPeerManager.h>
#include <Core/PeerManager/IGetEntriesResult.h>
#include <Core/UploadManager/IUploadManager.h>
#include <Core/DownloadManager/IDownloadManager.h>
#include <Core/NetworkListener/INetworkListener.h>
#include <Core/NetworkListener/ISearch.h>

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

      void newChatMessage(const Common::Hash& peerID, const Protos::Core::ChatMessage&);
      void searchFound(const Protos::Common::FindResult& result);
      void getEntriesResult(const Protos::Common::Entries& entries);

   private:
      bool readMessage();
      void send(quint32 type, const google::protobuf::Message& message);

      QSharedPointer<FM::IFileManager> fileManager;
      QSharedPointer<PM::IPeerManager> peerManager;
      QSharedPointer<UM::IUploadManager> uploadManager;
      QSharedPointer<DM::IDownloadManager> downloadManager;
      QSharedPointer<NL::INetworkListener> networkListener;

      QTcpSocket* socket;
      QTimer timerRefresh;

      Common::MessageHeader currentHeader;

      QList< QSharedPointer<NL::ISearch> > currentSearches;
      QList< QSharedPointer<PM::IGetEntriesResult> > getEntriesResults;

      MTRand mtrand;
   };
}
#endif
