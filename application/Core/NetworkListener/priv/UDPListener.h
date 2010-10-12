#ifndef NETWORKMANAGER_UDPLISTENER_H
#define NETWORKMANAGER_UDPLISTENER_H

#include <QObject>
#include <QTimer>
#include <QSharedPointer>
#include <QtNetwork/QNetworkInterface>
#include <QtNetwork/QUdpSocket>

#include <google/protobuf/message.h>

#include <Protos/core_protocol.pb.h>
#include <Protos/common.pb.h>

#include <Core/FileManager/IFileManager.h>
#include <Core/PeerManager/IPeerManager.h>
#include <Core/DownloadManager/IDownloadManager.h>

namespace NL
{
   class UDPListener : public QObject
   {
      Q_OBJECT
      static const int MAX_DATAGRAM_SIZE = 65536;

   public:
      UDPListener(
         QSharedPointer<FM::IFileManager> fileManager,
         QSharedPointer<PM::IPeerManager> peerManager,
         QSharedPointer<DM::IDownloadManager> downloadManager,
         quint16 unicastPort
      );

      void send(quint32 type, const Common::Hash& peerID, const google::protobuf::Message& message);
      void send(quint32 type, const google::protobuf::Message& message);

   signals:
      void newChatMessage(const Protos::Core::ChatMessage& message);
      void newFindResultMessage(const Protos::Core::Find& request);

   private slots:
      void sendIMAliveMessage();
      void processPendingMulticastDatagrams();
      void processPendingUnicastDatagrams();

   private:
      int writeMessageToBuffer(quint32 type, const google::protobuf::Message& message);

      char buffer[MAX_DATAGRAM_SIZE]; // Buffer used when sending or receiving datagram.
      char* const bodyBuffer;

      const quint16 UNICAST_PORT;
      const QHostAddress MULTICAST_GROUP;
      const quint16 MULTICAST_PORT;

      QSharedPointer<FM::IFileManager> fileManager;
      QSharedPointer<PM::IPeerManager> peerManager;
      QSharedPointer<DM::IDownloadManager> downloadManager;

      QUdpSocket* multicastSocket;
      QUdpSocket* unicastSocket;

      QTimer timerIMAlive;

   };
}
#endif
