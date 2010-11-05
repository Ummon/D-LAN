#ifndef NETWORKLISTENER_UDPLISTENER_H
#define NETWORKLISTENER_UDPLISTENER_H

#include <QObject>
#include <QUdpSocket>
#include <QTimer>
#include <QSharedPointer>
#include <QtNetwork/QNetworkInterface>
#include <QtNetwork/QUdpSocket>

#include <Libs/MersenneTwister.h>

#include <google/protobuf/message.h>

#include <Protos/core_protocol.pb.h>
#include <Protos/common.pb.h>

#include <Common/Network.h>
#include <Common/LogManager/Builder.h>
#include <Common/LogManager/ILogger.h>
#include <Core/FileManager/IFileManager.h>
#include <Core/PeerManager/IPeerManager.h>
#include <Core/DownloadManager/IDownloadManager.h>

namespace NL
{
   class UDPListener : public QObject
   {
      Q_OBJECT
      static const int BUFFER_SIZE = 65536; // The size max of an UDP datagram : 2^16.

   public:
      UDPListener(
         QSharedPointer<FM::IFileManager> fileManager,
         QSharedPointer<PM::IPeerManager> peerManager,
         QSharedPointer<DM::IDownloadManager> downloadManager,
         quint16 unicastPort
      );

      void send(Common::Network::CoreMessageType type, const Common::Hash& peerID, const google::protobuf::Message& message);
      void send(Common::Network::CoreMessageType type, const google::protobuf::Message& message);

   signals:
      void newChatMessage(const Common::Hash&, const Protos::Core::ChatMessage& chatMessage);
      void newFindResultMessage(const Protos::Common::FindResult& findResult);

   private slots:
      void sendIMAliveMessage();
      void processPendingMulticastDatagrams();
      void processPendingUnicastDatagrams();

   private:
      int writeMessageToBuffer(Common::Network::CoreMessageType type, const google::protobuf::Message& message);
      Common::Network::MessageHeader<Common::Network::CoreMessageType> readDatagramToBuffer(QUdpSocket& socket, QHostAddress& peerAddress);

      char buffer[BUFFER_SIZE]; // Buffer used when sending or receiving datagram.
      char* const bodyBuffer;

      const quint16 UNICAST_PORT;
      const QHostAddress MULTICAST_GROUP;
      const quint16 MULTICAST_PORT;

      QSharedPointer<FM::IFileManager> fileManager;
      QSharedPointer<PM::IPeerManager> peerManager;
      QSharedPointer<DM::IDownloadManager> downloadManager;

      QUdpSocket multicastSocket;
      QUdpSocket unicastSocket;

      MTRand mtrand;
      quint64 currentIMAliveTag;
      QList< QSharedPointer<DM::IChunkDownload> > currentChunkDownloads;

      QTimer timerIMAlive;
      QSharedPointer<LM::ILogger> loggerIMAlive; // A logger especially for the IMAlive message.
   };
}
#endif
