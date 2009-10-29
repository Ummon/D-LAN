#ifndef NETWORKMANAGER_UDPLISTENER_H
#define NETWORKMANAGER_UDPLISTENER_H

#include <QObject>
#include <QSharedPointer>
#include <QtNetwork/QNetworkInterface>
#include <QtNetwork/QUdpSocket>

#include <Common/LogManager/ILogger.h>

#include <Protos/core_protocol.pb.h>
#include <Protos/common.pb.h>

#include <Core/PeerManager/IPeerManager.h>

namespace NL
{
   class UDPListener : public QObject
   {
   Q_OBJECT

   public:
      UDPListener(QSharedPointer<PM::IPeerManager> newPeerManager);
      bool sendMessage(const QByteArray& datagram);
      bool sendMessageTo(const QByteArray& datagram, const QHostAddress& ipTo);

   signals:
      void newChatMessage(const Protos::Core::ChatMessage& message);
      void newFindResult(const Protos::Common::FindResult& result);
      void newFindRequset(const Protos::Core::Find& request, const QHostAddress& peerAdress);
      void newHaveChunksResult(const Protos::Core::HaveChunksResult& result);

   private:
      QSharedPointer<LM::ILogger> logger;
      QSharedPointer<PM::IPeerManager> peerManager;
      static const char TTL; ///< Time to live, see the UDP multicast documentation.
      static const int multicastPort;
      static const int unicastPort;
      static QHostAddress multicastIP; ///< A choosen multicast address channel used to send and received messages.
      QUdpSocket* multicastSocket;
      QUdpSocket* unicastSocket;

   private slots:
      void processPendingMulticastDatagrams();
      void processPendingUnicastDatagrams();

   };

   enum messageUDPType
   {
      chatMessagePacket = 1,
      IAmAlivePacket = 2,
      findPacket = 3,
      findResultPacket = 4
   };
}
#endif
