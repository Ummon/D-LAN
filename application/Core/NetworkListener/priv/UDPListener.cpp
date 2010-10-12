#include <priv/UDPListener.h>
using namespace NL;

#if defined(Q_OS_LINUX)
   #include <netinet/in.h>
#elif defined(Q_OS_WIN32)
   #include <Winsock.h>
#endif

#include <Common/Settings.h>
#include <Common/Global.h>
#include <Common/Network.h>

#include <Core/PeerManager/IPeer.h>

#include <priv/Log.h>

/**
  * @class UDPListener
  * @author mcuony
  * @author gburri
  */

/**
  * Initialize the socket to broadcast.
  */
UDPListener::UDPListener(
   QSharedPointer<FM::IFileManager> fileManager,
   QSharedPointer<PM::IPeerManager> peerManager,
   QSharedPointer<DM::IDownloadManager> downloadManager,
   quint16 unicastPort
) :
   bodyBuffer(UDPListener::buffer + Common::Network::HEADER_SIZE),
   UNICAST_PORT(unicastPort),
   MULTICAST_GROUP(SETTINGS.getUInt32("multicast_group")),
   MULTICAST_PORT(SETTINGS.getUInt32("multicast_port")),
   fileManager(fileManager),
   peerManager(peerManager),
   downloadManager(downloadManager)
{
   // Creating and setting options to the socket.
   this->multicastSocket = new QUdpSocket(this);

   if (!this->multicastSocket->bind(MULTICAST_PORT, QUdpSocket::ReuseAddressHint))
   {
      L_ERRO("Can't bind the multicast socket");
   }

   connect(this->multicastSocket, SIGNAL(readyRead()), this, SLOT(processPendingMulticastDatagrams()));

   int socketDescriptor = this->multicastSocket->socketDescriptor();

   // 'loop' is activated only for tests.
   const char loop = 1;
   if (setsockopt(socketDescriptor, IPPROTO_IP, IP_MULTICAST_LOOP, &loop, sizeof loop))
      L_ERRO("Can't set socket option : IP_MULTICAST_LOOP");

   const char TTL = SETTINGS.getUInt32("multicast_ttl");
   if (int error = setsockopt(socketDescriptor, IPPROTO_IP, IP_MULTICAST_TTL, &TTL, sizeof TTL))
      L_ERRO(QString("Can't set socket option : IP_MULTICAST_TTL : %1").arg(error));

   // 'htonl' reverse the order of the bytes, see : http://www.opengroup.org/onlinepubs/007908799/xns/htonl.html
   struct ip_mreq mreq;
   mreq.imr_multiaddr.s_addr = htonl(MULTICAST_GROUP.toIPv4Address());
   mreq.imr_interface.s_addr = htonl(INADDR_ANY);
#if defined(Q_OS_LINUX)
   if (int error = setsockopt(socketDescriptor, IPPROTO_IP, IP_ADD_MEMBERSHIP, &mreq, sizeof mreq))
#elif defined(Q_OS_WIN32)
   if (int error = setsockopt(socketDescriptor, IPPROTO_IP, IP_ADD_MEMBERSHIP, (char*)&mreq, sizeof mreq))
#endif
      L_ERRO(QString("Can't set socket option : IP_ADD_MEMBERSHIP : %1").arg(error));

   this->unicastSocket = new QUdpSocket(this);
   if (!this->unicastSocket->bind(UNICAST_PORT, QUdpSocket::ReuseAddressHint))
      L_ERRO("Can't bind the unicast socket");
   connect(this->unicastSocket, SIGNAL(readyRead()), this, SLOT(processPendingUnicastDatagrams()));

   connect(&this->timerIMAlive, SIGNAL(timeout()), this, SLOT(presence()));
   this->timerIMAlive.start(static_cast<int>(SETTINGS.getUInt32("peer_imalive_period")));
   this->sendIMAliveMessage();
}

void UDPListener::sendIMAliveMessage()
{

}

/**
  *
  */
void UDPListener::processPendingMulticastDatagrams()
{
   while (this->multicastSocket->hasPendingDatagrams())
   {
      QHostAddress peerAddress;
      qint64 datagramSize = this->multicastSocket->readDatagram(buffer, MAX_DATAGRAM_SIZE, &peerAddress);

      const Common::MessageHeader& header = Common::Network::readHeader(buffer);
      if (header.size > datagramSize - Common::Network::HEADER_SIZE)
      {
         L_ERRO("header.size > datagramSize");
         continue;
      }

      if (header.senderID == this->peerManager->getID())
      {
         L_WARN("We receive a datagram from ourself, skip");
         continue;
      }

      PM::IPeer* peer = this->peerManager->getPeer(header.senderID);
      if (!peer)
      {
         L_WARN("We receive a datagram from an unknown peer, skip");
         continue;
      }

      if (!peer->isAlive())
      {
         L_WARN("We receive a datagram from a dead peer, skip");
         continue;
      }

      switch (header.type)
      {
      case 0x01: // IMAlive.
         {
            Protos::Core::IMAlive IMAliveMessage;
            IMAliveMessage.ParseFromArray(bodyBuffer, header.size);

            this->peerManager->updatePeer(
               header.senderID,
               peerAddress,
               IMAliveMessage.port(),
               QString::fromStdString(IMAliveMessage.nick()),
               IMAliveMessage.amount()
            );

            if (IMAliveMessage.chunk_size() > 0)
            {
               QList<Common::Hash> hashes;
               for (int i = 0; i < IMAliveMessage.chunk_size(); i++)
                  hashes << Common::Hash(IMAliveMessage.chunk(i).hash().data());
               QBitArray bitArray = this->fileManager->haveChunks(hashes);

               Protos::Core::ChunksOwned chunkOwnedMessage;
               chunkOwnedMessage.set_tag(IMAliveMessage.tag());
               for (int i = 0; i < bitArray.size(); i++)
                  chunkOwnedMessage.add_chunk_state(bitArray[i]);

               this->send(0x02, header.senderID, chunkOwnedMessage);
            }
         }
         break;

      case 0x11: // ChatMessage.
         {
            Protos::Core::ChatMessage chatMessage;
            chatMessage.ParseFromArray(bodyBuffer, header.size);
            emit newChatMessage(chatMessage);
         }
         break;

      case 0x21: // Find.
         {
            Protos::Core::Find findMessage;
            findMessage.ParseFromArray(bodyBuffer, header.size);

            /*Protos::Common::FindResult result = this->fileManager->find(QString::fromStdString(findMessage.pattern()));
            result*/
         }
         break;

      default:
         L_WARN(QString("Unkown header type : %1").arg(header.type, 0, 16));
      }

//      switch (datagram.data()[0])
//      {
//         case chatMessagePacket:
//         {
//            // We create a new chatMessage.
//            Protos::Core::ChatMessage chatMessage;

//            // We get the correct string.
//            QString data = datagram.data();
//            std::string input = data.mid(1).toStdString();

//            // We convert in into a proto.
//            chatMessage.ParseFromString(input);

//            // And we rise the event.
//            emit newChatMessage(chatMessage);

//            break;
//         }
//      }
   }
}

/**
  * Function called when data is recevied by the socket : The corresponding proto is created and the coresponding event is rised.
  */
void UDPListener::processPendingUnicastDatagrams()
{
//   QTextStream out(stdout);

//   while (this->unicastSocket->hasPendingDatagrams())
//   {
//      QByteArray datagram;
//      datagram.resize(this->unicastSocket->pendingDatagramSize());
//      QHostAddress peerAddress;
//      this->unicastSocket->readDatagram(datagram.data(), datagram.size(), &peerAddress);

//      LOG_DEBU(this->logger, "[UNICAST] Recived from " +  peerAddress.toString() + " message " + datagram.data());

//      switch (datagram.data()[0])
//      {

//         case findResultPacket:
//         {
//            // We create a new findMessage.
//            Protos::Common::FindResult findRMessage;
//            findRMessage.ParseFromString(datagram.mid(1).data());

//            emit newFindResult(findRMessage);

//            //this->logger->log("Someone is alive: " + id + ", " +data.fromStdString(IMAlimeMessage.nick()), LM::Debug);

//            break;
//         }




//         default:
//         {
//            LOG_DEBU(this->logger, "Unknow type ???");
//            break;
//         }
//      }
//   }
}

void UDPListener::send(quint32 type, const Common::Hash& peerID, const google::protobuf::Message& message)
{
   PM::IPeer* peer = this->peerManager->getPeer(peerID);
   if (!peer)
   {
      L_WARN(QString("Unable to find the peer %1").arg(peerID.toStr()));
      return;
   }

   int messageSize;
   if (!(messageSize = this->writeMessageToBuffer(type, message)))
      return;

   L_DEBU(QString("Send unicast UDP to %1 : header.type = %2, message size = %3 \n%4").arg(peer->toStringLog()).arg(type, 0, 16).arg(messageSize).arg(QString::fromStdString(message.DebugString())));

   if (this->unicastSocket->writeDatagram(this->buffer, messageSize, peer->getIP(), peer->getPort()) == -1)
      L_ERRO("Unable to send datagram");
}

/**
  * Send an UDP multicast message.
  */
void UDPListener::send(quint32 type, const google::protobuf::Message& message)
{
   int messageSize;
   if (!(messageSize = this->writeMessageToBuffer(type, message)))
      return;

   if (this->multicastSocket->writeDatagram(this->buffer, messageSize, MULTICAST_GROUP, MULTICAST_PORT) == -1)
      L_ERRO("Unable to send datagram");
}

int UDPListener::writeMessageToBuffer(quint32 type, const google::protobuf::Message& message)
{
   const int bodySize = message.ByteSize();
   Common::MessageHeader header(type, bodySize, this->peerManager->getID());

   if (Common::Network::HEADER_SIZE + bodySize > static_cast<int>(SETTINGS.getUInt32("max_udp_datagram_size")))
   {
      L_ERRO(QString("Datagram size too big : %1").arg(MAX_DATAGRAM_SIZE + bodySize));
      return 0;
   }

   Common::Network::writeHeader(this->buffer, header);
   message.SerializeToArray(this->bodyBuffer, MAX_DATAGRAM_SIZE - Common::Network::HEADER_SIZE);

   return Common::Network::HEADER_SIZE + bodySize;
}

