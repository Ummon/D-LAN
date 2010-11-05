#include <priv/UDPListener.h>
using namespace NL;

#if defined(Q_OS_LINUX)
   #include <netinet/in.h>
#elif defined(Q_OS_WIN32)
   #include <Winsock.h>
#endif

#include <google/protobuf/message.h>

#include <Common/Settings.h>
#include <Common/Global.h>
#include <Common/Network.h>
#include <Common/ProtoHelper.h>

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
   MULTICAST_GROUP(SETTINGS.get<quint32>("multicast_group")),
   MULTICAST_PORT(SETTINGS.get<quint32>("multicast_port")),
   fileManager(fileManager),
   peerManager(peerManager),
   downloadManager(downloadManager),
   currentIMAliveTag(0),
   loggerIMAlive(LM::Builder::newLogger("NetworkListener (IMAlive)"))
{
   if (!this->multicastSocket.bind(MULTICAST_PORT, QUdpSocket::ReuseAddressHint))
   {
      L_ERRO("Can't bind the multicast socket");
   }

   connect(&this->multicastSocket, SIGNAL(readyRead()), this, SLOT(processPendingMulticastDatagrams()));

   int socketDescriptor = this->multicastSocket.socketDescriptor();

   // 'loop' is activated only for tests.
#if DEBUG
   const char loop = 1;
#else
   const char loop = 0;
#endif
   if (setsockopt(socketDescriptor, IPPROTO_IP, IP_MULTICAST_LOOP, &loop, sizeof loop))
      L_ERRO("Can't set socket option : IP_MULTICAST_LOOP");

   const char TTL = SETTINGS.get<quint32>("multicast_ttl");
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

   if (!this->unicastSocket.bind(UNICAST_PORT, QUdpSocket::ReuseAddressHint))
      L_ERRO("Can't bind the unicast socket");
   connect(&this->unicastSocket, SIGNAL(readyRead()), this, SLOT(processPendingUnicastDatagrams()));

   connect(&this->timerIMAlive, SIGNAL(timeout()), this, SLOT(sendIMAliveMessage()));
   this->timerIMAlive.start(static_cast<int>(SETTINGS.get<quint32>("peer_imalive_period")));
   this->sendIMAliveMessage();
}

void UDPListener::send(Common::Network::CoreMessageType type, const Common::Hash& peerID, const google::protobuf::Message& message)
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

   L_DEBU(QString("Send unicast UDP to %1 : header.type = %2, message size = %3 \n%4").
      arg(peer->toStringLog()).
      arg(type, 0, 16).arg(messageSize).
      arg(Common::ProtoHelper::getDebugStr(message))
   );

   if (this->unicastSocket.writeDatagram(this->buffer, messageSize, peer->getIP(), peer->getPort()) == -1)
      L_ERRO("Unable to send datagram");
}

/**
  * Send an UDP multicast message.
  */
void UDPListener::send(Common::Network::CoreMessageType type, const google::protobuf::Message& message)
{
   int messageSize;
   if (!(messageSize = this->writeMessageToBuffer(type, message)))
      return;

#if DEBUG
   QString logMess = QString("Send multicast UDP : header.type = %2, message size = %3 \n%4").arg(type, 0, 16).arg(messageSize).arg(Common::ProtoHelper::getDebugStr(message));
   if (type == Common::Network::CORE_IM_ALIVE)
      LOG_DEBU(this->loggerIMAlive, logMess);
   else
      L_DEBU(logMess);
#endif

   if (this->multicastSocket.writeDatagram(this->buffer, messageSize, MULTICAST_GROUP, MULTICAST_PORT) == -1)
      L_ERRO("Unable to send datagram");
}

void UDPListener::sendIMAliveMessage()
{
   Protos::Core::IMAlive IMAliveMessage;
   IMAliveMessage.set_version(SETTINGS.get<quint32>("protocol_version"));
   IMAliveMessage.set_port(this->UNICAST_PORT);
   Common::ProtoHelper::setStr(IMAliveMessage, &Protos::Core::IMAlive::set_nick, this->peerManager->getNick());
   IMAliveMessage.set_amount(this->fileManager->getAmount());

   this->currentIMAliveTag = this->mtrand.randInt();
   this->currentIMAliveTag <<= 32;
   this->currentIMAliveTag |= this->mtrand.randInt();
   IMAliveMessage.set_tag(this->currentIMAliveTag);

   this->currentChunkDownloads = this->downloadManager->getUnfinishedChunks(SETTINGS.get<quint32>("number_of_hashes_sent_imalive"));
   IMAliveMessage.mutable_chunk()->Reserve(this->currentChunkDownloads.size());
   for (QListIterator< QSharedPointer<DM::IChunkDownload> > i(this->currentChunkDownloads); i.hasNext();)
   {
      IMAliveMessage.add_chunk()->set_hash(i.next()->getHash().getData(), Common::Hash::HASH_SIZE);
   }

   this->send(Common::Network::CORE_IM_ALIVE, IMAliveMessage);
}

/**
  *
  */
void UDPListener::processPendingMulticastDatagrams()
{
   while (this->multicastSocket.hasPendingDatagrams())
   {
      QHostAddress peerAddress;
      const Common::Network::MessageHeader& header =  UDPListener::readDatagramToBuffer(this->multicastSocket, peerAddress);
      if (header.isNull())
         continue;

      switch (header.type)
      {
      case Common::Network::CORE_IM_ALIVE:
         {
            Protos::Core::IMAlive IMAliveMessage;
            IMAliveMessage.ParseFromArray(this->bodyBuffer, header.size);

            if (IMAliveMessage.version() != SETTINGS.get<quint32>("protocol_version"))
            {
               L_WARN(QString("Peer protocol (%1) doesn't match the current one (%2)").arg(IMAliveMessage.version()).arg(SETTINGS.get<quint32>("protocol_version")));
               continue;
            }

            PM::IPeer* peer = this->peerManager->getPeer(header.senderID);

            this->peerManager->updatePeer(
               header.senderID,
               peerAddress,
               IMAliveMessage.port(),
               Common::ProtoHelper::getStr(IMAliveMessage, &Protos::Core::IMAlive::nick),
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

               this->send(Common::Network::CORE_CHUNKS_OWNED, header.senderID, chunkOwnedMessage);
            }

            // If we don't know the peer, he may not know us.
            if (!peer || !peer->isAlive())
            {
               this->timerIMAlive.start();
               this->sendIMAliveMessage();
            }
         }
         break;

      case Common::Network::CORE_CHAT_MESSAGE:
         {
            Protos::Core::ChatMessage chatMessage;
            chatMessage.ParseFromArray(this->bodyBuffer, header.size);
            emit newChatMessage(header.senderID, chatMessage);
         }
         break;

      case Common::Network::CORE_FIND: // Find.
         {
            Protos::Core::Find findMessage;
            findMessage.ParseFromArray(this->bodyBuffer, header.size);

            QList<Protos::Common::FindResult> results =
               this->fileManager->find(
                  Common::ProtoHelper::getStr(findMessage, &Protos::Core::Find::pattern),
                  SETTINGS.get<quint32>("max_number_of_search_result_to_send"),
                  SETTINGS.get<quint32>("max_udp_datagram_size") - Common::Network::HEADER_SIZE
               );

            for (QMutableListIterator<Protos::Common::FindResult> i(results); i.hasNext();)
            {
               Protos::Common::FindResult& result = i.next();
               result.set_tag(findMessage.tag());
               this->send(Common::Network::CORE_FIND_RESULT, header.senderID, result);
            }
         }
         break;

      default:
         L_WARN(QString("Unkown header type from multicast socket : %1").arg(header.type, 0, 16));
      }
   }
}

/**
  * Function called when data is recevied by the socket : The corresponding proto is created and the coresponding event is rised.
  */
void UDPListener::processPendingUnicastDatagrams()
{
   while (this->unicastSocket.hasPendingDatagrams())
   {
      QHostAddress peerAddress;
      const Common::Network::MessageHeader& header =  UDPListener::readDatagramToBuffer(this->unicastSocket, peerAddress);
      if (header.isNull())
         continue;

      switch (header.type)
      {
      case 0x02: // ChunksOwned.
         {
            Protos::Core::ChunksOwned chunksOwnedMessage;
            chunksOwnedMessage.ParseFromArray(this->bodyBuffer, header.size);

            if (chunksOwnedMessage.tag() != this->currentIMAliveTag)
            {
               L_WARN(QString("ChunksOwned : tag (%1) doesn't match current tag (%2)").arg(chunksOwnedMessage.tag()).arg(currentIMAliveTag));
               continue;
            }

            if (chunksOwnedMessage.chunk_state_size() != this->currentChunkDownloads.size())
            {
               L_WARN(QString("ChunksOwned : The size (%1) doesn't match the expected one (%2)").arg(chunksOwnedMessage.chunk_state_size()).arg(this->currentChunkDownloads.size()));
               continue;
            }

            for (int i = 0; i < chunksOwnedMessage.chunk_state_size(); i++)
            {
               if (chunksOwnedMessage.chunk_state(i))
                  this->currentChunkDownloads[i]->addPeerID(header.senderID);
               else
                  this->currentChunkDownloads[i]->rmPeerID(header.senderID);
            }
         }
         break;

      case 0x22: // FindResult.
         {
            Protos::Common::FindResult findResultMessage;
            findResultMessage.ParseFromArray(this->bodyBuffer, header.size);
            findResultMessage.mutable_peer_id()->set_hash(header.senderID.getData(), Common::Hash::HASH_SIZE);
            emit newFindResultMessage(findResultMessage);
         }
         break;

      default:
         L_WARN(QString("Unkown header type from unicast socket : %1").arg(header.type, 0, 16));
      }
   }
}

int UDPListener::writeMessageToBuffer(Common::Network::CoreMessageType type, const google::protobuf::Message& message)
{
   const int bodySize = message.ByteSize();
   Common::Network::MessageHeader header(type, bodySize, this->peerManager->getID());

   if (Common::Network::HEADER_SIZE + bodySize > static_cast<int>(SETTINGS.get<quint32>("max_udp_datagram_size")))
   {
      L_ERRO(QString("Datagram size too big : %1").arg(BUFFER_SIZE + bodySize));
      return 0;
   }

   Common::Network::writeHeader(this->buffer, header);
   message.SerializeToArray(this->bodyBuffer, BUFFER_SIZE - Common::Network::HEADER_SIZE);

   return Common::Network::HEADER_SIZE + bodySize;
}

/**
  * @return false if error.
  */
Common::Network::MessageHeader UDPListener::readDatagramToBuffer(QUdpSocket& socket, QHostAddress& peerAddress)
{
   qint64 datagramSize = socket.readDatagram(this->buffer, BUFFER_SIZE, &peerAddress);

   Common::Network::MessageHeader header = Common::Network::readHeader(buffer);

   if (header.size > datagramSize - Common::Network::HEADER_SIZE)
   {
      L_ERRO("header.size > datagramSize");
      header.setNull();
      return header;
   }

   if (header.senderID == this->peerManager->getID())
   {
      // L_WARN("We receive a datagram from ourself, skip"); // Don't care..
      header.setNull();
      return header;
   }

   if (header.type != Common::Network::CORE_IM_ALIVE)
   {
      PM::IPeer* peer = this->peerManager->getPeer(header.senderID);
      if (!peer)
      {
         L_WARN("We receive a datagram from an unknown peer, skip");
         header.setNull();
         return header;
      }

      if (!peer->isAlive())
      {
         L_WARN("We receive a datagram from a dead peer, skip");
         header.setNull();
         return header;
      }

      L_DEBU(QString("Receive a datagram UDP from %1 : header.type = %2, message size = %3").arg(peer->toStringLog()).arg(header.type, 0, 16).arg(header.size));
   }
   else
   {
      L_DEBU(QString("Receive a datagram UDP from %1 : header.type = %2, message size = %3").arg(header.senderID.toStr()).arg(header.type, 0, 16).arg(header.size));
   }
   return header;
}

