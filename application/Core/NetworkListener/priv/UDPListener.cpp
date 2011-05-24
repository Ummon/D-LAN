/**
  * D-LAN - A decentralized LAN file sharing software.
  * Copyright (C) 2010-2011 Greg Burri <greg.burri@gmail.com>
  *
  * This program is free software: you can redistribute it and/or modify
  * it under the terms of the GNU General Public License as published by
  * the Free Software Foundation, either version 3 of the License, or
  * (at your option) any later version.
  *
  * This program is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  *
  * You should have received a copy of the GNU General Public License
  * along with this program.  If not, see <http://www.gnu.org/licenses/>.
  */
  
#include <priv/UDPListener.h>
using namespace NL;

#if defined(Q_OS_LINUX)
   #include <netinet/in.h>
#elif defined(Q_OS_DARWIN)
   #include <sys/types.h>
   #include <sys/socket.h>
#elif defined(Q_OS_WIN32)
   #include <Winsock.h>
#endif

#include <google/protobuf/message.h>

#include <Common/Settings.h>
#include <Common/Global.h>
#include <Common/ProtoHelper.h>

#include <Core/PeerManager/IPeer.h>

#include <priv/Log.h>

/**
  * @class NL::UDPListener
  * @author mcuony
  * @author gburri
  */

/**
  * Initialize the socket to broadcast.
  */
UDPListener::UDPListener(
   QSharedPointer<FM::IFileManager> fileManager,
   QSharedPointer<PM::IPeerManager> peerManager,
   QSharedPointer<UM::IUploadManager> uploadManager,
   QSharedPointer<DM::IDownloadManager> downloadManager,
   quint16 unicastPort
) :
   bodyBuffer(UDPListener::buffer + Common::MessageHeader::HEADER_SIZE),
   UNICAST_PORT(unicastPort),
   MULTICAST_GROUP(SETTINGS.get<quint32>("multicast_group")),
   MULTICAST_PORT(SETTINGS.get<quint32>("multicast_port")),
   fileManager(fileManager),
   peerManager(peerManager),
   uploadManager(uploadManager),
   downloadManager(downloadManager),
   currentIMAliveTag(0),
   loggerIMAlive(LM::Builder::newLogger("NetworkListener (IMAlive)"))
{
   this->initMulticastUDPSocket();
   this->initUnicastUDPSocket();

   connect(&this->timerIMAlive, SIGNAL(timeout()), this, SLOT(sendIMAliveMessage()));
   this->timerIMAlive.start(static_cast<int>(SETTINGS.get<quint32>("peer_imalive_period")));

   this->sendIMAliveMessage();
}

void UDPListener::send(Common::MessageHeader::MessageType type, const Common::Hash& peerID, const google::protobuf::Message& message)
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

   L_DEBU(QString("Send unicast UDP to %1 : header.getType() = %2, message size = %3 \n%4").
      arg(peer->toStringLog()).
      arg(Common::MessageHeader::messToStr(type)).
      arg(messageSize).
      arg(Common::ProtoHelper::getDebugStr(message))
   );

   if (this->unicastSocket.writeDatagram(this->buffer, messageSize, peer->getIP(), peer->getPort()) == -1)
      L_WARN("Unable to send datagram");
}

/**
  * Send an UDP multicast message.
  */
void UDPListener::send(Common::MessageHeader::MessageType type, const google::protobuf::Message& message)
{
   int messageSize;
   if (!(messageSize = this->writeMessageToBuffer(type, message)))
      return;

#if DEBUG
   QString logMess = QString("Send multicast UDP : header.getType() = %1, message size = %2 \n%3").
      arg(Common::MessageHeader::messToStr(type)).
      arg(messageSize).
      arg(Common::ProtoHelper::getDebugStr(message));

   if (type == Common::MessageHeader::CORE_IM_ALIVE)
      LOG_DEBU(this->loggerIMAlive, logMess);
   else
      L_DEBU(logMess);
#endif

   if (this->multicastSocket.writeDatagram(this->buffer, messageSize, MULTICAST_GROUP, MULTICAST_PORT) == -1)
      L_WARN("Unable to send datagram");
}

void UDPListener::sendIMAliveMessage()
{
   Protos::Core::IMAlive IMAliveMessage;
   IMAliveMessage.set_version(PROTOCOL_VERSION);
   IMAliveMessage.set_port(this->UNICAST_PORT);
   Common::ProtoHelper::setStr(IMAliveMessage, &Protos::Core::IMAlive::set_nick, this->peerManager->getNick());

   IMAliveMessage.set_amount(this->fileManager->getAmount());
   IMAliveMessage.set_download_rate(this->downloadManager->getDownloadRate());
   IMAliveMessage.set_upload_rate(this->uploadManager->getUploadRate());

   this->currentIMAliveTag = this->mtrand.randInt();
   this->currentIMAliveTag <<= 32;
   this->currentIMAliveTag |= this->mtrand.randInt();
   IMAliveMessage.set_tag(this->currentIMAliveTag);

   static const quint32 NUMBER_OF_HASHES_TO_SEND = SETTINGS.get<quint32>("number_of_hashes_sent_imalive");
   this->currentChunkDownloads = this->downloadManager->getUnfinishedChunks(NUMBER_OF_HASHES_TO_SEND);
   IMAliveMessage.mutable_chunk()->Reserve(this->currentChunkDownloads.size());
   for (QListIterator< QSharedPointer<DM::IChunkDownload> > i(this->currentChunkDownloads); i.hasNext();)
   {
      IMAliveMessage.add_chunk()->set_hash(i.next()->getHash().getData(), Common::Hash::HASH_SIZE);
   }

   this->send(Common::MessageHeader::CORE_IM_ALIVE, IMAliveMessage);
}

Common::Hash UDPListener::getOwnID() const
{
   return this->peerManager->getID();
}

/**
  *
  */
void UDPListener::processPendingMulticastDatagrams()
{
   while (this->multicastSocket.hasPendingDatagrams())
   {
      QHostAddress peerAddress;
      const Common::MessageHeader& header =  UDPListener::readDatagramToBuffer(this->multicastSocket, peerAddress);
      if (header.isNull())
         continue;

      switch (header.getType())
      {
      case Common::MessageHeader::CORE_IM_ALIVE:
         {
            Protos::Core::IMAlive IMAliveMessage;
            const bool readOk = IMAliveMessage.ParseFromArray(this->bodyBuffer, header.getSize());

            if (!readOk)
            {
               L_WARN(QString("Unable to read the IMAlive message from peer %1 %2").arg(header.getSenderID().toStr()).arg(peerAddress.toString()));
               break;
            }
            else if (IMAliveMessage.version() != PROTOCOL_VERSION) // If the protocol version doesn't match we don't add the peer.
            {
               L_WARN(
                  QString("The peer %1 %2 %3 doesn't have the same protocol version (%4) as us (%5). It will be ignored.")
                     .arg(Common::ProtoHelper::getStr(IMAliveMessage, &Protos::Core::IMAlive::nick))
                     .arg(header.getSenderID().toStr())
                     .arg(peerAddress.toString())
                     .arg(IMAliveMessage.version())
                     .arg(PROTOCOL_VERSION)
               );
               break;
            }

            this->peerManager->updatePeer(
               header.getSenderID(),
               peerAddress,
               IMAliveMessage.port(),
               Common::ProtoHelper::getStr(IMAliveMessage, &Protos::Core::IMAlive::nick),
               IMAliveMessage.amount()
            );

            if (IMAliveMessage.chunk_size() > 0)
            {
               QList<Common::Hash> hashes;
               hashes.reserve(IMAliveMessage.chunk_size());
               for (int i = 0; i < IMAliveMessage.chunk_size(); i++)
                  hashes << IMAliveMessage.chunk(i).hash();

               QBitArray bitArray = this->fileManager->haveChunks(hashes);

               if (!bitArray.isNull()) // If we own at least one chunk we reply with a CHUNKS_OWNED message.
               {
                  Protos::Core::ChunksOwned chunkOwnedMessage;
                  chunkOwnedMessage.set_tag(IMAliveMessage.tag());
                  chunkOwnedMessage.mutable_chunk_state()->Reserve(bitArray.size());
                  for (int i = 0; i < bitArray.size(); i++)
                     chunkOwnedMessage.add_chunk_state(bitArray[i]);
                  this->send(Common::MessageHeader::CORE_CHUNKS_OWNED, header.getSenderID(), chunkOwnedMessage);
               }
            }
         }
         break;

      case Common::MessageHeader::CORE_CHAT_MESSAGE:
         {
            Protos::Core::ChatMessage chatMessage;
            chatMessage.ParseFromArray(this->bodyBuffer, header.getSize());
            emit newChatMessage(header.getSenderID(), chatMessage);
         }
         break;

      case Common::MessageHeader::CORE_FIND: // Find.
         {
            Protos::Core::Find findMessage;
            findMessage.ParseFromArray(this->bodyBuffer, header.getSize());

            QList<Protos::Common::FindResult> results =
               this->fileManager->find(
                  Common::ProtoHelper::getStr(findMessage, &Protos::Core::Find::pattern),
                  SETTINGS.get<quint32>("max_number_of_search_result_to_send"),
                  SETTINGS.get<quint32>("max_udp_datagram_size") - Common::MessageHeader::HEADER_SIZE
               );

            for (QMutableListIterator<Protos::Common::FindResult> i(results); i.hasNext();)
            {
               Protos::Common::FindResult& result = i.next();
               result.set_tag(findMessage.tag());
               this->send(Common::MessageHeader::CORE_FIND_RESULT, header.getSenderID(), result);
            }
         }
         break;

      default:
         L_WARN(QString("Unkown header type from multicast socket : %1").arg(header.getType(), 0, 16));
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
      const Common::MessageHeader& header =  UDPListener::readDatagramToBuffer(this->unicastSocket, peerAddress);
      if (header.isNull())
         continue;

      switch (header.getType())
      {
      case Common::MessageHeader::CORE_CHUNKS_OWNED:
         {
            Protos::Core::ChunksOwned chunksOwnedMessage;
            chunksOwnedMessage.ParseFromArray(this->bodyBuffer, header.getSize());

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
                  this->currentChunkDownloads[i]->addPeerID(header.getSenderID());
               else
                  this->currentChunkDownloads[i]->rmPeerID(header.getSenderID());
            }
         }
         break;

      case Common::MessageHeader::CORE_FIND_RESULT:
         {
            Protos::Common::FindResult findResultMessage;
            findResultMessage.ParseFromArray(this->bodyBuffer, header.getSize());
            findResultMessage.mutable_peer_id()->set_hash(header.getSenderID().getData(), Common::Hash::HASH_SIZE);
            emit newFindResultMessage(findResultMessage);
         }
         break;

      default:
         L_WARN(QString("Unkown header type from unicast socket : %1").arg(header.getType(), 0, 16));
      }
   }
}

void UDPListener::initMulticastUDPSocket()
{
   this->multicastSocket.close();
   this->multicastSocket.disconnect(this);

   if (!this->multicastSocket.bind(MULTICAST_PORT, QUdpSocket::ReuseAddressHint))
   {
      L_ERRO("Can't bind the multicast socket");
      QTimer::singleShot(SOCKET_RETRY_TIME, this, SLOT(initMulticastUDPSocket));
      return;
   }

   const int multicastSocketDescriptor = this->multicastSocket.socketDescriptor();

   // 'loop' is activated only for tests.
#if DEBUG
   const char loop = 1;
#else
   const char loop = 0;
#endif

#if defined(Q_OS_DARWIN)
   if (false) // TODO
#else
   if (setsockopt(multicastSocketDescriptor, IPPROTO_IP, IP_MULTICAST_LOOP, &loop, sizeof loop))
#endif
   {
      L_ERRO("Can't set socket option : IP_MULTICAST_LOOP");
      QTimer::singleShot(SOCKET_RETRY_TIME, this, SLOT(initMulticastUDPSocket));
      return;
   }

   const char TTL = SETTINGS.get<quint32>("multicast_ttl");

#if defined(Q_OS_DARWIN)
   if (int error = 0) // TODO
#else
   if (int error = setsockopt(multicastSocketDescriptor, IPPROTO_IP, IP_MULTICAST_TTL, &TTL, sizeof TTL))
#endif
   {
      L_ERRO(QString("Can't set socket option : IP_MULTICAST_TTL : %1").arg(error));
      QTimer::singleShot(SOCKET_RETRY_TIME, this, SLOT(initMulticastUDPSocket));
      return;
   }

#if defined(Q_OS_LINUX) || defined(Q_OS_WIN32)
   // 'htonl' reverse the order of the bytes, see : http://www.opengroup.org/onlinepubs/007908799/xns/htonl.html
   struct ip_mreq mreq;
   mreq.imr_multiaddr.s_addr = htonl(MULTICAST_GROUP.toIPv4Address());
   mreq.imr_interface.s_addr = htonl(INADDR_ANY);
   #if defined(Q_OS_LINUX)
   if (int error = setsockopt(multicastSocketDescriptor, IPPROTO_IP, IP_ADD_MEMBERSHIP, &mreq, sizeof mreq))
   #elif defined(Q_OS_WIN32)
   if (int error = setsockopt(multicastSocketDescriptor, IPPROTO_IP, IP_ADD_MEMBERSHIP, (char*)&mreq, sizeof mreq))
   #endif
#elif defined(Q_OS_DARWIN)
   if (int error = 0) // TODO
#endif
   {
      L_ERRO(QString("Can't set socket option : IP_ADD_MEMBERSHIP : %1").arg(error));
      QTimer::singleShot(SOCKET_RETRY_TIME, this, SLOT(initMulticastUDPSocket));
      return;
   }

   static const int BUFFER_SIZE_UDP = SETTINGS.get<quint32>("udp_read_buffer_size");

#if defined(Q_OS_DARWIN)
   if (int error = 0) // TODO
#else
   if (int error = setsockopt(multicastSocketDescriptor, SOL_SOCKET, SO_RCVBUF, (char*)&BUFFER_SIZE_UDP, sizeof BUFFER_SIZE_UDP))
#endif
   {
      L_ERRO(QString("Can't set socket option (multicast socket) : SO_RCVBUF : %1").arg(error));
      QTimer::singleShot(SOCKET_RETRY_TIME, this, SLOT(initMulticastUDPSocket));
      return;
   }

   connect(&this->multicastSocket, SIGNAL(readyRead()), this, SLOT(processPendingMulticastDatagrams()));
}

void UDPListener::initUnicastUDPSocket()
{
   this->unicastSocket.close();
   this->unicastSocket.disconnect(this);

   if (!this->unicastSocket.bind(UNICAST_PORT, QUdpSocket::ReuseAddressHint))
      L_ERRO("Can't bind the unicast socket");

   static const int BUFFER_SIZE_UDP = SETTINGS.get<quint32>("udp_read_buffer_size");

#if defined(Q_OS_DARWIN)
   if (int error = 0) // TODO
#else
   if (int error = setsockopt(this->unicastSocket.socketDescriptor(), SOL_SOCKET, SO_RCVBUF, (char*)&BUFFER_SIZE_UDP, sizeof BUFFER_SIZE_UDP))
#endif
      L_ERRO(QString("Can't set socket option (uncast socket) : SO_RCVBUF : %1").arg(error));

   connect(&this->unicastSocket, SIGNAL(readyRead()), this, SLOT(processPendingUnicastDatagrams()));
}

int UDPListener::writeMessageToBuffer(Common::MessageHeader::MessageType type, const google::protobuf::Message& message)
{
   const int bodySize = message.ByteSize();
   Common::MessageHeader header(type, bodySize, this->peerManager->getID());

   if (Common::MessageHeader::HEADER_SIZE + bodySize > static_cast<int>(SETTINGS.get<quint32>("max_udp_datagram_size")))
   {
      L_ERRO(QString("Datagram size too big : %1").arg(Common::MessageHeader::HEADER_SIZE + bodySize));
      return 0;
   }

   Common::MessageHeader::writeHeader(this->buffer, header);
   message.SerializeToArray(this->bodyBuffer, BUFFER_SIZE - Common::MessageHeader::HEADER_SIZE);

   return Common::MessageHeader::HEADER_SIZE + bodySize;
}

/**
  * @return A null header if error.
  */
Common::MessageHeader UDPListener::readDatagramToBuffer(QUdpSocket& socket, QHostAddress& peerAddress)
{
   qint64 datagramSize = socket.readDatagram(this->buffer, BUFFER_SIZE, &peerAddress);

   Common::MessageHeader header = Common::MessageHeader::readHeader(buffer);

   if (header.getSize() > datagramSize - Common::MessageHeader::HEADER_SIZE)
   {
      L_ERRO("header.getSize() > datagramSize");
      header.setNull();
      return header;
   }

   if (header.getSenderID() == this->peerManager->getID())
   {
      // L_WARN("We receive a datagram from ourself, skip"); // Don't care..
      header.setNull();
      return header;
   }

   if (header.getType() != Common::MessageHeader::CORE_IM_ALIVE)
   {
      PM::IPeer* peer = this->peerManager->getPeer(header.getSenderID());
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

      L_DEBU(QString("Receive a datagram UDP from %1, %2").arg(peer->toStringLog()).arg(header.toStr()));
   }
   else
   {
      L_DEBU(QString("Receive a datagram UDP from %1, %2").arg(header.getSenderID().toStr()).arg(header.toStr()));
   }
   return header;
}

