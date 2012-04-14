/**
  * D-LAN - A decentralized LAN file sharing software.
  * Copyright (C) 2010-2012 Greg Burri <greg.burri@gmail.com>
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
#include <priv/Utils.h>

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
   MAX_UDP_DATAGRAM_PAYLOAD_SIZE(static_cast<int>(SETTINGS.get<quint32>("max_udp_datagram_size"))),
   bodyBuffer(UDPListener::buffer + Common::MessageHeader::HEADER_SIZE),
   UNICAST_PORT(unicastPort),
   MULTICAST_PORT(SETTINGS.get<quint32>("multicast_port")),
   multicastGroup(Utils::getMulticastGroup()),
   fileManager(fileManager),
   peerManager(peerManager),
   uploadManager(uploadManager),
   downloadManager(downloadManager),
   currentIMAliveTag(0),
   nextHashRequestType(FIRST_HASHES),
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
      return;

   int messageSize;
   if (!(messageSize = this->writeMessageToBuffer(type, message)))
      return;

   L_DEBU(QString("Send unicast UDP to %1, header.getType(): %2, message size: %3 \n%4").
      arg(peer->toStringLog()).
      arg(Common::MessageHeader::messToStr(type)).
      arg(messageSize).
      arg(Common::ProtoHelper::getDebugStr(message))
   );

   if (this->unicastSocket.writeDatagram(this->buffer, messageSize, peer->getIP(), peer->getPort()) == -1)
      L_WARN(QString("Unable to send datagram (unicast): error: %1").arg(this->unicastSocket.errorString()));
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

   if (this->multicastSocket.writeDatagram(this->buffer, messageSize, this->multicastGroup, MULTICAST_PORT) == -1)
      L_WARN(QString("Unable to send datagram (multicast): error: %1").arg(this->unicastSocket.errorString()));
}

void UDPListener::sendIMAliveMessage()
{
   Protos::Core::IMAlive IMAliveMessage;
   IMAliveMessage.set_version(PROTOCOL_VERSION);
   ProtoHelper::setStr(IMAliveMessage, &Protos::Core::IMAlive::set_core_version, Common::Global::getVersionFull());
   IMAliveMessage.set_port(this->UNICAST_PORT);

   const QString& nick = this->peerManager->getSelf()->getNick();
   Common::ProtoHelper::setStr(IMAliveMessage, &Protos::Core::IMAlive::set_nick, nick.length() > MAX_NICK_LENGTH ? nick.left(MAX_NICK_LENGTH) : nick);

   IMAliveMessage.set_amount(this->fileManager->getAmount());
   IMAliveMessage.set_download_rate(this->downloadManager->getDownloadRate());
   IMAliveMessage.set_upload_rate(this->uploadManager->getUploadRate());

   this->currentIMAliveTag = this->mtrand.randInt();
   this->currentIMAliveTag <<= 32;
   this->currentIMAliveTag |= this->mtrand.randInt();
   IMAliveMessage.set_tag(this->currentIMAliveTag);

   // We fill the rest of the message with a maximum of needed hashes.
   const int numberOfHashesToSend = (this->MAX_UDP_DATAGRAM_PAYLOAD_SIZE - IMAliveMessage.ByteSize() - Common::MessageHeader::HEADER_SIZE) / (Common::Hash::HASH_SIZE + 4); // "4" is the overhead added by protobuff for each hash.

   // The requested hashes method alternates from the first hashes and the oldest hashes.
   // We are trying to have the knowledge about who has which chunk for the whole download queue (IDownloadManager::getTheOldestUnfinishedChunks(..))
   // and for the chunks we want to download first (IDownloadManager::getTheFirstUnfinishedChunks(..)).
   switch (this->nextHashRequestType)
   {
   case FIRST_HASHES:
      this->currentChunkDownloads = this->downloadManager->getTheFirstUnfinishedChunks(numberOfHashesToSend);
      this->nextHashRequestType = OLDEST_HASHES;
      break;
   case OLDEST_HASHES:
      this->currentChunkDownloads = this->downloadManager->getTheOldestUnfinishedChunks(numberOfHashesToSend);
      this->nextHashRequestType = FIRST_HASHES;
      break;
   }

   IMAliveMessage.mutable_chunk()->Reserve(this->currentChunkDownloads.size());
   for (QListIterator< QSharedPointer<DM::IChunkDownload> > i(this->currentChunkDownloads); i.hasNext();)
   {
      QSharedPointer<DM::IChunkDownload> chunkDownload = i.next();
      IMAliveMessage.add_chunk()->set_hash(chunkDownload->getHash().getData(), Common::Hash::HASH_SIZE);

      // If we already have the chunk...
      QSharedPointer<FM::IChunk> chunk = this->fileManager->getChunk(chunkDownload->getHash());
      if (!chunk.isNull() && chunk->isComplete())
         chunkDownload->addPeer(this->peerManager->getSelf());
      else
         chunkDownload->rmPeer(this->peerManager->getSelf());
   }

   this->send(Common::MessageHeader::CORE_IM_ALIVE, IMAliveMessage);
}

Common::Hash UDPListener::getOwnID() const
{
   return this->peerManager->getSelf()->getID();
}

void UDPListener::rebindSockets()
{
   this->initMulticastUDPSocket();
   this->initUnicastUDPSocket();
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
               IMAliveMessage.amount(),
               Common::ProtoHelper::getStr(IMAliveMessage, &Protos::Core::IMAlive::core_version)
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

      case Common::MessageHeader::CORE_FIND:
         {
            Protos::Core::Find findMessage;
            findMessage.ParseFromArray(this->bodyBuffer, header.getSize());

            QList<Protos::Common::FindResult> results =
               this->fileManager->find(
                  Common::ProtoHelper::getStr(findMessage, &Protos::Core::Find::pattern),
                  SETTINGS.get<quint32>("max_number_of_search_result_to_send"),
                  this->MAX_UDP_DATAGRAM_PAYLOAD_SIZE - Common::MessageHeader::HEADER_SIZE
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
               if (PM::IPeer* peer = this->peerManager->getPeer(header.getSenderID()))
               {
                  if (chunksOwnedMessage.chunk_state(i))
                     this->currentChunkDownloads[i]->addPeer(peer);
                  else
                     this->currentChunkDownloads[i]->rmPeer(peer);
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

   this->multicastGroup = Utils::getMulticastGroup();

   if (!this->multicastSocket.bind(Utils::getCurrentAddressToListenTo(), MULTICAST_PORT))
   {
      L_ERRO("Can't bind the multicast socket");
      return;
   }

   // 'loop' is activated only for tests.
#if DEBUG
   const char loop = 1;
#else
   const char loop = 0;
#endif
   this->multicastSocket.setSocketOption(QAbstractSocket::MulticastLoopbackOption, loop);

   this->multicastSocket.setSocketOption(QAbstractSocket::MulticastTtlOption, SETTINGS.get<quint32>("multicast_ttl"));

   if (!this->multicastSocket.joinMulticastGroup(this->multicastGroup))
      L_ERRO(QString("Unable to join the multicast group: %1").arg(this->multicastGroup.toString()));

   static const int BUFFER_SIZE_UDP = SETTINGS.get<quint32>("udp_read_buffer_size");
   const int multicastSocketDescriptor = this->multicastSocket.socketDescriptor();
#if defined(Q_OS_DARWIN)
   if (int error = 0) // TODO: Mac OS X
#else
   if (int error = setsockopt(multicastSocketDescriptor, SOL_SOCKET, SO_RCVBUF, (char*)&BUFFER_SIZE_UDP, sizeof BUFFER_SIZE_UDP))
#endif
   {
      L_ERRO(QString("Can't set socket option (multicast socket) : SO_RCVBUF : %1").arg(error));
      return;
   }

   connect(&this->multicastSocket, SIGNAL(readyRead()), this, SLOT(processPendingMulticastDatagrams()));
}

void UDPListener::initUnicastUDPSocket()
{
   this->unicastSocket.close();
   this->unicastSocket.disconnect(this);

   if (!this->unicastSocket.bind(Utils::getCurrentAddressToListenTo(), UNICAST_PORT, QUdpSocket::ReuseAddressHint))
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

/**
  * Writes a given protobuff message to the buffer (this->buffer) prefixed by a header.
  * @return the total size (header size + message size). Return 0 if the total size is bigger than 'Protos.Core.Settings.max_udp_datagram_size'.
  */
int UDPListener::writeMessageToBuffer(Common::MessageHeader::MessageType type, const google::protobuf::Message& message)
{
   const int bodySize = message.ByteSize();
   const Common::MessageHeader header(type, bodySize, this->peerManager->getSelf()->getID());

   if (Common::MessageHeader::HEADER_SIZE + bodySize > this->MAX_UDP_DATAGRAM_PAYLOAD_SIZE)
   {
      L_ERRO(QString("Datagram size too big: %1, max allowed: %2").arg(Common::MessageHeader::HEADER_SIZE + bodySize).arg(this->MAX_UDP_DATAGRAM_PAYLOAD_SIZE));
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
   quint16 port;
   const qint64 datagramSize = socket.readDatagram(this->buffer, BUFFER_SIZE, &peerAddress, &port);
   if (datagramSize == -1)
   {
      L_WARN(QString("UDPListener::readDatagramToBuffer(..): Unable to read multicast datagram from address:port: %1:%2").arg(peerAddress.toString()).arg(port));
      return Common::MessageHeader();
   }

   Common::MessageHeader header = Common::MessageHeader::readHeader(buffer);

   if (header.getSize() > datagramSize - Common::MessageHeader::HEADER_SIZE)
   {
      L_ERRO("The message size (header.size) exceeds the datagram size received");
      header.setNull();
      return header;
   }

   if (header.getSenderID() == this->peerManager->getSelf()->getID())
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

