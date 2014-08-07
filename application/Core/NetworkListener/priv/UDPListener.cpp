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

#include <limits>

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
#include <Common/Constants.h>
#include <Common/Global.h>
#include <Common/ProtoHelper.h>

#include <Core/PeerManager/IPeer.h>

#include <priv/Log.h>
#include <priv/Utils.h>

/**
  * @class NL::UDPListener
  *
  * The goals of this class are:
  *  - Listen for incoming unicast and multicast datagrams, process them and dispatch the information the correct manager: 'FileManager', 'DownloadManager' or 'PeerManager'.
  *  - Offer methods to send unicast or multicast datagrams.
  *  - Periodically send a 'IMAlive' multicast datagrams.
  *
  * @author mcuony
  * @author gburri
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

/**
  * Send an UDP unicast datagram to the given peer.
  * @return 'false' if the datagram can't be sent.
  */
INetworkListener::SendStatus UDPListener::send(Common::MessageHeader::MessageType type, const google::protobuf::Message& message, const Common::Hash& peerID)
{
   PM::IPeer* peer = this->peerManager->getPeer(peerID);
   if (!peer)
      return INetworkListener::SendStatus::PEER_UNKNOWN;

   int messageSize;
   if (!(messageSize = this->writeMessageToBuffer(type, message)))
      return INetworkListener::SendStatus::MESSAGE_TOO_LARGE;

   QHostAddress peerIP = peer->getIP();

   L_DEBU(QString("Send unicast UDP to %1, header.getType(): %2, message size: %3 \n%4").
      arg(peer->toStringLog()).
      arg(Common::MessageHeader::messToStr(type)).
      arg(messageSize).
      arg(Common::ProtoHelper::getDebugStr(message))
   );

   if (this->unicastSocket.writeDatagram(this->buffer, messageSize, peer->getIP(), peer->getPort()) == -1)
   {
      L_WARN(QString("Unable to send datagram (unicast): error: %1").arg(this->unicastSocket.errorString()));
      return INetworkListener::SendStatus::UNABLE_TO_SEND;
   }

   return INetworkListener::SendStatus::OK;
}

/**
  * Send an UDP multicast message.
  */
INetworkListener::SendStatus UDPListener::send(Common::MessageHeader::MessageType type, const google::protobuf::Message& message)
{
   int messageSize;
   if (!(messageSize = this->writeMessageToBuffer(type, message)))
      return INetworkListener::SendStatus::MESSAGE_TOO_LARGE;

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
   {
      L_WARN(QString("Unable to send datagram (multicast): error: %1").arg(this->unicastSocket.errorString()));
      return INetworkListener::SendStatus::UNABLE_TO_SEND;
   }

   return INetworkListener::SendStatus::OK;
}

void UDPListener::sendIMAliveMessage()
{
   Protos::Core::IMAlive IMAliveMessage;
   IMAliveMessage.set_version(Common::Constants::PROTOCOL_VERSION);
   Common::ProtoHelper::setStr(IMAliveMessage, &Protos::Core::IMAlive::set_core_version, Common::Global::getVersionFull());
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
   static const quint32 MAX_IMALIVE_THROUGHPUT = SETTINGS.get<quint32>("max_imalive_throughput");
   static const int AVERAGE_FIXED_SIZE = 100; // [Byte]. Header size + information in the 'IMAlive' message without the hashes.
   static const quint32 IMALIVE_PERIOD = SETTINGS.get<quint32>("peer_imalive_period") / 1000; // [s]
   static const int FIXED_RATE_PER_PEER = AVERAGE_FIXED_SIZE / IMALIVE_PERIOD; // [Byte/s]
   static const int HASH_SIZE = Common::Hash::HASH_SIZE + 4; // "4" is the overhead added by protobuff for each hash.

   const int numberOfPeers = this->peerManager->getNbOfPeers();
   const int maxNumberOfHashesToSend = numberOfPeers == 0 ? std::numeric_limits<int>::max() : IMALIVE_PERIOD * (MAX_IMALIVE_THROUGHPUT - numberOfPeers * FIXED_RATE_PER_PEER) / (numberOfPeers * HASH_SIZE);

   int numberOfHashesToSend = (this->MAX_UDP_DATAGRAM_PAYLOAD_SIZE - IMAliveMessage.ByteSize() - Common::MessageHeader::HEADER_SIZE) / HASH_SIZE;
   if (numberOfHashesToSend > maxNumberOfHashesToSend)
      numberOfHashesToSend = maxNumberOfHashesToSend;

   // The requested hashes method alternates from the first hashes and the oldest hashes.
   // We are trying to have the knowledge about who has which chunk for the whole download queue (IDownloadManager::getTheOldestUnfinishedChunks(..))
   // and for the chunks we want to download first (IDownloadManager::getTheFirstUnfinishedChunks(..)).
   switch (this->nextHashRequestType)
   {
   case FIRST_HASHES:
      this->currentChunkDownloaders = this->downloadManager->getTheFirstUnfinishedChunks(numberOfHashesToSend);
      this->nextHashRequestType = OLDEST_HASHES;
      break;
   case OLDEST_HASHES:
      this->currentChunkDownloaders = this->downloadManager->getTheOldestUnfinishedChunks(numberOfHashesToSend);
      this->nextHashRequestType = FIRST_HASHES;
      break;
   }

   IMAliveMessage.mutable_chunk()->Reserve(this->currentChunkDownloaders.size());
   for (QListIterator<QSharedPointer<DM::IChunkDownloader>> i(this->currentChunkDownloaders); i.hasNext();)
   {
      QSharedPointer<DM::IChunkDownloader> chunkDownloader = i.next();
      IMAliveMessage.add_chunk()->set_hash(chunkDownloader->getHash().getData(), Common::Hash::HASH_SIZE);

      // If we already have the chunk . . .
      QSharedPointer<FM::IChunk> chunk = this->fileManager->getChunk(chunkDownloader->getHash());
      if (!chunk.isNull() && chunk->isComplete())
         chunkDownloader->addPeer(this->peerManager->getSelf());
      else
         chunkDownloader->rmPeer(this->peerManager->getSelf());
   }

   emit IMAliveMessageToBeSend(IMAliveMessage);

   this->send(Common::MessageHeader::CORE_IM_ALIVE, IMAliveMessage);

   ///// TESTS /////

   this->peerManager->updatePeer(Common::Hash("c7d4adaa63555932d3f460bde685bd93ab91dffa"), QHostAddress(), 12345, "Paul", 0, "fake", 0, 0, 4);
   this->peerManager->updatePeer(Common::Hash("3bd81861b9202ec7edcc49f46e3a3000dc04547c"), QHostAddress(), 12345, "Pierre", 0, "fake", 0, 0, 4);
}

void UDPListener::rebindSockets()
{
   this->initMulticastUDPSocket();
   this->initUnicastUDPSocket();
}

void UDPListener::processPendingMulticastDatagrams()
{
   while (this->multicastSocket.hasPendingDatagrams())
   {
      QHostAddress peerAddress;
      const Common::MessageHeader& header = this->readDatagramToBuffer(this->multicastSocket, peerAddress);
      if (header.isNull())
         continue;

      try
      {
         const Common::Message& message = Common::Message::readMessageBody(header, this->bodyBuffer);

         switch (header.getType())
         {
         case Common::MessageHeader::CORE_IM_ALIVE:
            {
               const Protos::Core::IMAlive& IMAliveMessage = message.getMessage<Protos::Core::IMAlive>();

               this->peerManager->updatePeer(
                  header.getSenderID(),
                  peerAddress,
                  IMAliveMessage.port(),
                  Common::ProtoHelper::getStr(IMAliveMessage, &Protos::Core::IMAlive::nick),
                  IMAliveMessage.amount(),
                  Common::ProtoHelper::getStr(IMAliveMessage, &Protos::Core::IMAlive::core_version),
                  IMAliveMessage.download_rate(),
                  IMAliveMessage.upload_rate(),
                  IMAliveMessage.version()
               );

               if (IMAliveMessage.chunk_size() > 0)
               {
                  QList<Common::Hash> hashes;
                  hashes.reserve(IMAliveMessage.chunk_size());
                  for (int i = 0; i < IMAliveMessage.chunk_size(); i++)
                     hashes << IMAliveMessage.chunk(i).hash();

                  const QBitArray& bitArray = this->fileManager->haveChunks(hashes);

                  if (!bitArray.isNull()) // If we own at least one chunk we reply with a CHUNKS_OWNED message.
                  {
                     Protos::Core::ChunksOwned chunkOwnedMessage;
                     chunkOwnedMessage.set_tag(IMAliveMessage.tag());
                     chunkOwnedMessage.mutable_chunk_state()->Reserve(bitArray.size());
                     for (int i = 0; i < bitArray.size(); i++)
                        chunkOwnedMessage.add_chunk_state(bitArray[i]);
                     this->send(Common::MessageHeader::CORE_CHUNKS_OWNED, chunkOwnedMessage, header.getSenderID());
                  }
               }
            }
            break;

         case Common::MessageHeader::CORE_GOODBYE:
            this->peerManager->removePeer(header.getSenderID(), peerAddress);
            break;

         case Common::MessageHeader::CORE_FIND:
            {
               PM::IPeer* peer = this->peerManager->getPeer(header.getSenderID());

               if (peer && peer->isAvailable())
               {
                  const Protos::Core::Find& findMessage = message.getMessage<Protos::Core::Find>();
                  QList<QString> extensions;
                  extensions.reserve(findMessage.pattern().extension_filter_size());
                  for (int i = 0; i < findMessage.pattern().extension_filter_size(); i++)
                     extensions << Common::ProtoHelper::getRepeatedStr(findMessage.pattern(), &Protos::Common::FindPattern::extension_filter, i);

                  QList<Protos::Common::FindResult> results =
                     this->fileManager->find(
                        Common::ProtoHelper::getStr(findMessage.pattern(), &Protos::Common::FindPattern::pattern),
                        extensions,
                        findMessage.pattern().min_size() == 0 ? std::numeric_limits<qint64>::min() : (qint64)findMessage.pattern().min_size(), // According the protocol.
                        findMessage.pattern().max_size() == 0 ? std::numeric_limits<qint64>::max() : (qint64)findMessage.pattern().max_size(), // According the protocol.
                        findMessage.pattern().category(),
                        SETTINGS.get<quint32>("max_number_of_search_result_to_send"),
                        this->MAX_UDP_DATAGRAM_PAYLOAD_SIZE - Common::MessageHeader::HEADER_SIZE
                     );

                  for (QMutableListIterator<Protos::Common::FindResult> i(results); i.hasNext();)
                  {
                     Protos::Common::FindResult& result = i.next();
                     result.set_tag(findMessage.tag());
                     this->send(Common::MessageHeader::CORE_FIND_RESULT, result, header.getSenderID());
                  }
               }
            }
            break;

         default:; // Ignore other messages.
         }

         emit received(message);
      }
      catch(Common::ReadErrorException&)
      {
         L_WARN(QString("Unable to read a multicast message from peer %1 %2").arg(header.getSenderID().toStr()).arg(peerAddress.toString()));
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

      try
      {
         const Common::Message& message = Common::Message::readMessageBody(header, this->bodyBuffer);
         PM::IPeer* peer = this->peerManager->getPeer(header.getSenderID());
         if (!peer || !peer->isAvailable())
            continue;

         switch (header.getType())
         {
         case Common::MessageHeader::CORE_CHUNKS_OWNED:
            {
               const Protos::Core::ChunksOwned& chunksOwnedMessage = message.getMessage<Protos::Core::ChunksOwned>();

               if (chunksOwnedMessage.tag() != this->currentIMAliveTag)
               {
                  L_WARN(QString("ChunksOwned : tag (%1) doesn't match current tag (%2)").arg(chunksOwnedMessage.tag()).arg(currentIMAliveTag));
                  continue;
               }

               if (chunksOwnedMessage.chunk_state_size() != this->currentChunkDownloaders.size())
               {
                  L_WARN(QString("ChunksOwned : The size (%1) doesn't match the expected one (%2)").arg(chunksOwnedMessage.chunk_state_size()).arg(this->currentChunkDownloaders.size()));
                  continue;
               }

               for (int i = 0; i < chunksOwnedMessage.chunk_state_size(); i++)
                  if (chunksOwnedMessage.chunk_state(i))
                     this->currentChunkDownloaders[i]->addPeer(peer);
                  else
                     this->currentChunkDownloaders[i]->rmPeer(peer);
            }
            break;

         case Common::MessageHeader::CORE_FIND_RESULT:
            {
               Protos::Common::FindResult findResultMessage = message.getMessage<Protos::Common::FindResult>();
               findResultMessage.mutable_peer_id()->set_hash(header.getSenderID().getData(), Common::Hash::HASH_SIZE);
               emit newFindResultMessage(findResultMessage);
            }
            break;

         default:; // Ignore other messages.
         }

         emit received(message);
      }
      catch(Common::ReadErrorException&)
      {
         L_WARN(QString("Unable to read an unicast message from peer %1 %2").arg(header.getSenderID().toStr()).arg(peerAddress.toString()));
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

   QNetworkInterface networkInterface = Utils::getCurrentInterfaceToListenTo();
   if (networkInterface.isValid() ? !this->multicastSocket.joinMulticastGroup(this->multicastGroup, networkInterface) : !this->multicastSocket.joinMulticastGroup(this->multicastGroup))
      L_ERRO(QString("Unable to join the multicast group: %1 on the interface: %2").arg(this->multicastGroup.toString()).arg(networkInterface.name()));

   static const int BUFFER_SIZE_UDP = SETTINGS.get<quint32>("udp_buffer_size");
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

#if defined(Q_OS_DARWIN)
   if (int error = 0) // TODO: Mac OS X
#else
   if (int error = setsockopt(multicastSocketDescriptor, SOL_SOCKET, SO_SNDBUF, (char*)&BUFFER_SIZE_UDP, sizeof BUFFER_SIZE_UDP))
#endif
   {
      L_ERRO(QString("Can't set socket option (multicast socket) : SO_SNDBUF : %1").arg(error));
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

   static const int BUFFER_SIZE_UDP = SETTINGS.get<quint32>("udp_buffer_size");

#if defined(Q_OS_DARWIN)
   if (int error = 0) // TODO
#else
   if (int error = setsockopt(this->unicastSocket.socketDescriptor(), SOL_SOCKET, SO_RCVBUF, (char*)&BUFFER_SIZE_UDP, sizeof BUFFER_SIZE_UDP))
#endif
      L_ERRO(QString("Can't set socket option (unicast socket) : SO_RCVBUF : %1").arg(error));

#if defined(Q_OS_DARWIN)
   if (int error = 0) // TODO
#else
   if (int error = setsockopt(this->unicastSocket.socketDescriptor(), SOL_SOCKET, SO_SNDBUF, (char*)&BUFFER_SIZE_UDP, sizeof BUFFER_SIZE_UDP))
#endif
      L_ERRO(QString("Can't set socket option (unicast socket) : SO_SNDBUF : %1").arg(error));

   connect(&this->unicastSocket, SIGNAL(readyRead()), this, SLOT(processPendingUnicastDatagrams()));
}

/**
  * Writes a given protobuff message to the buffer (this->buffer) prefixed by a header.
  * @return the total size (header size + message size). Return 0 if the total size is bigger than 'Protos.Core.Settings.max_udp_datagram_size'.
  */
int UDPListener::writeMessageToBuffer(Common::MessageHeader::MessageType type, const google::protobuf::Message& message)
{
   const Common::MessageHeader header(type, message.ByteSize(), this->getOwnID());

   const int nbBytesWritten = Common::Message::writeMessageToBuffer(this->buffer, this->MAX_UDP_DATAGRAM_PAYLOAD_SIZE, header, &message);
   if (!nbBytesWritten)
      L_ERRO(QString("Datagram size too big: %1, max allowed: %2").arg(Common::MessageHeader::HEADER_SIZE + header.getSize()).arg(this->MAX_UDP_DATAGRAM_PAYLOAD_SIZE));

   return nbBytesWritten;
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
      // L_WARN("We receive a datagram from ourself, skip"); // Don't care . . .
      header.setNull();
      return header;
   }

   if (header.getType() != Common::MessageHeader::CORE_IM_ALIVE)
   {
      PM::IPeer* peer = this->peerManager->getPeer(header.getSenderID());
      if (!peer)
      {
          L_WARN(QString("We receive a datagram from an unknown peer (%1), skip").arg(peerAddress.toString()));
         header.setNull();
         return header;
      }

      if (!peer->isAlive())
      {
          L_WARN(QString("We receive a datagram from a dead peer (%1), skip").arg(peerAddress.toString()));
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

Common::Hash UDPListener::getOwnID() const
{
   return this->peerManager->getSelf()->getID();
}
