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
#include <algorithm>

#include <QRandomGenerator64>

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
  *  - Listen for incoming unicast and multicast datagrams,
  *    process them and dispatch the information the correct manager: 'FileManager', 'DownloadManager' or 'PeerManager'.
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
   QSharedPointer<DM::IDownloadManager> downloadManager
) :
   MAX_UDP_DATAGRAM_PAYLOAD_SIZE(UDPListener::getMaxUDPDatagramPayloadSize()),
   bodyBuffer(UDPListener::buffer + Common::MessageHeader::HEADER_SIZE),
   MULTICAST_PORT(SETTINGS.get<quint32>("multicast_port")),
   fileManager(fileManager),
   peerManager(peerManager),
   uploadManager(uploadManager),
   downloadManager(downloadManager),
   maxNbChunksNeeded(0),
   nextHashRequestType(FIRST_HASHES),
   loggerChunksNeeded(LM::Builder::newLogger("NetworkListener (chunkNeeded)"))
{
   connect(&this->multicastSocket, &QUdpSocket::readyRead, this, &UDPListener::processPendingMulticastDatagrams);
   connect(&this->unicastSocket, &QUdpSocket::readyRead, this, &UDPListener::processPendingUnicastDatagrams);

   connect(&this->timerIMAlive, &QTimer::timeout, this, &UDPListener::sendIMAliveMessage);
   this->timerInitialIMAlive.setSingleShot(true);
   connect(&this->timerInitialIMAlive, &QTimer::timeout, this, &UDPListener::sendIMAliveMessage);

   connect(&this->timerChunksNeeded, &QTimer::timeout, this, &UDPListener::sendChunksNeededMessage);
}

/**
  * Send an UDP unicast datagram to the given peer.
  */
INetworkListener::SendStatus UDPListener::send(
   Common::MessageHeader::MessageType type,
   const google::protobuf::Message& message,
   const Common::Hash& peerID
)
{
   PM::IPeer* peer = this->peerManager->getPeer(peerID);
   if (!peer)
      return INetworkListener::SendStatus::PEER_UNKNOWN;

   return this->send(type, message, *peer);
}

/**
  * Send an UDP unicast datagram to a peer already looked up.
  */
INetworkListener::SendStatus UDPListener::send(
   Common::MessageHeader::MessageType type,
   const google::protobuf::Message& message, PM::IPeer& peer
)
{
   int messageSize;
   if (!(messageSize = this->writeMessageToBuffer(type, message)))
      return INetworkListener::SendStatus::MESSAGE_TOO_LARGE;

   if (!this->isListening())
      return INetworkListener::SendStatus::UNABLE_TO_SEND;

   L_DEBU(QString("Send unicast UDP to %1, header.getType(): %2, message size: %3 \n%4").
      arg(peer.toStringLog(), Common::MessageHeader::messToStr(type)).
      arg(messageSize).
      arg(Common::ProtoHelper::getDebugStr(message))
   );

   if (this->unicastSocket.writeDatagram(this->buffer, messageSize, peer.getIP(), peer.getPort()) == -1)
   {
      L_WARN(QString("Unable to send datagram (unicast): error: %1").arg(this->unicastSocket.errorString()));
      return INetworkListener::SendStatus::UNABLE_TO_SEND;
   }

   return INetworkListener::SendStatus::OK;
}

/**
  * Send an UDP multicast message.
  */
INetworkListener::SendStatus UDPListener::send(
   Common::MessageHeader::MessageType type,
   const google::protobuf::Message& message
)
{
   int messageSize;
   if (!(messageSize = this->writeMessageToBuffer(type, message)))
      return INetworkListener::SendStatus::MESSAGE_TOO_LARGE;

   if (!this->isListening())
      return INetworkListener::SendStatus::UNABLE_TO_SEND;

#if DEBUG
   QString logMess = QString("Send multicast UDP: header.getType() = %1, message size = %2 \n%3").
      arg(Common::MessageHeader::messToStr(type)).
      arg(messageSize).
      arg(Common::ProtoHelper::getDebugStr(message));

   if (type == Common::MessageHeader::CORE_CHUNKS_NEEDED)
      LOG_DEBU(this->loggerChunksNeeded, logMess);
   else
      L_DEBU(logMess);
#endif

   bool sent = false;
   for (const auto& networkInterface : this->multicastInterfaces)
   {
      this->multicastSocket.setMulticastInterface(networkInterface);
      // setMulticastInterface() has no return value. Do not send on the previous
      // adapter if selecting this one failed (for example, after unplugging it).
      if (this->multicastSocket.multicastInterface().index() != networkInterface.index() ||
          this->multicastSocket.writeDatagram(this->buffer, messageSize, this->multicastGroup, MULTICAST_PORT) == -1)
         L_WARN(QString("Unable to send datagram (multicast) on %1: %2")
            .arg(networkInterface.humanReadableName(), this->multicastSocket.errorString()));
      else
         sent = true;
   }

   // A failing adapter must not prevent searches and discovery on working ones.
   return sent ? INetworkListener::SendStatus::OK : INetworkListener::SendStatus::UNABLE_TO_SEND;
}

void UDPListener::sendIMAliveMessage()
{
   if (!this->isListening())
      return;

   Protos::Core::IMAlive IMAliveMessage;
   IMAliveMessage.set_version(Common::Constants::PROTOCOL_VERSION);
   IMAliveMessage.set_core_version(Common::Global::getVersionFull().toStdString());
   IMAliveMessage.set_port(this->unicastSocket.localPort());

   const QString& nick = this->peerManager->getSelf()->getNick();
   IMAliveMessage.set_nick((nick.length() > MAX_NICK_LENGTH ? nick.left(MAX_NICK_LENGTH) : nick).toStdString());

   IMAliveMessage.set_amount(this->fileManager->getAmount());
   IMAliveMessage.set_download_rate(this->downloadManager->getDownloadRate());
   IMAliveMessage.set_upload_rate(this->uploadManager->getUploadRate());

   // Add chat rooms by the ChatSystem component.
   emit IMAliveMessageToBeSend(IMAliveMessage);

   // Avoid exceeding the datagram payload maximum size.
   const int numberOfRooms = IMAliveMessage.chat_rooms_size();
   while (IMAliveMessage.chat_rooms_size() > 0 &&
          static_cast<qint64>(IMAliveMessage.ByteSizeLong()) +
          Common::MessageHeader::HEADER_SIZE > this->MAX_UDP_DATAGRAM_PAYLOAD_SIZE
   )
      IMAliveMessage.mutable_chat_rooms()->RemoveLast();

   if (IMAliveMessage.chat_rooms_size() != numberOfRooms)
      L_WARN(QString("IMAlive: %1 chat room announcements omitted to not exceed datagram size").arg(numberOfRooms - IMAliveMessage.chat_rooms_size()));

   this->send(Common::MessageHeader::CORE_IM_ALIVE, IMAliveMessage);
}

void UDPListener::sendChunksNeededMessage()
{
   if (!this->isListening())
      return;

   ChunksNeededRequest request;
   request.tag = QRandomGenerator64::global()->generate64();

   // The requested hashes method alternates from the first hashes and the oldest hashes.
   // We are trying to have the knowledge about who has which chunk for the whole download queue (IDownloadManager::getTheOldestUnfinishedChunks(..))
   // and for the chunks we want to download first (IDownloadManager::getTheFirstUnfinishedChunks(..)).
   switch (this->nextHashRequestType)
   {
   case FIRST_HASHES:
      request.chunkDownloaders = this->downloadManager->getTheFirstUnfinishedChunks(this->maxNbChunksNeeded);
      this->nextHashRequestType = OLDEST_HASHES;
      break;
   case OLDEST_HASHES:
      request.chunkDownloaders = this->downloadManager->getTheOldestUnfinishedChunks(this->maxNbChunksNeeded);
      this->nextHashRequestType = FIRST_HASHES;
      break;
   }

   if (request.chunkDownloaders.isEmpty())
      return;

   Protos::Core::ChunksNeeded chunkNeededMessage;
   chunkNeededMessage.set_tag(request.tag);
   chunkNeededMessage.mutable_chunks()->Reserve(request.chunkDownloaders.size());
   for (const QSharedPointer<DM::IChunkDownloader>& chunkDownloader : std::as_const(request.chunkDownloaders))
   {
      chunkNeededMessage.add_chunks()->set_hash(chunkDownloader->getHash().getData(), Common::Hash::HASH_SIZE);

      // Reuse complete local chunks, including those in other unfinished files.
      const QSharedPointer<FM::IChunk> chunk = this->fileManager->getChunk(chunkDownloader->getHash());
      if (!chunk.isNull() && chunk->isComplete())
         chunkDownloader->addPeer(this->peerManager->getSelf());
      else
         chunkDownloader->rmPeer(this->peerManager->getSelf());
   }

   while (this->chunksNeededRequests.size() >= NB_CHUNKS_NEEDED_REQUESTS_KEPT)
      this->chunksNeededRequests.removeFirst();
   this->chunksNeededRequests << request;

   this->send(Common::MessageHeader::CORE_CHUNKS_NEEDED, chunkNeededMessage);
}

void UDPListener::closeSockets()
{
   this->timerIMAlive.stop();
   this->timerInitialIMAlive.stop();
   this->timerChunksNeeded.stop();
   this->multicastSocket.close();
   this->multicastInterfaces.clear();
   this->unicastSocket.close();
   this->recentMulticastDatagrams.clear();
   this->chunksNeededRequests.clear();
}

bool UDPListener::startListening(const QList<QNetworkInterface>& interfaces, bool logFailures)
{
   if (this->unicastSocket.localPort() == 0 || !this->initMulticastUDPSocket(interfaces, logFailures))
   {
      this->closeSockets();
      return false;
   }

   this->timerIMAlive.start(Common::Constants::PEER_IMALIVE_PERIOD);
   // Defer the first heartbeat so callers can connect their message contributors.
   this->timerInitialIMAlive.start(0);

   // A 'ChunksNeeded' datagram mustn't be fragmented.
   static const int HASH_SIZE = Common::Hash::HASH_SIZE + 4; // "4" is the overhead added by protobuf for each hash.
   static const int MAX_TAG_SIZE = 11;
   const int maxDatagramSize = qMin(
      Utils::getMaxUnfragmentedDatagramSize(this->multicastInterfaces, this->multicastGroup.protocol()),
      this->MAX_UDP_DATAGRAM_PAYLOAD_SIZE
   );
   this->maxNbChunksNeeded = (maxDatagramSize - Common::MessageHeader::HEADER_SIZE - MAX_TAG_SIZE) / HASH_SIZE;
   this->timerChunksNeeded.start(Common::Constants::CHUNKS_NEEDED_PERIOD);
   return true;
}

void UDPListener::processPendingMulticastDatagrams()
{
   while (this->multicastSocket.hasPendingDatagrams())
   {
      QHostAddress peerAddress;
      const Common::MessageHeader header = this->readDatagramToBuffer(this->multicastSocket, peerAddress);
      if (header.isNull() || this->isDuplicate(header))
         continue;

      // Discovery comes from any peer and departure from an alive peer, even if it is unavailable.
      // All other multicast traffic follows the same availability policy as unicast.
      PM::IPeer* peer = nullptr;
      if (header.getType() != Common::MessageHeader::CORE_IM_ALIVE &&
          !(peer = this->getSender(header, peerAddress, header.getType() != Common::MessageHeader::CORE_GOODBYE)))
         continue;

      try
      {
         const Common::Message& message = Common::Message::readMessageBody(header, this->bodyBuffer);

         switch (header.getType())
         {
         case Common::MessageHeader::CORE_IM_ALIVE:
            {
               const Protos::Core::IMAlive& IMAliveMessage = message.getMessage<Protos::Core::IMAlive>();

               // The port is an 'uint32' in the protocol, it would be truncated to a wrong port.
               if (IMAliveMessage.port() == 0 || IMAliveMessage.port() > std::numeric_limits<quint16>::max())
               {
                  L_WARN(
                     QString("IMAlive: invalid port (%1) from peer %2 %3, message ignored")
                        .arg(IMAliveMessage.port()).arg(header.getSenderID().toStrShort(), peerAddress.toString())
                  );
                  continue;
               }

               this->peerManager->updatePeer(
                  header.getSenderID(),
                  peerAddress,
                  IMAliveMessage.port(),
                  QString::fromStdString(IMAliveMessage.nick()),
                  IMAliveMessage.amount(),
                  QString::fromStdString(IMAliveMessage.core_version()),
                  IMAliveMessage.download_rate(),
                  IMAliveMessage.upload_rate(),
                  IMAliveMessage.version()
               );
            }
            break;

         case Common::MessageHeader::CORE_CHUNKS_NEEDED:
            {
               const Protos::Core::ChunksNeeded& chunksNeededMessage = message.getMessage<Protos::Core::ChunksNeeded>();

               if (peer && peer->isAvailable() && chunksNeededMessage.chunks_size() > 0)
               {
                  QList<Common::Hash> hashes;
                  hashes.reserve(chunksNeededMessage.chunks_size());
                  for (int i = 0; i < chunksNeededMessage.chunks_size(); i++)
                     hashes << chunksNeededMessage.chunks(i).hash();

                  const QBitArray& bitArray = this->fileManager->haveChunks(hashes);

                  if (!bitArray.isNull()) // If we own at least one chunk we reply with a CHUNKS_OWNED message.
                  {
                     Protos::Core::ChunksOwned chunkOwnedMessage;
                     chunkOwnedMessage.set_tag(chunksNeededMessage.tag());
                     chunkOwnedMessage.mutable_chunk_state()->Reserve(bitArray.size());
                     for (int i = 0; i < bitArray.size(); i++)
                        chunkOwnedMessage.add_chunk_state(bitArray[i]);
                     this->send(Common::MessageHeader::CORE_CHUNKS_OWNED, chunkOwnedMessage, *peer);
                  }
               }
            }
            break;

         case Common::MessageHeader::CORE_GOODBYE:
            this->peerManager->removePeer(header.getSenderID(), peerAddress);
            break;

         case Common::MessageHeader::CORE_FIND:
            {
               static const quint32 MAX_NUMBER_OF_SEARCH_RESULT_TO_SEND =
                  SETTINGS.get<quint32>("max_number_of_search_result_to_send");

               const Protos::Core::Find& findMessage = message.getMessage<Protos::Core::Find>();
               QList<QString> extensions;
               extensions.reserve(findMessage.pattern().extension_filters_size());
               for (int i = 0; i < findMessage.pattern().extension_filters_size(); i++)
                  extensions << QString::fromStdString(findMessage.pattern().extension_filters(i));

               QList<Protos::Common::FindResult> results =
                  this->fileManager->find(
                     QString::fromStdString(findMessage.pattern().pattern()),
                     extensions,
                     findMessage.pattern().min_size() == 0 ? std::numeric_limits<qint64>::min() : (qint64)findMessage.pattern().min_size(), // According the protocol.
                     findMessage.pattern().max_size() == 0 ? std::numeric_limits<qint64>::max() : (qint64)findMessage.pattern().max_size(), // According the protocol.
                     findMessage.pattern().category(),
                     MAX_NUMBER_OF_SEARCH_RESULT_TO_SEND,
                     this->MAX_UDP_DATAGRAM_PAYLOAD_SIZE - Common::MessageHeader::HEADER_SIZE,
                     false
                  );

               for (Protos::Common::FindResult& result : results)
               {
                  result.set_tag(findMessage.tag());
                  this->send(Common::MessageHeader::CORE_FIND_RESULT, result, *peer);
               }
            }
            break;

         default:; // Ignore other messages.
         }

         emit received(message);
      }
      catch (Common::ReadErrorException&)
      {
         L_WARN(
            QString("Unable to read a multicast message from peer %1 %2")
               .arg(header.getSenderID().toStrShort(), peerAddress.toString())
         );
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
      const Common::MessageHeader header = this->readDatagramToBuffer(this->unicastSocket, peerAddress);
      if (header.isNull())
         continue;

      // Checked before parsing: datagrams from unavailable peers are not worth decoding.
      PM::IPeer* peer = this->getSender(header, peerAddress, true);
      if (!peer)
         continue;

      try
      {
         const Common::Message& message = Common::Message::readMessageBody(header, this->bodyBuffer);

         switch (header.getType())
         {
         case Common::MessageHeader::CORE_CHUNKS_OWNED:
            {
               const Protos::Core::ChunksOwned& chunksOwnedMessage = message.getMessage<Protos::Core::ChunksOwned>();

               const auto request = std::find_if(
                  this->chunksNeededRequests.cbegin(),
                  this->chunksNeededRequests.cend(),
                  [&](const ChunksNeededRequest& sent) { return sent.tag == chunksOwnedMessage.tag(); }
               );

               if (request == this->chunksNeededRequests.cend())
               {
                  L_WARN(
                     QString("ChunksOwned: tag (%1) doesn't match any of the last %2 'ChunksNeeded' messages")
                        .arg(chunksOwnedMessage.tag()).arg(NB_CHUNKS_NEEDED_REQUESTS_KEPT)
                  );
                  continue;
               }

               if (chunksOwnedMessage.chunk_state_size() != request->chunkDownloaders.size())
               {
                  L_WARN(
                     QString("ChunksOwned: The size (%1) doesn't match the expected one (%2)")
                        .arg(chunksOwnedMessage.chunk_state_size()).arg(request->chunkDownloaders.size())
                  );
                  continue;
               }

               for (int i = 0; i < chunksOwnedMessage.chunk_state_size(); i++)
                  if (chunksOwnedMessage.chunk_state(i))
                     request->chunkDownloaders[i]->addPeer(peer);
                  else
                     request->chunkDownloaders[i]->rmPeer(peer);
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
      catch (Common::ReadErrorException&)
      {
         L_WARN(
            QString("Unable to read an unicast message from peer %1 %2")
               .arg(header.getSenderID().toStrShort(), peerAddress.toString())
         );
      }
   }
}

bool UDPListener::initMulticastUDPSocket(const QList<QNetworkInterface>& interfaces, bool logFailures)
{
   this->multicastSocket.close();
   this->multicastInterfaces.clear();

   // Same protocol as the unicast socket, rather than a new lookup which may see other adapters.
   const auto protocol = this->unicastSocket.localAddress().protocol();

   this->multicastGroup = Utils::getMulticastGroup(protocol);

   if (
      !this->multicastSocket.bind(
         // Always bind to any, it seems there are some issue on Windows to bind multicast address to specific interface.
         protocol == QAbstractSocket::IPv4Protocol ?
              QHostAddress::AnyIPv4
            : QHostAddress::AnyIPv6,
         MULTICAST_PORT,
         QUdpSocket::ShareAddress | QUdpSocket::ReuseAddressHint
      )
   )
   {
      if (logFailures)
         L_ERRO(QString("Can't bind the multicast socket: %1").arg(this->multicastSocket.errorString()));
      return false;
   }

   // 'loop' is activated only for tests.
#if DEBUG
   const bool loop = true;
#else
   const bool loop = false;
#endif

   this->multicastSocket.setSocketOption(QAbstractSocket::MulticastLoopbackOption, loop);
   this->multicastSocket.setSocketOption(QAbstractSocket::MulticastTtlOption, SETTINGS.get<quint32>("multicast_ttl"));

   for (const auto& networkInterface : interfaces)
   {
      if (!this->multicastSocket.joinMulticastGroup(this->multicastGroup, networkInterface))
      {
         if (logFailures)
            L_WARN(QString("Unable to join multicast group %1 on %2: %3")
               .arg(this->multicastGroup.toString(), networkInterface.humanReadableName(), this->multicastSocket.errorString()));
         continue;
      }
      this->multicastInterfaces << networkInterface;
      L_DEBU(QString("Joined multicast group %1 on %2 (%3)")
         .arg(this->multicastGroup.toString(), networkInterface.humanReadableName()).arg(networkInterface.index()));
   }
   if (this->multicastInterfaces.isEmpty())
   {
      if (logFailures)
         L_ERRO("No usable multicast interface; discovery is disabled");
      return false;
   }

   UDPListener::setBufferSize(this->multicastSocket);

   return true;
}

bool UDPListener::bindUnicastSocket(const QHostAddress& address, quint16 port)
{
   this->closeSockets();

   if (port == 0 || !this->unicastSocket.bind(address, port, QUdpSocket::DontShareAddress))
      return false;

   UDPListener::setBufferSize(this->unicastSocket);

   return true;
}

/**
  * Set the size of the send and receive buffers of the given socket to the setting 'udp_buffer_size'.
  */
void UDPListener::setBufferSize(QUdpSocket& socket)
{
   // This settings cannot change dynamically -> static.
   static const quint32 BUFFER_SIZE_UDP = SETTINGS.get<quint32>("udp_buffer_size");
   socket.setSocketOption(QAbstractSocket::SendBufferSizeSocketOption, BUFFER_SIZE_UDP);
   socket.setSocketOption(QAbstractSocket::ReceiveBufferSizeSocketOption, BUFFER_SIZE_UDP);
}

/**
  * Writes a given protobuf message to the buffer (this->buffer) prefixed by a header.
  * @return the total size (header size + message size).
  *         Return 0 if the total size is bigger than 'Protos.Core.Settings.max_udp_datagram_size'.
  */
int UDPListener::writeMessageToBuffer(Common::MessageHeader::MessageType type, const google::protobuf::Message& message)
{
   const Common::MessageHeader header(type, message.ByteSizeLong(), this->getOwnID());

   const int nbBytesWritten =
      Common::Message::writeMessageToBuffer(this->buffer, this->MAX_UDP_DATAGRAM_PAYLOAD_SIZE, header, &message);

   if (!nbBytesWritten)
      L_ERRO(
         QString("Datagram size too big: %1, max allowed: %2")
            .arg(Common::MessageHeader::HEADER_SIZE + header.getPayloadSize()).arg(this->MAX_UDP_DATAGRAM_PAYLOAD_SIZE)
      );

   return nbBytesWritten;
}

/**
  * @return A null header if error.
  */
Common::MessageHeader UDPListener::readDatagramToBuffer(QUdpSocket& socket, QHostAddress& peerAddress)
{
   quint16 port = 0;
   const qint64 datagramSize = socket.readDatagram(this->buffer, BUFFER_SIZE, &peerAddress, &port);
   if (datagramSize == -1)
   {
      L_WARN(
         QString("UDPListener::readDatagramToBuffer(..): Unable to read a datagram (%1): %2")
            .arg(&socket == &this->multicastSocket ? "multicast" : "unicast", socket.errorString())
      );
      return Common::MessageHeader();
   }

   // The header must be entirely checked before being parsed, otherwise stale bytes from a previous datagram would be read.
   if (datagramSize < Common::MessageHeader::HEADER_SIZE)
   {
      L_WARN(
         QString("The datagram received from %1:%2 is too small (%3 bytes) to contain a header")
            .arg(peerAddress.toString())
            .arg(port)
            .arg(datagramSize)
      );
      return Common::MessageHeader();
   }

   Common::MessageHeader header = Common::MessageHeader::readHeader(this->buffer);

   if (header.getPayloadSize() > datagramSize - Common::MessageHeader::HEADER_SIZE)
   {
      L_ERRO("The payload size of the header exceeds the datagram size received");
      header.setNull();
      return header;
   }

   if (header.getSenderID() == this->getOwnID())
   {
      // L_WARN("We receive a datagram from ourself, skip"); // Don't care . . .
      header.setNull();
      return header;
   }

   L_DEBU(QString("Receive a datagram UDP from %1 (%2): %3").arg(header.getSenderID().toStrShort(), peerAddress.toString(), header.toStr()));
   return header;
}

/**
  * A multicast message is sent on each interface, see 'send(..)'. A peer sharing several networks with us, or
  * connected several times to the same one (wired and wireless for example), receives it several times, from
  * different addresses. Only the first copy must be processed: the others would duplicate the search results
  * and make the address of the sender flip, which retires the sockets connected to it.
  * The copies are identical and follow each other closely. A datagram is forgotten after a short time: an
  * identical one received later is a new message.
  * @return true if the multicast datagram in the buffer has already been received.
  */
bool UDPListener::isDuplicate(const Common::MessageHeader& header)
{
   static const int LIFETIME = 1000; // [ms].

   // Nothing distinguishes a new goodbye from a copy, and processing it again is harmless.
   if (header.getType() == Common::MessageHeader::CORE_GOODBYE)
      return false;

   while (!this->recentMulticastDatagrams.isEmpty() && this->recentMulticastDatagrams.first().expiration.hasExpired())
      this->recentMulticastDatagrams.removeFirst();

   // The header is included: it contains the ID of the sender.
   const QByteArrayView datagram(this->buffer, Common::MessageHeader::HEADER_SIZE + header.getPayloadSize());
   const size_t hash = qHash(datagram, QHashSeed::globalSeed());

   for (const ReceivedDatagram& received : std::as_const(this->recentMulticastDatagrams))
      if (received.hash == hash && received.size == datagram.size())
         return true;

   this->recentMulticastDatagrams << ReceivedDatagram { hash, datagram.size(), QDeadlineTimer(LIFETIME) };
   return false;
}

/**
  * @param mustBeAvailable If false, an alive peer is enough (it may be blocked or incompatible).
  * @return The sender of the datagram or nullptr if it is unknown, dead or, if required, unavailable.
  */
PM::IPeer* UDPListener::getSender(const Common::MessageHeader& header, const QHostAddress& peerAddress, bool mustBeAvailable)
{
   PM::IPeer* peer = this->peerManager->getPeer(header.getSenderID());
   if (!peer)
   {
      L_WARN(QString("We receive a datagram from an unknown peer (%1), skip").arg(peerAddress.toString()));
      return nullptr;
   }

   if (!peer->isAlive())
   {
      L_WARN(QString("We receive a datagram from a dead peer (%1), skip").arg(peerAddress.toString()));
      return nullptr;
   }

   // Blocked or incompatible peers are silently ignored.
   return !mustBeAvailable || peer->isAvailable() ? peer : nullptr;
}

Common::Hash UDPListener::getOwnID() const
{
   return this->peerManager->getSelf()->getID();
}

/**
  * @return The value of the setting 'max_udp_datagram_size' clamped to the size of the internal buffer.
  *         The value must be at least the size of a header and cannot exceed 'BUFFER_SIZE', otherwise 'writeMessageToBuffer(..)' could overflow the buffer.
  */
int UDPListener::getMaxUDPDatagramPayloadSize()
{
   const quint32 configuredSize = SETTINGS.get<quint32>("max_udp_datagram_size");
   const int clampedSize = static_cast<int>(qBound<quint32>(Common::MessageHeader::HEADER_SIZE, configuredSize, BUFFER_SIZE));

   if (static_cast<quint32>(clampedSize) != configuredSize)
      L_WARN(QString("The setting 'max_udp_datagram_size' (%1) is out of range, it has been clamped to %2").arg(configuredSize).arg(clampedSize));

   return clampedSize;
}
