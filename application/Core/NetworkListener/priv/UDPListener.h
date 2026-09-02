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

#pragma once

#include <QObject>
#include <QUdpSocket>
#include <QTimer>
#include <QSharedPointer>
#include <QNetworkInterface>

#include <google/protobuf/message.h>

#include <Protos/core_protocol.pb.h>
#include <Protos/common.pb.h>

#include <Common/Uncopyable.h>
#include <Common/Network/MessageHeader.h>
#include <Common/Network/Message.h>
#include <Common/LogManager/Builder.h>
#include <Common/LogManager/ILogger.h>
#include <Core/FileManager/IFileManager.h>
#include <Core/PeerManager/IPeerManager.h>
#include <Core/UploadManager/IUploadManager.h>
#include <Core/DownloadManager/IDownloadManager.h>
#include <INetworkListener.h>

namespace NL
{
   class UDPListener : public QObject, Common::Uncopyable
   {
      Q_OBJECT
      // The size max of an UDP datagram : 2^16.
      // Usually the size of an UDP datagram is smaller, see 'Protos::CoreSettings::max_udp_datagram_size'.
      static const int BUFFER_SIZE = 65536;

      static const int MAX_NICK_LENGTH = 255; // Datagram UDP are limited in size, this limit avoid to fill the whole datagram with only a nickname.

   public:
      UDPListener(
         QSharedPointer<FM::IFileManager> fileManager,
         QSharedPointer<PM::IPeerManager> peerManager,
         QSharedPointer<UM::IUploadManager> uploadManager,
         QSharedPointer<DM::IDownloadManager> downloadManager
      );

      INetworkListener::SendStatus send(Common::MessageHeader::MessageType type, const google::protobuf::Message& message, const Common::Hash& peerID);
      INetworkListener::SendStatus send(Common::MessageHeader::MessageType type, const google::protobuf::Message& message = Protos::Common::Null());

      /**
        * The sockets aren't bound until this method is called.
        * @param unicastPort The port to bind the unicast socket to, it must be the same as the TCP port.
        */
      void rebindSockets(quint16 unicastPort);

   signals:
      /**
        * This signal is emitted when a message is received (unicast or multicast).
        */
      void received(const Common::Message& message);
      void IMAliveMessageToBeSend(Protos::Core::IMAlive& IMAliveMessage);
      void newFindResultMessage(const Protos::Common::FindResult& findResult);

   private slots:
      void sendIMAliveMessage();
      void processPendingMulticastDatagrams();
      void processPendingUnicastDatagrams();

      void initMulticastUDPSocket();
      void initUnicastUDPSocket();

   private:
      int writeMessageToBuffer(Common::MessageHeader::MessageType type, const google::protobuf::Message& message);
      Common::MessageHeader readDatagramToBuffer(QUdpSocket& socket, QHostAddress& peerAddress);

      Common::Hash getOwnID() const;
      static int getMaxUDPDatagramPayloadSize();

      const int MAX_UDP_DATAGRAM_PAYLOAD_SIZE; // Always between HEADER_SIZE and BUFFER_SIZE.

      char buffer[BUFFER_SIZE]; // Buffer used when sending or receiving datagram.
      char* const bodyBuffer;

      quint16 unicastPort; // Same as the TCP port, it may change when the sockets are rebound.
      const quint16 MULTICAST_PORT;
      QHostAddress multicastGroup;

      QSharedPointer<FM::IFileManager> fileManager;
      QSharedPointer<PM::IPeerManager> peerManager;
      QSharedPointer<UM::IUploadManager> uploadManager;
      QSharedPointer<DM::IDownloadManager> downloadManager;

      QUdpSocket multicastSocket;
      QUdpSocket unicastSocket;

      quint64 currentIMAliveTag;
      QList<QSharedPointer<DM::IChunkDownloader>> currentChunkDownloaders;
      enum HashRequestType
      {
         FIRST_HASHES,
         OLDEST_HASHES
      };
      HashRequestType nextHashRequestType;

      QTimer timerIMAlive;
      QSharedPointer<LM::ILogger> loggerIMAlive; // A logger especially for the IMAlive message.
   };
}
