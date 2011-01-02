/**
  * Aybabtu - A decentralized LAN file sharing software.
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
  
#ifndef PEERMANAGER_SOCKET_H
#define PEERMANAGER_SOCKET_H

#include <QTcpSocket>
#include <QDateTime>
#include <QHostAddress>
#include <QTimer>
#include <QQueue>

#include <google/protobuf/message.h>

#include <Common/Network.h>
#include <Common/Hash.h>
#include <Common/Uncopyable.h>
#include <Core/FileManager/IFileManager.h>

#include <ISocket.h>

namespace PM
{
   class PeerManager;

   class Socket : public QObject, public ISocket, Common::Uncopyable
   {
      Q_OBJECT
   public:
      Socket(PeerManager* peerManager, QSharedPointer<FM::IFileManager> fileManager, const Common::Hash& peerID, QTcpSocket* socket);
      Socket(PeerManager* peerManager, QSharedPointer<FM::IFileManager> fileManager, const Common::Hash& peerID, const QHostAddress& address, quint16 port);
      ~Socket();

      QAbstractSocket* getQSocket() const;

      Common::Hash getPeerID() const;

      void startListening();
      void stopListening();
      bool isIdle() const;
      void setActive();

      void send(Common::Network::CoreMessageType type, const google::protobuf::Message& message);
      void finished(SocketFinishedStatus status = SFS_OK);

   public slots:
      void close();

   signals:
      void newMessage(Common::Network::CoreMessageType type, const google::protobuf::Message& message);
      void getIdle(Socket*);
      void closed(Socket*);
      void getChunk(const Common::Hash&, int, Socket*);

   private slots:
      void dataReceived();
      void disconnected();

      void nextAskedHash(Common::Hash hash);

   private:
      bool readMessage();
      void initActivityTimer();

      PeerManager* peerManager;
      QSharedPointer<FM::IFileManager> fileManager;
      const Common::Hash peerID;

      Common::Network::MessageHeader<Common::Network::CoreMessageType> currentHeader;

      QTcpSocket* socket;

      QTimer activityTimer; ///< When there is no activity during some time the socket is automatically closed.
      bool idle;
      bool listening;

      // Used when asking hashes to the fileManager.
      QSharedPointer<FM::IGetHashesResult> currentHashesResult;
      int nbHash;

      int nbError;

#ifdef DEBUG
      // To identify the sockets in debug mode.
      int num;
      static int currentNum;
#endif
   };
}

#endif
