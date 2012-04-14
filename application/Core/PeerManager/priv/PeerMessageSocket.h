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
  
#ifndef PEERMANAGER_SOCKET_H
#define PEERMANAGER_SOCKET_H

#include <QTcpSocket>
#include <QDateTime>
#include <QHostAddress>
#include <QTimer>
#include <QQueue>
#include <QSharedPointer>

#include <google/protobuf/message.h>

#include <Common/Hash.h>
#include <Common/Uncopyable.h>
#include <Common/Network/MessageHeader.h>
#include <Common/Network/MessageSocket.h>
#include <Core/FileManager/IFileManager.h>
#include <Core/FileManager/IChunk.h>

#include <ISocket.h>

namespace PM
{
   class PeerManager;

   class PeerMessageSocket : public Common::MessageSocket, public ISocket
   {
      Q_OBJECT

   protected:
      class Logger : public ILogger
      {
      public:
         void logDebug(const QString& message);
         void logError(const QString& message);
      };

   public:
      PeerMessageSocket(PeerManager* peerManager, QSharedPointer<FM::IFileManager> fileManager, const Common::Hash& remotePeerID, QTcpSocket* socket);
      PeerMessageSocket(PeerManager* peerManager, QSharedPointer<FM::IFileManager> fileManager, const Common::Hash& remotePeerID, const QHostAddress& address, quint16 port);
      ~PeerMessageSocket();

      void setReadBufferSize(qint64 size);

      qint64 bytesAvailable() const;
      qint64 read(char* data, qint64 maxSize);
      QByteArray readAll();
      bool waitForReadyRead(int msecs);

      qint64 bytesToWrite() const;
      qint64 write(const char* data, qint64 maxSize);
      qint64 write(const QByteArray& byteArray);
      bool waitForBytesWritten(int msecs);

      void moveToThread(QThread* targetThread);
      QString errorString() const;

      Common::Hash getRemotePeerID() const;

      void send(MessageHeader::MessageType type, const google::protobuf::Message& message);

      bool isActive() const;
      void setActive();

      void finished(bool closeTheSocket = false);

   public slots:
      void close();

   signals:
      void getChunk(QSharedPointer<FM::IChunk>, int, PeerMessageSocket*);
      void becomeIdle(PeerMessageSocket*);

      /**
        * Emitted when the socket is disconnected or explicitly closed by calling 'close()'.
        */
      void closed(PeerMessageSocket*);

   private slots:
      void nextAskedHash(Common::Hash hash);

   private:
      void onNewMessage(Common::MessageHeader::MessageType type, const google::protobuf::Message& message);
      void onNewDataReceived();
      void onDisconnected();
      void initUnactiveTimer();

      QSharedPointer<FM::IFileManager> fileManager;

      bool active;
      QTimer inactiveTimer;
      int nbError;

      // Used when asking hashes to the fileManager.
      QSharedPointer<FM::IGetHashesResult> currentHashesResult;
      int nbHash;
   };
}

#endif
