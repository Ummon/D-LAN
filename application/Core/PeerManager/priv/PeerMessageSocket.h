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
#include <Core/FileManager/IGetEntriesResult.h>
#include <Core/FileManager/IChunk.h>

#include <ISocket.h>
#include <GetChunkParams.h>

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
      PeerMessageSocket(
         PeerManager* peerManager,
         QSharedPointer<FM::IFileManager> fileManager,
         const Common::Hash& remotePeerID,
         QTcpSocket* socket
      );

      PeerMessageSocket(
         PeerManager* peerManager,
         QSharedPointer<FM::IFileManager> fileManager,
         const Common::Hash& remotePeerID,
         const QHostAddress& address,
         quint16 port
      );

      ~PeerMessageSocket();

      void setReadBufferSize(qint64 size) override;

      qint64 bytesAvailable() const override;
      qint64 read(char* data, qint64 maxSize) override;
      QByteArray readAll() override;
      bool waitForReadyRead(int msecs) override;

      qint64 bytesToWrite() const override;
      qint64 write(const char* data, qint64 maxSize) override;
      qint64 write(const QByteArray& byteArray) override;
      bool waitForBytesWritten(int msecs) override;

      void moveToThread(QThread* targetThread) override;
      QString errorString() const override;

      Common::Hash getRemotePeerID() const override;

      void send(Common::MessageHeader::MessageType type, const google::protobuf::Message& message) override;

      bool isActive() const;
      void setActive();

      void finished(bool closeTheSocket = false) override;

   public slots:
      void close() override;

   signals:
      void getChunks(QList<GetChunkParams>, PM::PeerMessageSocket*);
      void becomeIdle(PM::PeerMessageSocket*);

      /**
        * Emitted when the socket is disconnected or explicitly closed by calling 'close()'.
        */
      void closed(PM::PeerMessageSocket*);

   private slots:
      void nextAskedHash(Protos::Core::HashResult hash);
      void entriesResult(const Protos::Core::GetEntriesResult::EntryResult& result);
      void entriesResultTimeout();

   private:
      void onNewMessage(const Common::Message& message) override;
      void onNewDataReceived() override;
      void onDisconnected() override;
      void initUnactiveTimer();

      void sendEntriesResultMessage();

      QList<QSharedPointer<FM::IGetEntriesResult>> entriesResultsToReceive;
      Protos::Core::GetEntriesResult entriesResultMessage;

      QSharedPointer<FM::IFileManager> fileManager;

      bool active;
      QTimer inactiveTimer;
      int nbError;

      // Used when asking hashes to the fileManager.
      QSharedPointer<FM::IGetHashesResult> currentHashesResult;
      int nbHash;
   };
}
