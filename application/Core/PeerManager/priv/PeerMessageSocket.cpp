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
  
#include <priv/PeerMessageSocket.h>
using namespace PM;

#include <QCoreApplication>

#include <Protos/core_protocol.pb.h>
#include <Protos/common.pb.h>

#include <Common/ZeroCopyStreamQIODevice.h>
#include <Common/Settings.h>
#include <Common/ProtoHelper.h>

#include <priv/Log.h>
#include <priv/PeerManager.h>
#include <priv/Constants.h>

void PeerMessageSocket::Logger::logDebug(const QString& message)
{
   L_DEBU(message);
}

void PeerMessageSocket::Logger::logError(const QString& message)
{
   L_WARN(message);
}

PeerMessageSocket::PeerMessageSocket(PeerManager* peerManager, QSharedPointer<FM::IFileManager> fileManager, const Common::Hash& remotePeerID, QTcpSocket* socket) :
   MessageSocket(new PeerMessageSocket::Logger(), socket, peerManager->getSelf()->getID(), remotePeerID), fileManager(fileManager), active(true), nbError(0)
{
   this->initUnactiveTimer();
}

PeerMessageSocket::PeerMessageSocket(PeerManager* peerManager, QSharedPointer<FM::IFileManager> fileManager, const Common::Hash& remotePeerID, const QHostAddress& address, quint16 port) :
   MessageSocket(new PeerMessageSocket::Logger(), address, port, peerManager->getSelf()->getID(), remotePeerID), fileManager(fileManager), active(true), nbError(0)
{
   this->initUnactiveTimer();
}

PeerMessageSocket::~PeerMessageSocket()
{
   L_DEBU(QString("Socket[%1] deleted").arg(this->num));
}

void PeerMessageSocket::setReadBufferSize(qint64 size)
{
   this->socket->setReadBufferSize(size);
}

qint64 PeerMessageSocket::bytesAvailable() const
{
   return this->socket->bytesAvailable();
}

qint64 PeerMessageSocket::read(char* data, qint64 maxSize)
{
   return this->socket->read(data, maxSize);
}

QByteArray PeerMessageSocket::readAll()
{
   return this->socket->readAll();
}

bool PeerMessageSocket::waitForReadyRead(int msecs)
{
   return this->socket->waitForReadyRead(msecs);
}

qint64 PeerMessageSocket::bytesToWrite() const
{
   return this->socket->bytesToWrite();
}

qint64 PeerMessageSocket::write(const char* data, qint64 maxSize)
{
   return this->socket->write(data, maxSize);
}

qint64 PeerMessageSocket::write(const QByteArray& byteArray)
{
   return this->socket->write(byteArray);
}

bool PeerMessageSocket::waitForBytesWritten(int msecs)
{
   return this->socket->waitForBytesWritten(msecs);
}

void PeerMessageSocket::moveToThread(QThread* targetThread)
{
   this->socket->moveToThread(targetThread);
}

QString PeerMessageSocket::errorString() const
{
   return this->socket->errorString();
}

Common::Hash PeerMessageSocket::getRemotePeerID() const
{
   return this->MessageSocket::getRemoteID();
}

void PeerMessageSocket::send(MessageHeader::MessageType type, const google::protobuf::Message& message)
{
   if (!this->isListening())
      return;

   this->setActive();

   this->MessageSocket::send(type, message);
}

/**
  * Is the socket currently been used?
  */
bool PeerMessageSocket::isActive() const
{
   return this->active;
}

/**
  * Change the status of the socket to active. Automatically called when a message is sent.
  */
void PeerMessageSocket::setActive()
{
   this->inactiveTimer.start(); // Some transactions (like GET_HASHES) can go for a long time, we have to restart the timer even for an active connection.

   if (this->active)
      return;

   L_DEBU(QString("Socket[%1] set to active >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>").arg(this->num));

   this->active = true;
}

/**
  * Must be called when a transaction is terminated.
  */
void PeerMessageSocket::finished(bool closeTheSocket)
{
   if (!this->active)
      return;

   L_DEBU(QString("Socket[%1] set to idle%2<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<").arg(this->num).arg(closeTheSocket ? " (socket forced to close) " : " "));

   if (closeTheSocket)
   {
      L_WARN("Socket forced to close..");
      this->close();
      return;
   }
   else if (!this->socket->isValid())
   {
      L_WARN("Socket non-valid, closed");
      this->close();
      return;
   }

   this->socket->flush();
   this->active = false;

   this->startListening();
   emit becomeIdle(this);
}

/**
  * Only emit the 'closed(..)' signal, do not close the socket.
  */
void PeerMessageSocket::close()
{
   this->active = false;

   emit closed(this);
}

/**
  * When we ask to the fileManager some hashes for a given file this
  * slot will be called each time a new hash is available.
  */
void PeerMessageSocket::nextAskedHash(Common::Hash hash)
{
   Protos::Common::Hash hashProto;
   hashProto.set_hash(hash.getData(), Common::Hash::HASH_SIZE);
   this->send(Common::MessageHeader::CORE_HASH, hashProto);

   if (--this->nbHash == 0)
   {
      this->currentHashesResult.clear();
      this->finished();
   }
}

/**
  *
  */
void PeerMessageSocket::onNewMessage(Common::MessageHeader::MessageType type, const google::protobuf::Message& message)
{
   switch (type)
   {
   case Common::MessageHeader::CORE_GET_ENTRIES:
      {
         const Protos::Core::GetEntries& getEntries = static_cast<const Protos::Core::GetEntries&>(message);

         Protos::Core::GetEntriesResult result;
         for (int i = 0; i < getEntries.dirs().entry_size(); i++)
            result.add_entries()->CopyFrom(this->fileManager->getEntries(getEntries.dirs().entry(i)));

         // Add the root directories if asked.
         if (getEntries.dirs().entry_size() == 0 || getEntries.get_roots())
            result.add_entries()->CopyFrom(this->fileManager->getEntries());

         this->send(Common::MessageHeader::CORE_GET_ENTRIES_RESULT, result);

         this->finished();
      }
      break;

   case Common::MessageHeader::CORE_GET_ENTRIES_RESULT:
      this->finished();
      break;

   case Common::MessageHeader::CORE_GET_HASHES:
      {
         const Protos::Core::GetHashes& getHashes = static_cast<const Protos::Core::GetHashes&>(message);

         this->currentHashesResult = this->fileManager->getHashes(getHashes.file());
         connect(this->currentHashesResult.data(), SIGNAL(nextHash(Common::Hash)), this, SLOT(nextAskedHash(Common::Hash)), Qt::QueuedConnection);
         Protos::Core::GetHashesResult res = this->currentHashesResult->start();

         this->nbHash = res.nb_hash();

         this->send(Common::MessageHeader::CORE_GET_HASHES_RESULT, res);

         if (res.status() != Protos::Core::GetHashesResult_Status_OK)
         {
            this->currentHashesResult.clear();
            this->finished();
         }
      }
      break;

   case Common::MessageHeader::CORE_GET_HASHES_RESULT:
      {
         const Protos::Core::GetHashesResult& getHashesResult = static_cast<const Protos::Core::GetHashesResult&>(message);
         this->nbHash = getHashesResult.nb_hash();
      }
      break;

   case Common::MessageHeader::CORE_HASH:
      {
         if (--this->nbHash == 0)
            this->finished();
      }
      break;

   case Common::MessageHeader::CORE_GET_CHUNK:
      {
         const Protos::Core::GetChunk& getChunkMessage = static_cast<const Protos::Core::GetChunk&>(message);

         const Common::Hash hash(getChunkMessage.chunk().hash());
         if (hash.isNull())
         {
            L_WARN("GET_CHUNK: Chunk null");
            this->finished(true);
            break;
         }

         // TODO implements 'GetChunkResult.ALREADY_DOWNLOADING', 'GetChunkResult.TOO_MANY_CONNECTIONS' and 'GetChunkResult.DONT_HAVE_DATA_FROM_OFFSET'
         QSharedPointer<FM::IChunk> chunk = this->fileManager->getChunk(hash);
         if (chunk.isNull())
         {
            Protos::Core::GetChunkResult result;
            result.set_status(Protos::Core::GetChunkResult::DONT_HAVE);
            this->send(Common::MessageHeader::CORE_GET_CHUNK_RESULT, result);
            this->finished();

            L_WARN(QString("GET_CHUNK: Chunk unknown : %1").arg(hash.toStr()));
         }
         else
         {
            Protos::Core::GetChunkResult result;
            result.set_status(Protos::Core::GetChunkResult::OK);
            result.set_chunk_size(chunk->getKnownBytes());
            this->send(Common::MessageHeader::CORE_GET_CHUNK_RESULT, result);

            this->stopListening();

            emit getChunk(chunk, getChunkMessage.offset(), this);
         }
      }
      break;

   default:; // Do nothing.
   }
}

void PeerMessageSocket::onNewDataReceived()
{
   this->setActive();
}

void PeerMessageSocket::onDisconnected()
{
   this->close();
}

void PeerMessageSocket::initUnactiveTimer()
{
   this->inactiveTimer.setSingleShot(true);
   this->inactiveTimer.setInterval(SETTINGS.get<quint32>("idle_socket_timeout"));
   connect(&this->inactiveTimer, SIGNAL(timeout()), this, SLOT(close()));
   this->inactiveTimer.start();
}
