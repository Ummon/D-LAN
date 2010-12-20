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
  
#include <priv/Socket.h>
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

Socket::Socket(PeerManager* peerManager, QSharedPointer<FM::IFileManager> fileManager, const Common::Hash& peerID, QTcpSocket* socket)
   : peerManager(peerManager), fileManager(fileManager), peerID(peerID), socket(socket), idle(false), listening(false), nbError(0)
{
#ifdef DEBUG
   this->num = ++Socket::currentNum;
   L_DEBU(QString("New Socket[%1] (connection from %2:%3)").arg(this->num).arg(socket->peerAddress().toString()).arg(socket->peerPort()));
#endif

   this->socket->setReadBufferSize(SETTINGS.get<quint32>("socket_buffer_size"));
   this->initActivityTimer();
}

Socket::Socket(PeerManager* peerManager, QSharedPointer<FM::IFileManager> fileManager, const Common::Hash& peerID, const QHostAddress& address, quint16 port)
   : peerManager(peerManager), fileManager(fileManager), peerID(peerID), idle(false), listening(false), nbError(0)
{   
#ifdef DEBUG
   this->num = ++Socket::currentNum;
   L_DEBU(QString("New Socket[%1] (connection to %2:%3)").arg(this->num).arg(address.toString()).arg(port));
#endif

   this->socket = new QTcpSocket();
   this->socket->setReadBufferSize(SETTINGS.get<quint32>("socket_buffer_size"));
   this->socket->connectToHost(address, port);
   this->initActivityTimer();
}

Socket::~Socket()
{
   L_DEBU(QString("Socket[%1] deleted").arg(this->num));

   this->socket->deleteLater();
}

QAbstractSocket* Socket::getQSocket() const
{
   return this->socket;
}

Common::Hash Socket::getPeerID() const
{
   return this->peerID;
}

void Socket::startListening()
{
   // To prevent multi listening.
   if (this->listening)
      return;

   L_DEBU(QString("Socket[%1] starting to listen").arg(this->num));

   this->listening = true;

   if (!this->socket->isValid())
      this->close();
   else
   {
      connect(this->socket, SIGNAL(readyRead()), this, SLOT(dataReceived()), Qt::DirectConnection);
      connect(this->socket, SIGNAL(disconnected()), this, SLOT(disconnected()), Qt::DirectConnection);
      this->dataReceived();
   }
}

void Socket::stopListening()
{
   L_DEBU(QString("Socket[%1] stopping to listen").arg(this->num));

   disconnect(this->socket, SIGNAL(readyRead()), this, SLOT(dataReceived()));
   disconnect(this->socket, SIGNAL(disconnected()), this, SLOT(disconnected()));

   this->listening = false;
}

bool Socket::isIdle() const
{
   return this->idle;
}

void Socket::setActive()
{
   this->activityTimer.start(); // Some transactions (like GET_HASHES) can go for a long time, we have to restart the timer even for an active connection.

   if (!this->idle)
      return;

   L_DEBU(QString("Socket[%1] set to active >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>").arg(this->num));

   this->idle = false;
}

void Socket::send(Common::Network::CoreMessageType type, const google::protobuf::Message& message)
{
   if (!this->listening)
      return;

   Common::Network::MessageHeader<Common::Network::CoreMessageType> header(type, message.ByteSize(), this->peerManager->getID());

   this->setActive();

   L_DEBU(QString("Socket[%1]::send : %2 to %3\n%4").arg(this->num).arg(header.toStr()).arg(this->peerID.toStr()).arg(Common::ProtoHelper::getDebugStr(message)));

   {
      Common::Network::writeHeader(*this->socket, header);
      Common::ZeroCopyOutputStreamQIODevice outputStream(this->socket);
      if (!message.SerializeToZeroCopyStream(&outputStream))
         L_WARN(QString("Unable to send %1").arg(Common::ProtoHelper::getDebugStr(message)));
   }

   if (this->socket->state() == QAbstractSocket::ConnectedState)
      this->socket->flush();
}

void Socket::dataReceived()
{
   while (!this->socket->atEnd() && this->listening)
   {
      this->setActive();

      if (this->currentHeader.isNull() && this->socket->bytesAvailable() >= Common::Network::HEADER_SIZE)
      {
         this->currentHeader = Common::Network::readHeader<Common::Network::CoreMessageType>(*this->socket);

         L_DEBU(QString("Socket[%1]: Data received from %2, %3")
            .arg(this->num)
            .arg(this->socket->peerAddress().toString())
            .arg(this->currentHeader.toStr())
         );

         if (this->currentHeader.senderID != this->peerID)
         {
            L_DEBU(QString("Socket[%1]: Peer ID from message (%2) doesn't match the known peer ID (%3)").arg(this->num).arg(this->currentHeader.senderID.toStr()).arg(this->peerID.toStr()));
            this->finished(true);
            this->currentHeader.setNull();
            return;
         }
      }

      if (!this->currentHeader.isNull() && this->socket->bytesAvailable() >= this->currentHeader.size)
      {
         if (!this->readMessage())
         {
            L_DEBU("Can't read the message -> finished");
            this->finished(true);
         }
         this->currentHeader.setNull();
      }
      else
         return;
   }
}

void Socket::finished(bool error)
{
   if (this->idle)
      return;

   L_DEBU(QString("Socket[%1] set to idle%2<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<").arg(this->num).arg(error ? " with error " : " "));

   if (error && ++this->nbError > 5) // TODO : -> constant
   {
      L_WARN("Socket with too many error, closed");
      this->close();
      return;
   }

   if (!this->socket->isValid())
   {
      this->close();
      return;
   }

   this->socket->flush();
   this->idle = true;

   this->startListening();
   emit getIdle(this);
}

/**
  * Close remove from the connection pool.
  */
void Socket::close()
{
   L_DEBU(QString("Socket[%1] closing..").arg(this->num));

   disconnect(this->socket, SIGNAL(readyRead()), this, SLOT(dataReceived()));
   disconnect(this->socket, SIGNAL(disconnected()), this, SLOT(disconnected()));

   this->idle = true;
   emit closed(this);
}

void Socket::disconnected()
{   
   L_DEBU(QString("Socket[%1] disconnected").arg(this->num));
   this->close();
}

/**
  * When we ask to the fileManager some hashes for a given file this
  * slot will be called each time a new hash is available.
  */
void Socket::nextAskedHash(Common::Hash hash)
{
   Protos::Common::Hash hashProto;
   hashProto.set_hash(hash.getData(), Common::Hash::HASH_SIZE);
   this->send(Common::Network::CORE_HASH, hashProto);

   if (--this->nbHash == 0)
   {
      this->currentHashesResult.clear();
      this->finished();
   }
}

/**
  * @return 'true' if the message has been handled properly otherwise return 'false'.
  */
bool Socket::readMessage()
{
   bool readOK = false;

   switch (this->currentHeader.type)
   {
   case Common::Network::CORE_GET_ENTRIES:
      {
         Protos::Core::GetEntries getEntries;

         // This scope (and the others ones below) is here to force the input stream to read all the bytes.
         // See Common::ZeroCopyInputStreamQIODevice::~ZeroCopyInputStreamQIODevice.
         {
            Common::ZeroCopyInputStreamQIODevice inputStream(this->socket);
            readOK = this->currentHeader.size == 0 || getEntries.ParseFromBoundedZeroCopyStream(&inputStream, this->currentHeader.size);
         }

         if (readOK)
            this->send(
               Common::Network::CORE_GET_ENTRIES_RESULT,
               getEntries.has_dir() ? this->fileManager->getEntries(getEntries.dir()) : this->fileManager->getEntries()
            );

         this->finished();
      }
      break;

   case Common::Network::CORE_GET_ENTRIES_RESULT:
      {
         Protos::Common::Entries getEntriesResult;
         {
            Common::ZeroCopyInputStreamQIODevice inputStream(this->socket);
            readOK = getEntriesResult.ParseFromBoundedZeroCopyStream(&inputStream, this->currentHeader.size);
         }

         this->finished();

         if (readOK)
         {
            emit newMessage(this->currentHeader.type, getEntriesResult);
         }
      }
      break;

   case Common::Network::CORE_GET_HASHES:
      {
         Protos::Core::GetHashes getHashes;
         {
            Common::ZeroCopyInputStreamQIODevice inputStream(this->socket);
            readOK = getHashes.ParseFromBoundedZeroCopyStream(&inputStream, this->currentHeader.size);
         }

         if (readOK)
         {
            this->currentHashesResult = this->fileManager->getHashes(getHashes.file());
            connect(this->currentHashesResult.data(), SIGNAL(nextHash(Common::Hash)), this, SLOT(nextAskedHash(Common::Hash)), Qt::QueuedConnection);
            Protos::Core::GetHashesResult res = this->currentHashesResult->start();

            this->nbHash = res.nb_hash();

            this->send(Common::Network::CORE_GET_HASHES_RESULT, res);

            if (res.status() != Protos::Core::GetHashesResult_Status_OK)
            {
               this->currentHashesResult.clear();
               this->finished();
            }
         }
      }
      break;

   case Common::Network::CORE_GET_HASHES_RESULT:
      {
         Protos::Core::GetHashesResult getHashesResult;
         {
            Common::ZeroCopyInputStreamQIODevice inputStream(this->socket);
            readOK = getHashesResult.ParseFromBoundedZeroCopyStream(&inputStream, this->currentHeader.size);
         }

         if (readOK)
         {
            this->nbHash = getHashesResult.nb_hash();
            emit newMessage(this->currentHeader.type, getHashesResult);
         }
      }
      break;

   case Common::Network::CORE_HASH:
      {
         Protos::Common::Hash hash;
         {
            Common::ZeroCopyInputStreamQIODevice inputStream(this->socket);
            readOK = hash.ParseFromBoundedZeroCopyStream(&inputStream, this->currentHeader.size);
         }

         if (readOK)
         {
            if (--this->nbHash == 0)
               this->finished();

            emit newMessage(this->currentHeader.type, hash);
         }
      }
      break;

   case Common::Network::CORE_GET_CHUNK:
      {
         Protos::Core::GetChunk getChunkMessage;
         {
            Common::ZeroCopyInputStreamQIODevice inputStream(this->socket);
            readOK = getChunkMessage.ParseFromBoundedZeroCopyStream(&inputStream, this->currentHeader.size);
         }

         if (readOK)
         {
            emit getChunk(Common::Hash(getChunkMessage.chunk().hash().data()), getChunkMessage.offset(), this);
         }
      }
      break;

   case Common::Network::CORE_GET_CHUNK_RESULT:
      {
         Protos::Core::GetChunkResult getChunkResult;
         {
            Common::ZeroCopyInputStreamQIODevice inputStream(this->socket);
            readOK = getChunkResult.ParseFromBoundedZeroCopyStream(&inputStream, this->currentHeader.size);
         }

         if (readOK)
            emit newMessage(this->currentHeader.type, getChunkResult);
      }
      break;

   default:
      readOK = false;
   }

   return readOK;
}

void Socket::initActivityTimer()
{
   this->activityTimer.setSingleShot(true);
   this->activityTimer.setInterval(SETTINGS.get<quint32>("idle_socket_timeout"));
   connect(&this->activityTimer, SIGNAL(timeout()), this, SLOT(close()));
   this->activityTimer.start();
}

#ifdef DEBUG
   int Socket::currentNum(0);
#endif
