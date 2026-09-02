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

PeerMessageSocket::PeerMessageSocket(
   PeerManager* peerManager,
   QSharedPointer<FM::IFileManager> fileManager,
   const Common::Hash& remotePeerID,
   QTcpSocket* socket
) :
   MessageSocket(
      new PeerMessageSocket::Logger(),
      socket,
      peerManager->getSelf()->getID(),
      remotePeerID
   ),
   peerManager(peerManager),
   fileManager(fileManager),
   active(true),
   nbError(0),
   nbHash(0)
{
   this->initUnactiveTimer();
}

PeerMessageSocket::PeerMessageSocket(
   PeerManager* peerManager,
   QSharedPointer<FM::IFileManager> fileManager,
   const Common::Hash& remotePeerID,
   const QHostAddress& address,
   quint16 port
) :
   MessageSocket(
      new PeerMessageSocket::Logger(),
      address,
      port,
      peerManager->getSelf()->getID(),
      remotePeerID
   ),
   peerManager(peerManager),
   fileManager(fileManager),
   active(true),
   nbError(0),
   nbHash(0)
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

void PeerMessageSocket::send(Common::MessageHeader::MessageType type, const google::protobuf::Message& message)
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
   // Some transactions (like GET_HASHES) can go for a long time, we have to restart the timer even for an active connection.
   this->inactiveTimer.start();

   if (this->active)
      return;

   L_DEBU(QString("Socket[%1] set to active >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>").arg(this->num));

   this->active = true;
}

/**
  * Must be called when a transaction is terminated.
  */
void PeerMessageSocket::finished(bool closeTheSocket)
{
   if (!this->active)
      return;

   L_DEBU(
      QString("Socket[%1] set to idle%2<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<")
         .arg(this->num).arg(closeTheSocket ? " (socket forced to close) " : " ")
   );

   if (closeTheSocket)
   {
      L_WARN("Socket forced to close");
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
  * The idle timer watches the socket only while it is listening for messages.
  * 'stopListening()' means a raw data stream is about to be exchanged (see 'onNewMessage(..)' for
  * 'CORE_GET_CHUNKS' and 'GetChunksResult::newMessage(..)'): the socket is then given to another thread
  * which may legitimately use it for a long time. A chunk is 'Common::Constants::CHUNK_SIZE' bytes so a
  * transfer easily outlives 'idle_socket_timeout'. The timer must not fire during this period, first
  * because the socket isn't idle at all and second because 'close()' would touch a QTcpSocket owned by
  * another thread. The transfer itself is guarded by 'socket_timeout', see 'UM::ChunksUploader::run()'
  * and 'DM::ChunkDownloader::run()'.
  */
void PeerMessageSocket::startListening()
{
   // Started before the base method: the latter may read a pending message and stop the timer again.
   this->inactiveTimer.start();

   this->MessageSocket::startListening();
}

void PeerMessageSocket::stopListening()
{
   this->inactiveTimer.stop();

   this->MessageSocket::stopListening();
}

/**
  * Only emit the 'closed(..)' signal, do not close the socket.
  */
void PeerMessageSocket::close()
{
   this->active = false;
   this->stopListening();
   emit closed(this);
}

/**
  * When we ask to the fileManager some hashes for a given file this
  * slot will be called each time a new hash is available.
  */
void PeerMessageSocket::nextAskedHash(Protos::Core::HashResult hash)
{
   this->send(Common::MessageHeader::CORE_HASH_RESULT, hash);

   if (--this->nbHash == 0)
   {
      this->currentHashesResult.clear();
      this->finished();
   }
}

void PeerMessageSocket::entriesResult(const Protos::Core::GetEntriesResult::EntryResult& result)
{
   bool resultEmpty = true;
   for (int i = 0; i < this->entriesResultsToReceive.count(); i++)
   {
      if (this->entriesResultsToReceive[i] == this->sender())
      {
         this->entriesResultMessage.mutable_results(i)->CopyFrom(result);
         this->entriesResultsToReceive[i].clear();
      }
      else if (!this->entriesResultsToReceive[i].isNull())
      {
         resultEmpty = false;
      }
   }

   if (resultEmpty)
      this->sendEntriesResultMessage();
}

/**
  * If one of the directories can't be browsed then we never send a respond.
  */
void PeerMessageSocket::entriesResultTimeout()
{
   L_DEBU("PeerMessageSocket::entriesResultTimeout()");

   bool resultEmpty = true;
   for (int i = 0; i < this->entriesResultsToReceive.count(); i++)
   {
      if (this->entriesResultsToReceive[i] == this->sender())
      {
         this->entriesResultMessage.mutable_results(i)->set_status(
            Protos::Core::GetEntriesResult::EntryResult::TIMEOUT_SCANNING_IN_PROGRESS
         );
         this->entriesResultsToReceive[i].clear();
      }
      else if (!this->entriesResultsToReceive[i].isNull())
      {
         resultEmpty = false;
      }
   }

   if (resultEmpty)
      this->sendEntriesResultMessage();
}

void PeerMessageSocket::onNewMessage(const Common::Message& message)
{
   switch (message.getHeader().getType())
   {
   case Common::MessageHeader::CORE_GET_ENTRIES:
      {
         if (!this->entriesResultsToReceive.isEmpty())
            return;

         const Protos::Core::GetEntries& getEntries = message.getMessage<Protos::Core::GetEntries>();

         for (int i = 0; i < getEntries.dirs().entries_size(); i++)
         {
            QSharedPointer<FM::IGetEntriesResult> result =
               this->fileManager->getScannedEntries(
                  getEntries.dirs().entries(i),
                  getEntries.nb_max_hashes_per_entry() > 0 ?
                       getEntries.nb_max_hashes_per_entry()
                     : std::numeric_limits<int>::max()
               );
            connect(
               result.data(),
               &FM::IGetEntriesResult::result,
               this,
               &PeerMessageSocket::entriesResult,
               Qt::DirectConnection
            );
            connect(
               result.data(),
               &FM::IGetEntriesResult::timeout,
               this,
               &PeerMessageSocket::entriesResultTimeout,
               Qt::DirectConnection
            );
            this->entriesResultsToReceive << result;
            this->entriesResultMessage.add_results();
         }

         // Add the root directories if asked.
         if (getEntries.dirs().entries_size() == 0 || getEntries.get_roots())
         {
            auto entries = this->entriesResultMessage.add_results()->mutable_entries();
            entries->CopyFrom(this->fileManager->getEntries());

            // Remove local absolute paths.
            for (int i = 0; i < entries->entries_size(); ++i)
               entries->mutable_entries(i)->mutable_shared_entry()->mutable_path()->clear();
         }

         if (this->entriesResultsToReceive.isEmpty())
            this->sendEntriesResultMessage();
         else
            foreach (QSharedPointer<FM::IGetEntriesResult> entriesResult, this->entriesResultsToReceive)
               entriesResult->start();
      }
      break;

   case Common::MessageHeader::CORE_GET_ENTRIES_RESULT:
      this->finished();
      break;

   case Common::MessageHeader::CORE_GET_HASHES:
      {
         const Protos::Core::GetHashes& getHashes = message.getMessage<Protos::Core::GetHashes>();

         this->currentHashesResult = this->fileManager->getHashes(getHashes.file());
         connect(
            this->currentHashesResult.data(),
            &FM::IGetHashesResult::nextHash,
            this,
            &PeerMessageSocket::nextAskedHash,
            Qt::QueuedConnection
         );
         Protos::Core::GetHashesResult res = this->currentHashesResult->start();
         this->nbHash = res.nb_hash();

         this->send(Common::MessageHeader::CORE_GET_HASHES_RESULT, res);

         // 'nbHash' can be zero even with a status OK, when the remote peer already knows every hash of the
         // file. 'nextAskedHash(..)' is then never called and would leave the socket active forever.
         if (res.status() != Protos::Core::GetHashesResult_Status_OK || this->nbHash == 0)
         {
            this->currentHashesResult.clear();
            this->finished();
         }
      }
      break;

   case Common::MessageHeader::CORE_GET_HASHES_RESULT:
      {
         const Protos::Core::GetHashesResult& getHashesResult = message.getMessage<Protos::Core::GetHashesResult>();
         this->nbHash = getHashesResult.nb_hash();

         // No 'CORE_HASH_RESULT' will follow, the transaction is already over.
         if (getHashesResult.status() != Protos::Core::GetHashesResult_Status_OK || this->nbHash == 0)
            this->finished();
      }
      break;

   case Common::MessageHeader::CORE_HASH_RESULT:
      {
         if (--this->nbHash == 0)
            this->finished();
      }
      break;

   case Common::MessageHeader::CORE_GET_CHUNKS:
      {
         const Protos::Core::GetChunks& getChunksMessage = message.getMessage<Protos::Core::GetChunks>();

         QList<GetChunkParams> chunksParams;
         Protos::Core::GetChunksResult chunksResult;

         // TODO: implements:
         // - 'GetChunkResult.ALREADY_DOWNLOADING'
         // - 'GetChunkResult.TOO_MANY_CONNECTIONS'
         // - 'GetChunkResult.DONT_HAVE_DATA_FROM_OFFSET'

         for (int i = 0; i < getChunksMessage.chunks_size(); i++)
         {
            const auto& chunkNeeded = getChunksMessage.chunks(i);
            const Common::Hash hash(chunkNeeded.hash().hash());
            Protos::Core::GetChunksResult::ChunkResult* result = chunksResult.add_results();
            if (hash.isNull())
            {
               result->set_status(Protos::Core::GetChunksResult::ChunkResult::ERROR_HASH_NULL);
            }
            else
            {
               QSharedPointer<FM::IChunk> chunk = this->fileManager->getChunk(hash);
               if (chunk.isNull())
               {
                  result->set_status(Protos::Core::GetChunksResult::ChunkResult::DONT_HAVE);
               }
               else
               {
                  // A single snapshot for both the answer and the uploader: 'getKnownBytes()' may grow while
                  // the chunk is being uploaded (it may be downloaded at the same time) but the peer reads
                  // exactly the announced amount, no more may be sent. See 'UM::ChunksUploader::run()'.
                  const int knownBytes = chunk->getKnownBytes();
                  result->set_status(Protos::Core::GetChunksResult::ChunkResult::OK);
                  result->set_chunk_size(knownBytes);
                  chunksParams << GetChunkParams(chunk, chunkNeeded.offset(), knownBytes, chunkNeeded.file_bytes_owned());
               }
            }
         }

         // The answer can't be a status OK if nothing is able to send the data: the remote peer would read
         // everything following it as chunk data and no error message could be sent anymore.
         if (!chunksParams.empty() && !this->peerManager->isReadyToSendChunks())
         {
            L_ERRO("PeerMessageSocket::onNewMessage(..): no slot connected to the signal 'PeerManager::getChunks(..)'");
            chunksParams.clear();
         }

         if (!chunksParams.empty())
            chunksResult.set_status(Protos::Core::GetChunksResult::OK);
         else
            chunksResult.set_status(Protos::Core::GetChunksResult::ERROR_UNKNOWN);

         this->send(Common::MessageHeader::CORE_GET_CHUNKS_RESULT, chunksResult);

         if (!chunksParams.empty())
         {
            this->stopListening();
            emit getChunks(chunksParams, this);
         }
         else
         {
            this->finished();
         }

         // const Common::Hash hash(getChunksMessage.chunks().hash());
         // if (hash.isNull())
         // {
         //    L_WARN("GET_CHUNK: Chunk null");
         //    this->finished(true);
         //    break;
         // }

         // // TODO: implements 'GetChunkResult.ALREADY_DOWNLOADING', 'GetChunkResult.TOO_MANY_CONNECTIONS' and 'GetChunkResult.DONT_HAVE_DATA_FROM_OFFSET'
         // QSharedPointer<FM::IChunk> chunk = this->fileManager->getChunk(hash);
         // if (chunk.isNull())
         // {
         //    // TODO
         //    // Protos::Core::GetChunksResult result;
         //    // result.set_status(Protos::Core::GetChunksResult::ChunkResult::DONT_HAVE);
         //    // this->send(Common::MessageHeader::CORE_GET_CHUNK_RESULT, result);
         //    // this->finished();

         //    L_WARN(QString("GET_CHUNK: Chunk unknown: %1").arg(hash.toStr()));
         // }
         // else
         // {
         //    Protos::Core::GetChunksResult result;
         //    result.set_status(Protos::Core::GetChunksResult::OK);
         //    result.set_chunk_size(chunk->getKnownBytes());
         //    this->send(Common::MessageHeader::CORE_GET_CHUNK_RESULT, result);

         //    this->stopListening();

         //    emit getChunk(chunk, getChunkMessage.offset(), this);
         // }
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
   connect(&this->inactiveTimer, &QTimer::timeout, this, &PeerMessageSocket::close);
   // Not started here: 'startListening()' owns the timer and is called right after the socket is built,
   // see 'ConnectionPool::addNewSocket(..)'.
}

void PeerMessageSocket::sendEntriesResultMessage()
{
   this->send(Common::MessageHeader::CORE_GET_ENTRIES_RESULT, this->entriesResultMessage);
   this->entriesResultMessage.Clear();
   this->entriesResultsToReceive.clear();
   this->finished();
}
