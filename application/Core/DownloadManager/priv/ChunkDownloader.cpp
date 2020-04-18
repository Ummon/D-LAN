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

#include <priv/ChunkDownloader.h>
using namespace DM;

#include <QElapsedTimer>

#include <Common/Settings.h>
#include <Core/FileManager/Exceptions.h>
#include <Core/PeerManager/IPeer.h>

#include <priv/Log.h>

/**
  * @class DM::ChunkDownloader
  *
  * A class to download a file chunk. A ChunkDownloader can exist only if we know its hash.
  * It can be created when a new FileDownload is added for each chunk known in the given entry or when a FileDownload receive a hash.
  */

const int ChunkDownloader::MINIMUM_DELTA_TIME_TO_COMPUTE_SPEED(100); // [ms]

ChunkDownloader::ChunkDownloader(LinkedPeers& linkedPeers, OccupiedPeers& occupiedPeersDownloadingChunk, Common::TransferRateCalculator& transferRateCalculator, Common::ThreadPool& threadPool, Common::Hash chunkHash) :
   linkedPeers(linkedPeers),
   occupiedPeersDownloadingChunk(occupiedPeersDownloadingChunk),
   transferRateCalculator(transferRateCalculator),
   threadPool(threadPool),
   chunkHash(chunkHash),
   socket(0),
   downloading(false),
   closeTheSocket(false),
   lastTransferStatus(QUEUED),
   mainThread(QThread::currentThread()),
   mutex(QMutex::Recursive)
{
   Q_ASSERT(!chunkHash.isNull());
   L_DEBU(QString("New ChunkDownloader : %1").arg(this->chunkHash.toStr()));
}

ChunkDownloader::~ChunkDownloader()
{
   this->stop();

   for (QListIterator<PM::IPeer*> i(this->peers); i.hasNext();)
      this->linkedPeers.rmLink(i.next());

   L_DEBU(QString("ChunkDownloader deleted : %1").arg(this->chunkHash.toStr()));
}

/**
  * Return true if the chunk was downloading.
  */
void ChunkDownloader::stop()
{
   if (this->downloading)
   {
      this->mutex.lock();
      this->downloading = false;
      this->mutex.unlock();

      this->threadPool.wait(this->getWeakRef());

      this->downloadingEnded();
   }
}

Common::Hash ChunkDownloader::getHash() const
{
   return this->chunkHash;
}

void ChunkDownloader::addPeer(PM::IPeer* peer)
{
   Q_ASSERT(peer);

   QMutexLocker locker(&this->mutex);

   if (this->isComplete())
      return;

   if (!this->peers.contains(peer))
   {
      this->peers << peer;
      this->linkedPeers.addLink(peer);
      emit numberOfPeersChanged();
      this->occupiedPeersDownloadingChunk.newPeer(peer);
   }
}

void ChunkDownloader::rmPeer(PM::IPeer* peer)
{
   Q_ASSERT(peer);

   QMutexLocker locker(&this->mutex);

   if (this->peers.isEmpty())
      return;

   if (this->peers.removeOne(peer))
   {
      this->linkedPeers.rmLink(peer);
      emit numberOfPeersChanged();
   }
}

void ChunkDownloader::init(QThread* thread)
{
   this->socket->moveToThread(thread);
}

void ChunkDownloader::run()
{
   int deltaRead = 0;
   QElapsedTimer timer;
   timer.start();
   this->lastTransferStatus = QUEUED;

   try
   {
      QSharedPointer<FM::IDataWriter> writer = this->chunk->getDataWriter();

      static const int SOCKET_TIMEOUT = SETTINGS.get<quint32>("socket_timeout");
      static const int TIME_PERIOD_CHOOSE_ANOTHER_PEER = 1000.0 * SETTINGS.get<double>("time_recheck_chunk_factor") * SETTINGS.get<quint32>("chunk_size") / SETTINGS.get<quint32>("lan_speed");

      static const int BUFFER_SIZE = SETTINGS.get<quint32>("buffer_size_writing");
      char buffer[BUFFER_SIZE];

      const int initialKnownBytes = this->chunk->getKnownBytes();
      int bytesToRead = this->chunkSize - initialKnownBytes;
      int bytesToWrite = 0;
      int bytesWritten = 0;

      forever
      {
         this->mutex.lock();
         if (!this->downloading)
         {
            L_DEBU(QString("Downloading aborted, chunk : %1%2").arg(this->chunk->toStringLog()).arg(this->chunk->isComplete() ? "" : " Not complete!"));
            this->closeTheSocket = true; // Because some garbage from the remote uploader will continue to come in this socket.
            this->mutex.unlock();
            break;
         }
         this->mutex.unlock();

         int bytesRead = this->socket->read(buffer + bytesToWrite, bytesToRead < BUFFER_SIZE - bytesToWrite ? bytesToRead : BUFFER_SIZE - bytesToWrite);
         bytesToRead -= bytesRead;

         if (bytesRead == 0)
         {
            if (!this->socket->waitForReadyRead(SOCKET_TIMEOUT))
            {
               L_WARN(QString("Connection dropped, error = %1, bytesAvailable = %2").arg(socket->errorString()).arg(socket->bytesAvailable()));
               this->closeTheSocket = true;
               this->lastTransferStatus = TRANSFER_ERROR;
               break;
            }
            continue;
         }
         else if (bytesRead == -1)
         {
            L_WARN(QString("Socket : cannot receive data : %1").arg(this->chunk->toStringLog()));
            this->closeTheSocket = true;
            this->lastTransferStatus = TRANSFER_ERROR;
            break;
         }

         deltaRead += bytesRead;
         bytesToWrite += bytesRead;

         if (timer.elapsed() > TIME_PERIOD_CHOOSE_ANOTHER_PEER)
         {
            this->currentDownloadingPeer->setSpeed(deltaRead / timer.elapsed() * 1000);
            L_DEBU(QString("Check for a better peer for the chunk: %1, current peer: %2 . . .").arg(this->chunk->toStringLog()).arg(this->currentDownloadingPeer->toStringLog()));
            timer.start();
            deltaRead = 0;

            // If a another peer exists and its speed is greater than our by a factor 'switch_to_another_peer_factor'
            // then we will try to switch to this peer.
            static const double SWITCH_TO_ANOTHER_PEER_FACTOR = SETTINGS.get<double>("switch_to_another_peer_factor");
            PM::IPeer* peer = this->getTheFastestFreePeer();
            if (
               peer &&
               peer != this->currentDownloadingPeer &&
               peer->getSpeed() / SWITCH_TO_ANOTHER_PEER_FACTOR > this->currentDownloadingPeer->getSpeed()
            )
            {
               L_DEBU(QString("Switch to a better peer: %1").arg(peer->toStringLog()));
               this->closeTheSocket = true; // We ask to close the socket to avoid to get garbage data.
               break;
            }
         }

         // If the buffer is full or there is no more byte to read.
         if (bytesToWrite == BUFFER_SIZE || bytesToRead == 0)
         {
            writer->write(buffer, bytesToWrite);
            bytesWritten += bytesToWrite;
            bytesToWrite = 0;
         }

         this->transferRateCalculator.addData(bytesRead);

         if (initialKnownBytes + bytesWritten >= this->chunkSize)
            break;
      }
   }
   catch (FM::FileResetException)
   {
      L_DEBU("FileResetException");
      this->closeTheSocket = true;
      this->lastTransferStatus = FILE_NON_EXISTENT;
   }
   catch (FM::ChunkDataUnknownException)
   {
      L_DEBU("ChunkDataUnknownException");
      this->closeTheSocket = true;
      this->lastTransferStatus = UNABLE_TO_OPEN_THE_FILE;
   }
   catch (FM::UnableToOpenFileInWriteModeException)
   {
      L_DEBU("UnableToOpenFileInWriteModeException");
      this->closeTheSocket = true;
      this->lastTransferStatus = UNABLE_TO_OPEN_THE_FILE;
   }
   catch (FM::IOErrorException&)
   {
      L_DEBU("IOErrorException");
      this->closeTheSocket = true;
      this->lastTransferStatus = FILE_IO_ERROR;
   }
   catch (FM::ChunkDeletedException&)
   {
      L_DEBU("ChunkDeletedException");
      this->closeTheSocket = true;
      this->lastTransferStatus = FILE_NON_EXISTENT;
   }
   catch (FM::TryToWriteBeyondTheEndOfChunkException&)
   {
      L_DEBU("TryToWriteBeyondTheEndOfChunkException");
      this->closeTheSocket = true;
      this->lastTransferStatus = GOT_TOO_MUCH_DATA;
   }
   catch (FM::hashMismatchException)
   {
      static const quint32 BLOCK_DURATION = SETTINGS.get<quint32>("block_duration_corrupted_data");
      L_USER(QString(tr("Corrupted data received for the file \"%1\" from peer %2. Peer blocked for %3 ms")).arg(this->chunk->getFilePath()).arg(this->currentDownloadingPeer->getNick()).arg(BLOCK_DURATION));
      /*: A reason why the user has been blocked */
      this->currentDownloadingPeer->block(BLOCK_DURATION, tr("Has sent corrupted data"));
      this->closeTheSocket = true;
      this->lastTransferStatus = HASH_MISMATCH;
   }

   if (timer.elapsed() > MINIMUM_DELTA_TIME_TO_COMPUTE_SPEED)
      this->currentDownloadingPeer->setSpeed(deltaRead / timer.elapsed() * 1000);

   this->socket->setReadBufferSize(0);
   this->socket->moveToThread(this->mainThread);
}

void ChunkDownloader::finished()
{
   if (this->downloading)
      this->downloadingEnded();
}

void ChunkDownloader::setChunk(const QSharedPointer<FM::IChunk>& chunk)
{
   this->chunk = chunk;
   this->chunk->setHash(this->chunkHash);
}

QSharedPointer<FM::IChunk> ChunkDownloader::getChunk() const
{
   return this->chunk;
}

void ChunkDownloader::setPeerSource(PM::IPeer* peer, bool informOccupiedPeers)
{
   QMutexLocker locker(&this->mutex);
   if (!this->peers.contains(peer))
   {
      this->peers << peer;
      this->linkedPeers.addLink(peer);
      emit numberOfPeersChanged();

      if (informOccupiedPeers && peer->isAvailable())
         this->occupiedPeersDownloadingChunk.newPeer(peer);
   }
}

/**
  * To be ready :
  * - It must be have at least one peer.
  * - It isn't finished.
  * - It isn't currently downloading.
  * @return The number of free peer.
  * @remarks This method may remove dead peers from the list.
  */
int ChunkDownloader::isReadyToDownload()
{
   if (this->peers.isEmpty() || this->downloading || (!this->chunk.isNull() && this->chunk->isComplete()))
      return 0;

   return this->getNumberOfFreePeer();
}

bool ChunkDownloader::isDownloading() const
{
   return this->downloading;
}

bool ChunkDownloader::isComplete() const
{
   return !this->chunk.isNull() && this->chunk->isComplete();
}

bool ChunkDownloader::isPartiallyDownloaded() const
{
   return !this->chunk.isNull() && !this->chunk->isComplete() && this->chunk->getKnownBytes() > 0;
}

bool ChunkDownloader::hasAtLeastAPeer()
{
   return !this->getPeers().isEmpty();
}

/**
  * May return one of this status:
  * QUEUED (all is ok)
  * TRANSFER_ERROR
  * UNABLE_TO_OPEN_THE_FILE
  * FILE_IO_ERROR
  * FILE_NON_EXISTENT
  * GOT_TOO_MUCH_DATA
  * HASH_MISMATCH
  */
Status ChunkDownloader::getLastTransferStatus() const
{
   return this->lastTransferStatus;
}

void ChunkDownloader::resetLastTransferStatus()
{
   this->lastTransferStatus = QUEUED;
}

int ChunkDownloader::getDownloadedBytes() const
{
   if (this->chunk.isNull())
      return 0;

   return this->chunk->getKnownBytes();
}

/**
  * @remarks This method may remove dead peers from the list.
  */
QList<PM::IPeer*> ChunkDownloader::getPeers()
{
   QMutexLocker locker(&this->mutex);

   QList<PM::IPeer*> peers;
   peers.reserve(this->peers.size());

   bool isTheNumberOfPeersHasChanged = false;
   for (QMutableListIterator<PM::IPeer*> i(this->peers); i.hasNext();)
   {
      PM::IPeer* peer = i.next();
      if (peer->isAvailable())
         peers << peer;
      else
      {
         i.remove();
         this->linkedPeers.rmLink(peer);
         isTheNumberOfPeersHasChanged = true;
      }
   }
   if (isTheNumberOfPeersHasChanged)
      emit numberOfPeersChanged();
   return peers;
}

/**
  * Tell the ChunkDownloader to download the chunk from one of its peer.
  * @return the chosen peer if the downloading has been started else return 0.
  */
PM::IPeer* ChunkDownloader::startDownloading()
{
   if (this->chunk.isNull())
   {
      L_WARN(QString("Unable to download without the chunk. Hash : %1").arg(this->chunkHash.toStr()));
      return nullptr;
   }

   this->currentDownloadingPeer = this->getTheFastestFreePeer();
   if (!this->currentDownloadingPeer)
      return nullptr;

   Protos::Core::GetChunk getChunkMess;
   getChunkMess.mutable_chunk()->set_hash(this->chunkHash.getData(), Common::Hash::HASH_SIZE);
   getChunkMess.set_offset(this->chunk->getKnownBytes());
   this->getChunkResult = this->currentDownloadingPeer->getChunk(getChunkMess);
   if (this->getChunkResult.isNull())
      return nullptr;

   L_DEBU(QString("Starting downloading a chunk : %1 from %2").arg(this->chunk->toStringLog()).arg(this->currentDownloadingPeer->getID().toStr()));

   this->downloading = true;
   emit downloadStarted();

   this->occupiedPeersDownloadingChunk.setPeerAsOccupied(this->currentDownloadingPeer);

   connect(this->getChunkResult.data(), &PM::IGetChunkResult::result, this, &ChunkDownloader::result, Qt::DirectConnection);
   connect(this->getChunkResult.data(), &PM::IGetChunkResult::stream, this, &ChunkDownloader::stream, Qt::DirectConnection);
   connect(this->getChunkResult.data(), &PM::IGetChunkResult::timeout, this, &ChunkDownloader::getChunkTimeout, Qt::DirectConnection);

   this->getChunkResult->start();
   return this->currentDownloadingPeer;
}

void ChunkDownloader::tryToRemoveItsIncompleteFile()
{
   if (!this->chunk.isNull())
      this->chunk->removeItsIncompleteFile();
}

void ChunkDownloader::reset()
{
   this->chunk.clear();
}

void ChunkDownloader::result(const Protos::Core::GetChunkResult& result)
{
   if (result.status() != Protos::Core::GetChunkResult::OK)
   {
      L_WARN(QString("Status error from GetChunkResult : %1. Download aborted.").arg(result.status()));
      if (this->peers.removeOne(this->currentDownloadingPeer))
      {
         this->linkedPeers.rmLink(this->currentDownloadingPeer);
         emit numberOfPeersChanged();
      }
      this->downloadingEnded();
   }
   else
   {
      if (result.chunk_size() == 0)
      {
         L_ERRO(QString("Message 'GetChunkResult' doesn't contain the size of the chunk: %1. Download aborted.").arg(this->chunk->getHash().toStr()));
         this->closeTheSocket = true;
         this->downloadingEnded();
      }
      else
      {
         this->chunkSize = result.chunk_size();
      }
   }
}

void ChunkDownloader::stream(const QSharedPointer<PM::ISocket>& socket)
{
   this->socket = socket;
   static const quint32 SOCKET_BUFFER_SIZE = SETTINGS.get<quint32>("socket_buffer_size");
   this->socket->setReadBufferSize(SOCKET_BUFFER_SIZE);
   this->threadPool.run(this->getWeakRef());
}

void ChunkDownloader::getChunkTimeout()
{
   L_WARN("Timeout from GetChunkResult, Download aborted.");
   this->downloadingEnded();
}

void ChunkDownloader::downloadingEnded()
{
   L_DEBU(QString("Downloading ended, chunk : %1%2").arg(this->chunk->toStringLog()).arg(this->chunk->isComplete() ? "" : " Not complete!"));

   if (!this->socket.isNull())
      this->socket.clear();

   this->getChunkResult->setStatus(this->closeTheSocket);
   this->closeTheSocket = false;
   this->getChunkResult.clear();

   this->downloading = false;
   emit downloadFinished();

   // occupiedPeersDownloadingChunk can relaunch the download, so we have to set this->currentDownloadingPeer to 0 before.
   PM::IPeer* currentPeer = this->currentDownloadingPeer;
   this->currentDownloadingPeer = 0;

   // When a chunk is finished we don't care to know the associated peers.
   if (this->isComplete())
      this->peers.clear();

   this->occupiedPeersDownloadingChunk.setPeerAsFree(currentPeer);
}

/**
  * Get the fastest free peer, may remove dead peers.
  */
PM::IPeer* ChunkDownloader::getTheFastestFreePeer()
{
   QMutexLocker locker(&this->mutex);

   PM::IPeer* current = nullptr;
   bool isTheNumberOfPeersHasChanged = false;
   for (QMutableListIterator<PM::IPeer*> i(this->peers); i.hasNext();)
   {
      PM::IPeer* peer = i.next();
      if (!peer->isAvailable())
      {
         i.remove();
         this->linkedPeers.rmLink(peer);
         isTheNumberOfPeersHasChanged = true;
      }
      else if (this->occupiedPeersDownloadingChunk.isPeerFree(peer) && (!current || peer->getSpeed() > current->getSpeed()))
         current = peer;
   }

   if (isTheNumberOfPeersHasChanged)
      emit numberOfPeersChanged();

   return current;
}

int ChunkDownloader::getNumberOfFreePeer()
{
   QMutexLocker locker(&this->mutex);

   int n = 0;
   bool isTheNumberOfPeersHasChanged = false;
   for (QMutableListIterator<PM::IPeer*> i(this->peers); i.hasNext();)
   {
      PM::IPeer* peer = i.next();
      if (!peer->isAvailable())
      {
         i.remove();
         this->linkedPeers.rmLink(peer);
         isTheNumberOfPeersHasChanged = true;
      }
      else if (this->occupiedPeersDownloadingChunk.isPeerFree(peer))
         n++;
   }

   if (isTheNumberOfPeersHasChanged)
      emit numberOfPeersChanged();

   return n;
}
