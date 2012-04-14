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
  
#include <priv/ChunkDownload.h>
using namespace DM;

#include <QElapsedTimer>

#include <Common/Settings.h>
#include <Core/FileManager/Exceptions.h>
#include <Core/PeerManager/IPeer.h>

#include <priv/Log.h>

/**
  * @class DM::ChunkDownload
  *
  * A class to download a file chunk. A ChunkDownload can exist only if we know its hash.
  * It can be created when a new FileDownload is added for each chunk known in the given entry or when a FileDownload receive a hash.
  */

const int ChunkDownload::MINIMUM_DELTA_TIME_TO_COMPUTE_SPEED(100); // [ms]

ChunkDownload::ChunkDownload(LinkedPeers& linkedPeers, OccupiedPeers& occupiedPeersDownloadingChunk, Common::TransferRateCalculator& transferRateCalculator, Common::ThreadPool& threadPool, Common::Hash chunkHash) :
   linkedPeers(linkedPeers),
   occupiedPeersDownloadingChunk(occupiedPeersDownloadingChunk),
   transferRateCalculator(transferRateCalculator),
   threadPool(threadPool),
   chunkHash(chunkHash),
   socket(0),
   downloading(false),
   closeTheSocket(false),
   lastTransfertStatus(QUEUED),
   mainThread(QThread::currentThread()),
   mutex(QMutex::Recursive)
{
   L_DEBU(QString("New ChunkDownload : %1").arg(this->chunkHash.toStr()));
}

ChunkDownload::~ChunkDownload()
{
   this->stop();

   for (QListIterator<PM::IPeer*> i(this->peers); i.hasNext();)
      this->linkedPeers.rmLink(i.next());

   L_DEBU(QString("ChunkDownload deleted : %1").arg(this->chunkHash.toStr()));
}

/**
  * Return true if the chunk was downloading.
  */
void ChunkDownload::stop()
{
   if (this->downloading)
   {
      this->mutex.lock();
      this->downloading = false;
      this->mutex.unlock();

      this->threadPool.wait(this);

      this->downloadingEnded();
   }
}

Common::Hash ChunkDownload::getHash() const
{
   return this->chunkHash;
}

void ChunkDownload::addPeer(PM::IPeer* peer)
{
   Q_ASSERT(peer);

   QMutexLocker locker(&this->mutex);

   if (!this->peers.contains(peer))
   {
      this->peers << peer;
      this->linkedPeers.addLink(peer);
      emit numberOfPeersChanged();
      this->occupiedPeersDownloadingChunk.newPeer(peer);
   }
}

void ChunkDownload::rmPeer(PM::IPeer* peer)
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

void ChunkDownload::init(QThread* thread)
{
   this->socket->moveToThread(thread);
}

void ChunkDownload::run()
{
   int deltaRead = 0;
   QElapsedTimer timer;
   timer.start();
   this->lastTransfertStatus = QUEUED;

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
               this->lastTransfertStatus = TRANSFERT_ERROR;
               break;
            }
            continue;
         }
         else if (bytesRead == -1)
         {
            L_WARN(QString("Socket : cannot receive data : %1").arg(this->chunk->toStringLog()));
            this->closeTheSocket = true;
            this->lastTransfertStatus = TRANSFERT_ERROR;
            break;
         }

         deltaRead += bytesRead;
         bytesToWrite += bytesRead;

         if (timer.elapsed() > TIME_PERIOD_CHOOSE_ANOTHER_PEER)
         {
            this->currentDownloadingPeer->setSpeed(deltaRead / timer.elapsed() * 1000);
            L_DEBU(QString("Check for a better peer for the chunk: %1, current peer: %2 ..").arg(this->chunk->toStringLog()).arg(this->currentDownloadingPeer->toStringLog()));
            timer.start();
            deltaRead = 0;

            // If a another peer exists and its speed is greater than our by a factor 'switch_to_another_peer_factor'
            // then we will try to switch to this peer.
            PM::IPeer* peer = this->getTheFastestFreePeer();
            if (
               peer &&
               peer != this->currentDownloadingPeer &&
               peer->getSpeed() / SETTINGS.get<double>("switch_to_another_peer_factor") > this->currentDownloadingPeer->getSpeed()
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
   catch(FM::UnableToOpenFileInWriteModeException)
   {
      L_DEBU("UnableToOpenFileInWriteModeException");
      this->closeTheSocket = true;
      this->lastTransfertStatus = UNABLE_TO_OPEN_THE_FILE;
   }
   catch(FM::IOErrorException&)
   {
      L_DEBU("IOErrorException");
      this->closeTheSocket = true;
      this->lastTransfertStatus = FILE_IO_ERROR;
   }
   catch (FM::ChunkDeletedException&)
   {
      L_DEBU("ChunkDeletedException");
      this->closeTheSocket = true;
      this->lastTransfertStatus = FILE_NON_EXISTENT;
   }
   catch (FM::TryToWriteBeyondTheEndOfChunkException&)
   {
      L_DEBU("TryToWriteBeyondTheEndOfChunkException");
      this->closeTheSocket = true;
      this->lastTransfertStatus = GOT_TOO_MUCH_DATA;
   }
   catch (FM::hashMissmatchException)
   {
      const quint32 BAN_DURATION = SETTINGS.get<quint32>("ban_duration_corrupted_data");
      L_USER(QString(tr("Corrupted data received for the file \"%1\" from peer %2. Peer banned for %3 ms")).arg(this->chunk->getBasePath()).arg(this->currentDownloadingPeer->getNick()).arg(BAN_DURATION));
      /*: A reason why the user has been banned */
      this->currentDownloadingPeer->ban(BAN_DURATION, tr("Has sent corrupted data"));
      this->closeTheSocket = true;
      this->lastTransfertStatus = HASH_MISSMATCH;
   }

   if (timer.elapsed() > MINIMUM_DELTA_TIME_TO_COMPUTE_SPEED)
      this->currentDownloadingPeer->setSpeed(deltaRead / timer.elapsed() * 1000);

   this->socket->setReadBufferSize(0);
   this->socket->moveToThread(this->mainThread);
}

void ChunkDownload::finished()
{
   if (this->downloading)
      this->downloadingEnded();
}

void ChunkDownload::setChunk(QSharedPointer<FM::IChunk> chunk)
{
   this->chunk = chunk;
   this->chunk->setHash(this->chunkHash);
}

QSharedPointer<FM::IChunk> ChunkDownload::getChunk() const
{
   return this->chunk;
}

void ChunkDownload::setPeerSource(PM::IPeer* peer, bool informOccupiedPeers)
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
int ChunkDownload::isReadyToDownload()
{
   if (this->peers.isEmpty() || this->downloading || (!this->chunk.isNull() && this->chunk->isComplete()))
      return 0;

   return this->getNumberOfFreePeer();
}

bool ChunkDownload::isDownloading() const
{
   return this->downloading;
}

bool ChunkDownload::isComplete() const
{
   return !this->chunk.isNull() && this->chunk->isComplete();
}

bool ChunkDownload::isPartiallyDownloaded() const
{
   return !this->chunk.isNull() && !this->chunk->isComplete() && this->chunk->getKnownBytes() > 0;
}

bool ChunkDownload::hasAtLeastAPeer()
{
   return !this->getPeers().isEmpty();
}

/**
  * May return one of this status:
  * QUEUED (all is ok)
  * TRANSFERT_ERROR
  * UNABLE_TO_OPEN_THE_FILE
  * FILE_IO_ERROR
  * FILE_NON_EXISTENT
  * GOT_TOO_MUCH_DATA
  * HASH_MISSMATCH
  */
Status ChunkDownload::getLastTransfertStatus() const
{
   return this->lastTransfertStatus;
}

void ChunkDownload::resetLastTransfertStatus()
{
   this->lastTransfertStatus = QUEUED;
}

int ChunkDownload::getDownloadedBytes() const
{
   if (this->chunk.isNull())
      return 0;

   return this->chunk->getKnownBytes();
}

/**
  * @remarks This method may remove dead peers from the list.
  */
QList<Common::Hash> ChunkDownload::getPeers()
{
   QMutexLocker locker(&this->mutex);

   QList<Common::Hash> peerIDs;
   bool isTheNmberOfPeersHasChanged = false;
   for (QMutableListIterator<PM::IPeer*> i(this->peers); i.hasNext();)
   {
      PM::IPeer* peer = i.next();
      if (peer->isAvailable())
         peerIDs << peer->getID();
      else
      {
         i.remove();
         this->linkedPeers.rmLink(peer);
         isTheNmberOfPeersHasChanged = true;
      }
   }
   if (isTheNmberOfPeersHasChanged)
      emit numberOfPeersChanged();
   return peerIDs;
}

/**
  * Tell the chunkDownload to download the chunk from one of its peer.
  * @return the choosen peer if the downloading has been started else return 0.
  */
PM::IPeer* ChunkDownload::startDownloading()
{
   if (this->chunk.isNull())
   {
      L_WARN(QString("Unable to download without the chunk. Hash : %1").arg(this->chunkHash.toStr()));
      return 0;
   }

   this->currentDownloadingPeer = this->getTheFastestFreePeer();
   if (!this->currentDownloadingPeer)
      return 0;

   L_DEBU(QString("Starting downloading a chunk : %1 from %2").arg(this->chunk->toStringLog()).arg(this->currentDownloadingPeer->getID().toStr()));

   this->downloading = true;
   emit downloadStarted();

   this->occupiedPeersDownloadingChunk.setPeerAsOccupied(this->currentDownloadingPeer);

   Protos::Core::GetChunk getChunkMess;
   getChunkMess.mutable_chunk()->set_hash(this->chunkHash.getData(), Common::Hash::HASH_SIZE);
   getChunkMess.set_offset(this->chunk->getKnownBytes());
   this->getChunkResult = this->currentDownloadingPeer->getChunk(getChunkMess);
   connect(this->getChunkResult.data(), SIGNAL(result(const Protos::Core::GetChunkResult&)), this, SLOT(result(const Protos::Core::GetChunkResult&)), Qt::DirectConnection);
   connect(this->getChunkResult.data(), SIGNAL(stream(QSharedPointer<PM::ISocket>)), this, SLOT(stream(QSharedPointer<PM::ISocket>)), Qt::DirectConnection);
   connect(this->getChunkResult.data(), SIGNAL(timeout()), this, SLOT(getChunkTimeout()), Qt::DirectConnection);

   this->getChunkResult->start();
   return this->currentDownloadingPeer;
}

void ChunkDownload::tryToRemoveItsIncompleteFile()
{
   if (!this->chunk.isNull())
      this->chunk->removeItsIncompleteFile();
}

void ChunkDownload::reset()
{
   this->chunk.clear();
}

void ChunkDownload::result(const Protos::Core::GetChunkResult& result)
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
      if (!result.has_chunk_size())
      {
         L_ERRO(QString("Message 'GetChunkResult' doesn't contain the size of the chunk : %1. Download aborted.").arg(this->chunk->getHash().toStr()));
         this->closeTheSocket = true;
         this->downloadingEnded();
      }
      else
      {
         this->chunkSize = result.chunk_size();
      }
   }
}

void ChunkDownload::stream(QSharedPointer<PM::ISocket> socket)
{
   this->socket = socket;
   this->socket->setReadBufferSize(SETTINGS.get<quint32>("socket_buffer_size"));
   this->threadPool.run(this);
}

void ChunkDownload::getChunkTimeout()
{
   L_WARN("Timeout from GetChunkResult, Download aborted.");
   this->downloadingEnded();
}

void ChunkDownload::downloadingEnded()
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

   this->occupiedPeersDownloadingChunk.setPeerAsFree(currentPeer);
}

/**
  * Get the fastest free peer, may remove dead peers.
  */
PM::IPeer* ChunkDownload::getTheFastestFreePeer()
{
   QMutexLocker locker(&this->mutex);

   PM::IPeer* current = 0;   
   bool isTheNmberOfPeersHasChanged = false;
   for (QMutableListIterator<PM::IPeer*> i(this->peers); i.hasNext();)
   {
      PM::IPeer* peer = i.next();
      if (!peer->isAvailable())
      {
         i.remove();
         this->linkedPeers.rmLink(peer);
         isTheNmberOfPeersHasChanged = true;
      }
      else if (this->occupiedPeersDownloadingChunk.isPeerFree(peer) && (!current || peer->getSpeed() > current->getSpeed()))
         current = peer;
   }

   if (isTheNmberOfPeersHasChanged)
      emit numberOfPeersChanged();

   return current;
}

int ChunkDownload::getNumberOfFreePeer()
{
   QMutexLocker locker(&this->mutex);

   int n = 0;
   bool isTheNmberOfPeersHasChanged = false;
   for (QMutableListIterator<PM::IPeer*> i(this->peers); i.hasNext();)
   {
      PM::IPeer* peer = i.next();
      if (!peer->isAvailable())
      {
         i.remove();
         this->linkedPeers.rmLink(peer);
         isTheNmberOfPeersHasChanged = true;
      }
      else if (this->occupiedPeersDownloadingChunk.isPeerFree(peer))
         n++;
   }

   if (isTheNmberOfPeersHasChanged)
      emit numberOfPeersChanged();

   return n;
}
