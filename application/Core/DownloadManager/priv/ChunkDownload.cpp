#include <priv/ChunkDownload.h>
using namespace DM;

#include <QElapsedTimer>

#include <Common/Settings.h>
#include <Core/FileManager/Exceptions.h>
#include <Core/PeerManager/IPeer.h>

#include <priv/Log.h>

/**
  * @class ChunkDownload
  * A class to download a file chunk. A ChunkDownload can exist only if we know its hash.
  * It can be created when a new FileDownload is added for each chunk known in the given entry or when a FileDownload receive a hash.
  */

ChunkDownload::ChunkDownload(QSharedPointer<PM::IPeerManager> peerManager, OccupiedPeers& occupiedPeersDownloadingChunk, Common::Hash chunkHash) :
   SOCKET_TIMEOUT(SETTINGS.get<quint32>("socket_timeout")),
   peerManager(peerManager),
   occupiedPeersDownloadingChunk(occupiedPeersDownloadingChunk),
   chunkHash(chunkHash),
   socket(0),
   downloading(false),
   networkTransferError(false),
   mutex(QMutex::Recursive)
{
   L_DEBU(QString("New ChunkDownload : %1").arg(this->chunkHash.toStr()));
   connect(this, SIGNAL(finished()), this, SLOT(downloadingEnded()), Qt::QueuedConnection);
   this->mainThread = QThread::currentThread();
}

ChunkDownload::~ChunkDownload()
{
   disconnect(this, SIGNAL(finished()), this, SLOT(downloadingEnded()));

   if (this->downloading)
   {
      this->mutex.lock();
      this->downloading = false;
      this->mutex.unlock();

      this->wait();
      this->downloadingEnded();
   }

   if (!this->chunk.isNull())
   {
      this->chunk->removeItsFile(); // (only if incomplete).
   }
}

int ChunkDownload::getDownloadRate() const
{
   return this->transferRateCalculator.getTransferRate();
}

Common::Hash ChunkDownload::getHash() const
{
   return this->chunkHash;
}

void ChunkDownload::addPeerID(const Common::Hash& peerID)
{
   QMutexLocker lock(&this->mutex);
   PM::IPeer* peer = this->peerManager->getPeer(peerID);
   if (peer && !this->peers.contains(peer))
   {
      this->peers << peer;
      this->occupiedPeersDownloadingChunk.newPeer(peer);
   }
}

void ChunkDownload::rmPeerID(const Common::Hash& peerID)
{
   QMutexLocker lock(&this->mutex);
   PM::IPeer* peer = this->peerManager->getPeer(peerID);
   if (peer)
      this->peers.removeOne(peer);
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
   QMutexLocker lock(&this->mutex);
   if (!this->peers.contains(peer))
   {
      this->peers << peer;

      if (informOccupiedPeers)
         this->occupiedPeersDownloadingChunk.newPeer(peer);
   }
}

/**
  * To be ready :
  * - It must be have at least one peer.
  * - It isn't finished.
  * - It isn't currently downloading.
  * @remarks This method may remove dead peers from the list.
  */
bool ChunkDownload::isReadyToDownload()
{
   if (this->peers.isEmpty() || this->downloading || (!this->chunk.isNull() && this->chunk->isComplete()))
      return false;

   this->currentDownloadingPeer = this->getTheFastestFreePeer();

   return this->currentDownloadingPeer != 0;
}

bool ChunkDownload::isDownloading() const
{
   return this->downloading;
}

bool ChunkDownload::isComplete() const
{
   return !this->chunk.isNull() && this->chunk->isComplete();
}

bool ChunkDownload::hasAtLeastAPeer() const
{
   return !this->peers.isEmpty();
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
   QMutexLocker lock(&this->mutex);

   QList<Common::Hash> peerIDs;
   for (QMutableListIterator<PM::IPeer*> i(this->peers); i.hasNext();)
   {
      PM::IPeer* peer = i.next();
      if (peer->isAlive())
         peerIDs << peer->getID();
      else
         i.remove();
   }
   return peerIDs;
}

/**
  * Tell the chunkDownload to download the chunk from one of its peer.
  * @return true if the downloading has been started.
  */
bool ChunkDownload::startDownloading()
{
   if (this->chunk.isNull())
   {
      L_ERRO(QString("Unable to download without the chunk. Hash : %1").arg(this->chunkHash.toStr()));
      return false;
   }

   L_DEBU(QString("Starting downloading a chunk : %1 from %2").arg(this->chunk->toStr()).arg(this->currentDownloadingPeer->getID().toStr()));

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
   return true;
}

void ChunkDownload::run()
{
   try
   {
      QSharedPointer<FM::IDataWriter> writer = this->chunk->getDataWriter();

      const int BUFFER_SIZE = SETTINGS.get<quint32>("buffer_size");
      char buffer[BUFFER_SIZE];

      const int initialKnownBytes = this->chunk->getKnownBytes();
      int bytesRead = 0;
      qint64 bytesReadTotal = 0;

      qint64 deltaRead = 0;

      this->transferRateCalculator.reset();

      QElapsedTimer timer;
      timer.start();

      forever
      {
         this->mutex.lock();
         if (!this->downloading)
         {
            L_DEBU(QString("Downloading aborted, chunk : %1%2").arg(this->chunk->toStr()).arg(this->chunk->isComplete() ? "" : " Not complete!"));
            this->mutex.unlock();
            break;
         }
         this->mutex.unlock();

         bytesRead = this->socket->getQSocket()->read(buffer, BUFFER_SIZE);

         if (bytesRead == 0)
         {
            if (!this->socket->getQSocket()->waitForReadyRead(SOCKET_TIMEOUT))
            {
               L_WARN(QString("Connection dropped, error = %1, bytesAvailable = %2").arg(socket->getQSocket()->errorString()).arg(socket->getQSocket()->bytesAvailable()));
               this->networkTransferError = true;
               break;
            }
            continue;
         }
         else if (bytesRead == -1)
         {
            L_ERRO(QString("Socket : cannot receive data : %1").arg(this->chunk->toStr()));
            this->networkTransferError = true;
            break;
         }

         bytesReadTotal += bytesRead;
         deltaRead += bytesRead;

         if (timer.elapsed() > 1000)
         {
            this->currentDownloadingPeer->setSpeed(deltaRead / timer.elapsed() * 1000);
            timer.start();
            deltaRead = 0;

            // If a another peer exists and our speed is lesser than 'lan_speed' / 'time_recheck_chunk_factor' and the other peer speed is greater than our
            // then we will try to switch to this peer.
            PM::IPeer* peer = this->getTheFastestFreePeer();
            if (
               peer && peer != this->currentDownloadingPeer &&
               this->currentDownloadingPeer->getSpeed() < SETTINGS.get<quint32>("lan_speed") / SETTINGS.get<double>("time_recheck_chunk_factor") &&
               peer->getSpeed() > SETTINGS.get<double>("switch_to_another_peer_factor") * this->currentDownloadingPeer->getSpeed()
            )
            {
               L_DEBU("Switch to a better peer..");
               break;
            }
         }

         writer->write(buffer, bytesRead);

         this->transferRateCalculator.addData(bytesRead);

         if (initialKnownBytes + bytesReadTotal >= this->chunkSize)
            break;
      }
   }
   catch(FM::UnableToOpenFileInWriteModeException)
   {
      L_ERRO("UnableToOpenFileInWriteModeException");
   }
   catch(FM::IOErrorException&)
   {
      L_ERRO("IOErrorException");
   }
   catch (FM::ChunkDeletedException&)
   {
      L_ERRO("ChunkDeletedException");
   }
   catch (FM::TryToWriteBeyondTheEndOfChunkException&)
   {
      L_ERRO("TryToWriteBeyondTheEndOfChunkException");
   }

   this->transferRateCalculator.reset();
   this->socket->getQSocket()->moveToThread(this->mainThread);
}

void ChunkDownload::result(const Protos::Core::GetChunkResult& result)
{
   if (result.status() != Protos::Core::GetChunkResult_Status_OK)
   {
      L_WARN(QString("Status error from GetChunkResult : %1. Download aborted.").arg(result.status()));
      this->downloadingEnded();
   }
   else
   {
      if (!result.has_chunk_size())
      {
         L_ERRO(QString("Message 'GetChunkResult' doesn't contain the size of the chunk : %1. Download aborted.").arg(this->chunk->getHash().toStr()));
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
   this->socket->stopListening();
   this->socket->getQSocket()->moveToThread(this);

   this->start();
}

void ChunkDownload::getChunkTimeout()
{
   L_WARN("Timeout from GetChunkResult, Download aborted.");
   this->downloadingEnded();
}

void ChunkDownload::downloadingEnded()
{
   L_DEBU(QString("Downloading ended, chunk : %1%2").arg(this->chunk->toStr()).arg(this->chunk->isComplete() ? "" : " Not complete!"));

   if (!this->socket.isNull())
      this->socket.clear();

   if (this->networkTransferError)
      this->getChunkResult->setError();
   this->networkTransferError = false;
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
   QMutexLocker lock(&this->mutex);

   PM::IPeer* current = 0;
   for (QMutableListIterator<PM::IPeer*> i(this->peers); i.hasNext();)
   {
      PM::IPeer* peer = i.next();
      if (!peer->isAlive())
         i.remove();
      else if (this->occupiedPeersDownloadingChunk.isPeerFree(peer) && (!current || peer->getSpeed() > current->getSpeed()))
         current = peer;
   }
   return current;
}
