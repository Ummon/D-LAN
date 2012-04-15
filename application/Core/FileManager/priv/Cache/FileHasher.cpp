#include <priv/Cache/FileHasher.h>
using namespace FM;

#include <QMutexLocker>
#include <QString>
#include <QFile>
#include <QElapsedTimer>

#include <Common/Global.h>
#include <Common/Settings.h>
#include <Common/Hash.h>
#include <Common/FileLocker.h>

#include <Exceptions.h>
#include <priv/Cache/Cache.h>
#include <priv/Cache/File.h>
#include <priv/Log.h>

/**
  * @class FileHasher
  *
  * The class can compute the hashes of a given file (FM::File*).
  * A 'Chunk' object is added to the file for each hash computed.
  *
  * TODO: this class is tightly coupled to 'File' (friend) and 'FileUpdater', try to avoid this.
  */

FileHasher::FileHasher() :
   currentFileCache(0),
   hashing(false),
   toStopHashing(false),
   hashingMutex(QMutex::Recursive)
{
}

/**
  * It will open the file, read it and calculate all theirs chunk hashes.
  * Only the chunk without hashes will be computed.
  * This method can be called from an another thread than the main one. For example,
  * from 'FileUpdated' thread.
  * @param n number of hashes to compute, 0 if we want to compute all the hashes.
  * @exception IOErrorException Thrown when the file cannot be opened or read. Some chunk may be computed before this exception is thrown.
  */
bool FileHasher::start(File* fileCache, int n, int* amountHashed)
{
   QMutexLocker locker(&this->hashingMutex);

   static const int CHUNK_SIZE = SETTINGS.get<quint32>("chunk_size");

   this->currentFileCache = fileCache;

   connect(this->currentFileCache->getCache(), SIGNAL(entryRemoved(Entry*)), this, SLOT(entryRemoved(Entry*)), Qt::UniqueConnection);

   if (this->toStopHashing)
   {
      this->toStopHashing = false;
      this->currentFileCache = 0;
      return false;
   }

   this->hashing = true;

   const QString& filePath = this->currentFileCache->getFullPath();

   L_DEBU(QString("Computing the hash for %1").arg(filePath));

   Common::Hasher hasher;

   QFile file(filePath);
   if (!file.open(QIODevice::ReadOnly | QIODevice::Unbuffered)) // Same performance with or without "QIODevice::Unbuffered".
   {
      this->toStopHashing = false;
      this->hashing = false;
      this->currentFileCache = 0;
      L_WARN(QString("Unable to open this file : %1").arg(filePath));
      throw IOErrorException();
   }

   const QList< QSharedPointer<Chunk> >& chunks = this->currentFileCache->getChunks();

   // Skip the already known full hashes.
   qint64 bytesSkipped = 0;
   int chunkNum = 0;
   while (
      chunkNum < chunks.size() &&
      chunks[chunkNum]->hasHash() &&
      chunks[chunkNum]->getKnownBytes() == CHUNK_SIZE) // Maybe the file has grown and the last chunk must be recomputed.
   {
      bytesSkipped += CHUNK_SIZE;
      chunkNum++;
      file.seek(file.pos() + CHUNK_SIZE);
   }

#if DEBUG
   QElapsedTimer timer;
   timer.start();
#endif

   static const int BUFFER_SIZE = SETTINGS.get<quint32>("buffer_size_reading");
   char buffer[BUFFER_SIZE];
   bool endOfFile = false;
   qint64 bytesReadTotal = 0;
   while (!endOfFile)
   {
      // See 'stopHashing()'.

      int bytesReadChunk = 0;
      while (bytesReadChunk < CHUNK_SIZE)
      {
         locker.unlock();
         locker.relock();

         if (this->toStopHashing)
         {
            this->hashingStopped.wakeOne();
            this->toStopHashing = false;
            this->hashing = false;
            this->currentFileCache = 0;
            return false;
         }

         int bytesRead = 0;
         {
            Common::FileLocker fileLocker(file, BUFFER_SIZE, Common::FileLocker::READ);
            if (!fileLocker.isLocked())
            {
               this->toStopHashing = false;
               this->hashing = false;
               this->currentFileCache = 0;
               L_WARN(QString("Unable to acquire the lock for this file : %1").arg(filePath));
               throw IOErrorException();
            }

            bytesRead = file.read(buffer, BUFFER_SIZE);
            switch (bytesRead)
            {
            case -1:
               this->toStopHashing = false;
               this->hashing = false;
               this->currentFileCache = 0;
               L_ERRO(QString("Error during reading the file %1").arg(filePath));
               throw IOErrorException();
            case 0:
               endOfFile = true;
               this->currentFileCache->setSize(bytesReadChunk + bytesReadTotal + bytesSkipped);
               goto endReading;
            }
         }

         hasher.addData(buffer, bytesRead);

         bytesReadChunk += bytesRead;
      }
      endReading:

      bytesReadTotal += bytesReadChunk;

      if (bytesReadChunk > 0)
      {
         if (amountHashed)
            *amountHashed += bytesReadChunk;

         const Common::Hash& hash = hasher.getResult();

         if (chunks.size() <= chunkNum) // The size of the file has increased during the read..
         {
            QSharedPointer<Chunk> newChunk(new Chunk(this->currentFileCache, chunkNum, bytesReadChunk, hash));
            this->currentFileCache->addChunk(newChunk);
            this->currentFileCache->getCache()->onChunkHashKnown(newChunk);
         }
         else
         {
            if (chunks[chunkNum]->getHash() != hash)
            {
               if (chunks[chunkNum]->hasHash())
                  this->currentFileCache->getCache()->onChunkRemoved(chunks[chunkNum]); // To remove the chunk from the chunk index (TODO: find a more elegant way).

               chunks[chunkNum]->setHash(hash);
               chunks[chunkNum]->setKnownBytes(bytesReadChunk);

               this->currentFileCache->getCache()->onChunkHashKnown(chunks[chunkNum]);
            }
         }

         if (--n == 0)
            break;
      }

      hasher.reset();
      chunkNum += 1;
   }

#ifdef DEBUG
   const int delta = timer.elapsed();
   if (delta < 50)
      L_DEBU("Hashing speed : ?? MB/s (delta too small)");
   else
   {
      const int speed = 1000LL * bytesReadTotal / delta;
      L_DEBU(QString("Hashing speed : %1/s").arg(Common::Global::formatByteSize(speed)));
   }
#endif

   this->toStopHashing = false;
   this->hashing = false;

   // TODO: seriously rethink this part, a file being written shouldn't be shared or hashed...
   if (bytesReadTotal + bytesSkipped != this->currentFileCache->getSize())
   {
      if (n != 0)
      {
         L_DEBU(QString("The file content has changed during the hashes computing process. File = %1, bytes read = %2, previous size = %3").arg(filePath).arg(bytesReadTotal).arg(this->currentFileCache->getSize()));
         this->currentFileCache->setSize(bytesReadTotal + bytesSkipped);
         this->currentFileCache->updateDateLastModified(QFileInfo(filePath).lastModified());

         if (bytesReadTotal + bytesSkipped < this->currentFileCache->getSize()) // In this case, maybe some chunk must be deleted.
            for (int i = this->currentFileCache->getNbChunks(); i < chunks.size(); i++)
            {
               QSharedPointer<Chunk> c = this->currentFileCache->removeLastChunk();
               this->currentFileCache->getCache()->onChunkRemoved(c);
            }
      }

      this->currentFileCache = 0;
      return false;
   }

   this->currentFileCache->updateDateLastModified(QFileInfo(filePath).lastModified()); // A file may have been changed from its creation in the cache.
   this->currentFileCache = 0;
   return true;
}

void FileHasher::stop()
{
   QMutexLocker locker(&this->hashingMutex);
   this->toStopHashing = true;
   if (this->hashing)
   {
      L_DEBU(QString("FileHasher::stop(): %1 ..").arg(this->currentFileCache ? this->currentFileCache->getFullPath() : "?"));
      this->hashingStopped.wait(&this->hashingMutex);
      L_DEBU("File hashing stopped");
   }
}

void FileHasher::entryRemoved(Entry* entry)
{
   QMutexLocker locker(&this->hashingMutex);
   if (this->currentFileCache == entry)
      this->stop();
}
