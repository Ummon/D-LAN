#include <priv/Cache/File.h>
using namespace FM;

#include <QString>
#include <QFile>
#include <QCryptographicHash>

#include <Common/Global.h>

#include <Exceptions.h>
#include <priv/Exceptions.h>
#include <priv/Log.h>
#include <priv/Constants.h>
#include <priv/Cache/Cache.h>
#include <priv/Cache/Directory.h>
#include <priv/Cache/SharedDirectory.h>
#include <priv/Cache/Chunk.h>


/**
  * @class File
  * A file can be finished or unfinished.
  * If it is an unfinished one, the name ends with ".unfinished" (see UNFINISHED_SUFFIX_TERM).
  * When a file is just finished the suffix ".unfinished" is removed and the file is renamed.
  */

/**
  * Create a new file into a given directory.
  * The file may or may not have a correponding local file.
  * If 'createPhysically' is true then the file is created as unfinished with no byte known.
  * @param hashes Optional hashes, if given it must contain ALL hashes.
  * @exception FileAlreadyExistsException : TODO!!!
  * @exception FilePhysicallyAlreadyExistsException : TODO!!!
  */
File::File(
   Directory* dir,
   const QString& name,
   qint64 size,
   const QDateTime& dateLastModified,
   const Common::Hashes& hashes,
   bool createPhysically
)
   : Entry(dir->getCache(), name + (createPhysically ? UNFINISHED_SUFFIX_TERM : ""), size),
     dir(dir),
     dateLastModified(dateLastModified),
     numDataWriter(0),
     numDataReader(0),
     fileInWriteMode(0),
     fileInReadMode(0),
     hashing(false),
     toStopHashing(false)
{
   //QMutexLocker locker(&this->getCache()->getMutex());
   L_DEBU(QString("New file : %1 (%2)").arg(this->getFullPath()).arg(Common::Global::formatByteSize(this->size)));

   if (!hashes.isEmpty())
      if (this->getNbChunks() != hashes.size())
         L_ERRO(QString("File::File(..) : The number of hashes (%1) doesn't correspond to the calculate number of chunk (%2)").arg(hashes.size()).arg(this->getNbChunks()));

   bool complete = !(this->name.size() > UNFINISHED_SUFFIX_TERM.size() && this->name.endsWith(UNFINISHED_SUFFIX_TERM));

   // Test if the file already exists.
   /*
   foreach (File* f, this->dir->getFiles())
      if (f->getName() == this->name)
         throw FileAlreadyExistsException();*/

   for (int i = 0; i < this->getNbChunks(); i++)
   {
      int chunkKnownBytes = !complete ? 0 : i == this->getNbChunks() - 1 && this->size % CHUNK_SIZE != 0 ? this->size % CHUNK_SIZE : CHUNK_SIZE;

      if (i < hashes.size())
      {
         QSharedPointer<Chunk> chunk(new Chunk(this, i, chunkKnownBytes, hashes[i]));
         this->chunks.append(chunk);
         this->cache->onChunkHashKnown(chunk);
      }
      else
         // If there is too few hashes then null hashes are added.
         this->chunks.append(QSharedPointer<Chunk>(new Chunk(this, i, chunkKnownBytes)));
   }

   if (createPhysically)
   {
      if (static_cast<SharedDirectory*>(this->getRoot())->getRights() == SharedDirectory::READ_ONLY)
         L_ERRO(QString("File::File(..) : Cannot create a file (%1) in a read only shared directory (%2)").arg(this->getPath()).arg(static_cast<SharedDirectory*>(this->getRoot())->getFullPath()));
      else
      {
         QFile file(this->getFullPath());
         if (file.exists())
         {
            L_ERRO(QString("File::File(..) : Ask to physically create a file which already exists : %1").arg(this->getFullPath()));
            throw FilePhysicallyAlreadyExistsException();
         }
         else
         {
            file.open(QIODevice::WriteOnly);
            file.resize(this->size);
            file.close();
            this->dateLastModified = QFileInfo(file).lastModified();
         }
      }
   }

   this->dir->addFile(this);
}

File::~File()
{
   QMutexLocker lockerWrite(&this->writeLock);
   QMutexLocker lockerRead(&this->readLock);
   // QMutexLocker(&this->cache->getMutex()); // TODO :

   this->dir->fileDeleted(this);

   foreach (QSharedPointer<Chunk> c, this->chunks)
   {
      c->fileDeleted();
      this->cache->onChunkRemoved(c);
   }

   L_DEBU(QString("File deleted : %1").arg(this->getFullPath()));
}

bool File::restoreFromFileCache(const Protos::FileCache::Hashes_File& file)
{
   L_DEBU(QString("Restoring file '%1' from file cache..").arg(this->getFullPath()));

   if (
      file.filename().data() == this->getName() &&
      (qint64)file.size() == this->getSize() &&
      file.date_last_modified() == this->getDateLastModified().toTime_t() &&
      this->chunks.size() == file.chunk_size()
   )
   {
      L_DEBU(QString("Restoring file '%1' from the file cache").arg(this->getFullPath()));

      for (int i = 0; i < file.chunk_size(); i++)
      {
         this->chunks[i]->restoreFromFileCache(file.chunk(i));
         if (file.chunk(i).has_hash())
            this->cache->onChunkHashKnown(this->chunks[i]);
      }

      return true;
   }
   return false;
}

void File::populateHashesFile(Protos::FileCache::Hashes_File& fileToFill) const
{
   fileToFill.set_filename(this->name.toStdString());
   fileToFill.set_size(this->size);
   fileToFill.set_date_last_modified(this->getDateLastModified().currentMSecsSinceEpoch());

   for (QListIterator< QSharedPointer<Chunk> > i(this->chunks); i.hasNext();)
   {
      Protos::FileCache::Hashes_Chunk* chunk = fileToFill.add_chunk();
      i.next()->populateHashesChunk(*chunk);
   }
}

/**
  * Will add the hashes to the entry.
  */
void File::populateEntry(Protos::Common::Entry* entry) const
{
   Entry::populateEntry(entry);

   entry->set_type(Protos::Common::Entry_Type_FILE);

   for (QListIterator< QSharedPointer<Chunk> > i(this->chunks); i.hasNext();)
   {
      Common::Hash hash = i.next()->getHash();
      if (hash.isNull())
         break;

      entry->add_chunk()->set_hash(hash.getData(), Common::Hash::HASH_SIZE);
   }
}

QString File::getPath() const
{
   QString path;
   if (!dynamic_cast<SharedDirectory*>(this->dir))
      path.append(this->dir->getPath()).append(this->dir->getName());

   return path.append('/');
}

QString File::getFullPath() const
{
   return this->dir->getFullPath().append('/').append(this->name);
}

Directory* File::getRoot() const
{
   return this->dir->getRoot();
}

QDateTime File::getDateLastModified() const
{
   return this->dateLastModified;
}

void File::newDataWriterCreated()
{
   QMutexLocker locker(&this->writeLock);

   if (this->numDataWriter == 0)
   {
      this->fileInWriteMode = new QFile(this->getPath());
      this->fileInWriteMode->open(QIODevice::WriteOnly);
   }
   this->numDataWriter += 1;
}

void File::newDataReaderCreated()
{
   QMutexLocker locker(&this->readLock);

   if (this->numDataReader == 0)
   {
      this->fileInReadMode = new QFile(this->getPath());
      this->fileInReadMode->open(QIODevice::ReadOnly);
   }
   this->numDataReader += 1;
}

void File::dataWriterDeleted()
{
   QMutexLocker locker(&this->writeLock);

   this->numDataWriter -= 1;
   if  (this->numDataWriter == 0)
   {
      delete this->fileInWriteMode;
      this->fileInWriteMode = 0;
   }
}

void File::dataReaderDeleted()
{
   QMutexLocker locker(&this->readLock);

   this->numDataReader -= 1;
   if  (this->numDataReader == 0)
   {
      delete this->fileInReadMode;
      this->fileInReadMode = 0;
   }
}

/**
  * Write some bytes to the file at the given offset.
  * If the buffer exceed the file size then only the begining of the buffer is
  * used, the file is not resizing.
  * @param buffer The buffer.
  * @param offset An offset.
  * @return true if end of file reached.
  */
bool File::write(const QByteArray& buffer, qint64 offset)
{
   QMutexLocker locker(&this->writeLock);

   if (offset >= this->size)
      return true;

   this->fileInWriteMode->seek(offset);
   int maxSize = (this->size - offset);
   qint64 n = this->fileInWriteMode->write(buffer.data(), buffer.size() > maxSize ? maxSize : buffer.size());

   return offset + n >= (qint64)this->size || n == -1;
}

/**
  * Fill the buffer with the read bytes from the given offset.
  * If the end of file is reached the buffer will be partialy filled.
  * @param buffer The buffer.
  * @param offset An offset.
  * @return the number of bytes read.
  */
qint64 File::read(QByteArray& buffer, qint64 offset)
{
   QMutexLocker locker(&this->readLock);

   if (offset >= this->size)
      return 0;

   this->fileInReadMode->seek(offset);
   qint64 bytesRead = this->fileInReadMode->read(buffer.data(), buffer.size());

   return bytesRead;
}

/**
  * It will open the file, read it and calculate all theirs chunk hashes.
  * Only the chunk without hashes will be computed.
  * This method can be called from an another thread than the main one. For example,
  * from 'FileUpdated' thread.
  * @param n number of hashes to compute, 0 if we want to compute all the hashes.
  * @return Return true if all the hashes as been computed.
  * @exception FileNotFoundException
  */
bool File::computeHashes(int n)
{
   QMutexLocker lock(&this->hashingMutex);

   if (this->toStopHashing)
   {
      this->toStopHashing = false;
      return false;
   }

   this->hashing = true;

   L_DEBU("Computing the hash for " + this->getFullPath());

   QList<QByteArray> result;

   QCryptographicHash crypto(QCryptographicHash::Sha1);

   QFile file(this->getFullPath());
   if (!file.open(QIODevice::ReadOnly))
   {
      this->toStopHashing = false;
      this->hashing = false;
      L_WARN(QString("Unable to open this file : %1").arg(this->getFullPath()));
      return false;
   }

   // Skip the already known full hashes.
   int chunkNum = 0;
   while (
      chunkNum < this->chunks.size() &&
      this->chunks[chunkNum]->hasHash() &&
      this->chunks[chunkNum]->getKnownBytes() == CHUNK_SIZE) // Maybe the file has grown and the last chunk must be recomputed.
   {
      chunkNum++;
      file.seek(file.pos() + CHUNK_SIZE);
   }

#if DEBUG
   QTime time;
   time.start();
#endif

   char buffer[BUFFER_SIZE];
   bool endOfFile = false;
   qint64 bytesReadTotal = 0;
   while (!endOfFile)
   {
      // See 'stopHashing()'.
      this->hashingMutex.unlock();
      this->hashingMutex.lock();
      if (this->toStopHashing)
      {
         this->hashingStopped.wakeOne();
         this->toStopHashing = false;
         this->hashing = false;
         return false;
      }

      int bytesReadChunk = 0;
      while (bytesReadChunk < CHUNK_SIZE)
      {
         int bytesRead = file.read(buffer, BUFFER_SIZE);
         switch (bytesRead)
         {
         case -1:
            L_ERRO(QString("Error during reading the file %1").arg(this->getFullPath()));
         case 0:
            endOfFile = true;
            goto endReading;
         }

         crypto.addData(buffer, bytesRead);
         bytesReadChunk += bytesRead;
      }
      endReading:

      bytesReadTotal += bytesReadChunk;

      if (bytesReadChunk > 0)
      {
         L_WARN(QString("crypto.result() = %1").arg(QString(crypto.result().toHex())));

         if (this->chunks.size() <= chunkNum) // The size of the file has increased during the read..
            this->chunks.append(QSharedPointer<Chunk>(new Chunk(this, chunkNum, bytesReadChunk, crypto.result())));
         else
         {
            this->chunks[chunkNum]->setHash(crypto.result());
            this->chunks[chunkNum]->setKnownBytes(bytesReadChunk);
         }

         this->cache->onChunkHashKnown(this->chunks[chunkNum]);
      }

      crypto.reset();
      chunkNum += 1;
   }

#ifdef DEBUG
   const int delta = time.elapsed();
   if (delta == 0)
      L_DEBU("Hashing speed : ?? MB/s (delta too small)");
   else
   {
      const double speed = static_cast<double>(bytesReadTotal) / 1024 / 1024 / (static_cast<double>(delta) / 1000);
      L_DEBU(QString("Hashing speed : %1 MB/s").arg(speed < 0.1 ? "< 0.1" : QString::number(speed)));
   }
#endif

   this->toStopHashing = false;
   this->hashing = false;

   if (bytesReadTotal != this->size)
   {
      L_DEBU(QString("The file content has changed during the hashes computing process. File = %1, bytes read = %2, previous size = %3").arg(this->getFullPath()).arg(bytesReadTotal).arg(this->size));
      this->size = bytesReadTotal;
      this->dateLastModified = QFileInfo(this->getFullPath()).lastModified();

      if (bytesReadTotal < this->size) // In this case, maybe some chunk must be deleted.
         for (int i = this->getNbChunks(); i < this->chunks.size(); i++)
         {
            QSharedPointer<Chunk> c = this->chunks.takeLast();
            this->cache->onChunkRemoved(c);
         }

      return false;
   }

   return true;
}

void File::stopHashing()
{
   QMutexLocker locker(&this->hashingMutex);

   this->toStopHashing = true;
   if (this->hashing)
   {
      L_DEBU(QString("File::stopHashing() for %1 ..").arg(this->getFullPath()));
      this->hashingStopped.wait(&this->hashingMutex);
      L_DEBU("File hashing stopped");
   }
}

QList< QSharedPointer<Chunk> > File::getChunks() const
{
   return this->chunks;
}

bool File::hasAllHashes()
{
   foreach (QSharedPointer<Chunk> c, this->chunks)
      if (!c->hasHash())
         return false;
   return true;
}

bool File::hasOneOrMoreHashes()
{
   foreach (QSharedPointer<Chunk> c, this->chunks)
     if (c->hasHash())
         return true;
   return false;
}

bool File::isComplete()
{
   //L_DEBU(QString("this->size = %1, CHUNK_SIZE = %2, res = %3, this->getNbChunks() = %4").arg(this->size).arg(CHUNK_SIZE).arg(this->size / CHUNK_SIZE + (this->size % CHUNK_SIZE == 0 ? 0 : 1)).arg(this->getNbChunks()));

   qint64 currentSize = 0;
   for (int i = 0; i < this->chunks.size(); i++)
      currentSize += this->chunks[i]->getKnownBytes();

   return this->size == currentSize;
}

/**
  * Return true if the size and the last modification date correspond to the given file information
  */
bool File::correspondTo(const QFileInfo& fileInfo)
{
   return this->getSize() == fileInfo.size() && this->getDateLastModified() == fileInfo.lastModified();
}

/**
  * Remove the file physically only if it's not complete.
  * The file removed must ended by UNFINISHED_SUFFIX_TERM.
  */
void File::physicallyRemoveUnfinished()
{
   if (!this->isComplete())
   {
      if (!QFile::remove(this->getFullPath()))
         L_WARN(QString("File::physicallyRemoveUnfinished() : Cannot remove the file '%1'").arg(this->getFullPath()));
   }
}

void File::changeDirectory(Directory* dir)
{
   this->dir = dir;
}

int File::getNbChunks()
{
   return this->size / CHUNK_SIZE + (this->size % CHUNK_SIZE == 0 ? 0 : 1);
}

/*QList<IChunk*> File::getChunks() const
{
   // TODO
   return QList<IChunk*>(); //this->chunks;
}

const QList<Chunk*>& File::getChunksRef() const
{
   return this->chunks;
}*/


