#include <priv/Cache/File.h>
using namespace FM;

#include <QString>
#include <QFile>
#include <QCryptographicHash>

#include <Exceptions.h>
#include <priv/Exceptions.h>
#include <priv/Log.h>
#include <priv/Constants.h>
#include <priv/Cache/Cache.h>
#include <priv/Cache/Directory.h>
#include <priv/Cache/SharedDirectory.h>
#include <priv/Cache/Chunk.h>

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
   LOG_DEBUG(QString("New file : %1").arg(this->getFullPath()));

   if (!hashes.isEmpty())
      if (this->getNbChunks() != hashes.size())
         LOG_ERR(QString("File::File(..) : The number of hashes (%1) doesn't correspond to the calculate number of chunk (%2)").arg(hashes.size()).arg(this->getNbChunks()));

   bool complete = true;
   if (this->name.size() > UNFINISHED_SUFFIX_TERM.size() && this->name.endsWith(UNFINISHED_SUFFIX_TERM))
   {
      complete = false;
      //this->name = this->name.left(this->name.length() - UNFINISHED_SUFFIX_TERM.length());
   }

   // Test if the file already exists.
   /*
   foreach (File* f, this->dir->getFiles())
      if (f->getName() == this->name)
         throw FileAlreadyExistsException();*/

   for (int i = 0; i < this->getNbChunks(); i++)
   {
      int chunkKnownBytes = !complete ? 0 : i == this->getNbChunks() - 1 ? this->size % CHUNK_SIZE : CHUNK_SIZE;

      if (i < hashes.size())
         this->chunks.append(QSharedPointer<Chunk>(new Chunk(this->cache, this, i, chunkKnownBytes, hashes[i])));
      else
         // If there is too few hashes then null hashes are added.
         this->chunks.append(QSharedPointer<Chunk>(new Chunk(this->cache, this, i, chunkKnownBytes)));
   }

   if (createPhysically)
   {
      if (static_cast<SharedDirectory*>(this->getRoot())->getRights() == SharedDirectory::READ_ONLY)
         LOG_ERR(QString("File::File(..) : Cannot create a file (%1) in a read only shared directory (%2)").arg(this->getPath()).arg(static_cast<SharedDirectory*>(this->getRoot())->getFullPath()));
      else
      {
         QFile file(this->getFullPath());
         if (file.exists())
         {
            LOG_ERR(QString("File::File(..) : Ask to physically create a file which already exists : %1").arg(this->getFullPath()));
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
   QMutexLocker writeLocker(&this->writeLock);
   QMutexLocker readLocker(&this->readLock);

   this->dir->fileDeleted(this);

   foreach (QSharedPointer<Chunk> c, this->chunks)
      c->fileDeleted();

   LOG_DEBUG(QString("File deleted : %1").arg(this->getFullPath()));
}

bool File::restoreFromFileCache(const Protos::FileCache::Hashes_File& file)
{
   LOG_DEBUG(QString("Restoring file '%1' from file cache..").arg(this->getFullPath()));

   if (
      file.filename().data() == this->getName() &&
      (qint64)file.size() == this->getSize() &&
      file.date_last_modified() == this->getDateLastModified().toTime_t() &&
      this->chunks.size() == file.chunk_size()
   )
   {
      LOG_DEBUG(QString("Restoring file '%1' from the file cache").arg(this->getFullPath()));

      for (int i = 0; i < file.chunk_size(); i++)
         this->chunks[i]->restoreFromFileCache(file.chunk(i));

      return true;
   }
   return false;
}

void File::populateHashesFile(Protos::FileCache::Hashes_File& fileToFill) const
{
   return;
   //fileToFill.set_filename(this->name.toStdString());
   fileToFill.set_size(this->size);
   fileToFill.set_date_last_modified(this->getDateLastModified().toTime_t());
   for (QListIterator< QSharedPointer<Chunk> > i(this->chunks); i.hasNext();)
   {
      Protos::FileCache::Hashes_Chunk* chunk = fileToFill.add_chunk();
      i.next();
      //i.next()->populateHashesChunk(*chunk);
   }
}

void File::populateFileEntry(Protos::Common::FileEntry* entry) const
{
   this->populateEntry(entry->mutable_file());
}

QString File::getPath() const
{
   QString path = this->dir->getPath();
   if (!path.isEmpty())
      path.append('/');
   return path.append(this->dir->getName());
}

QString File::getFullPath() const
{
   return this->dir->getFullPath().append("/").append(this->name);
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

qint64 File::read(QByteArray& buffer, qint64 offset)
{
   QMutexLocker locker(&this->readLock);

   if (offset >= this->size)
      return 0;

   this->fileInReadMode->seek(offset);
   qint64 bytesRead = this->fileInReadMode->read(buffer.data(), buffer.size());

   return bytesRead;
}

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

bool File::computeHashes()
{
   this->hashingMutex.lock();

   if (this->toStopHashing)
   {
      this->toStopHashing = false;
      this->hashingMutex.unlock();
      return false;
   }

   this->hashing = true;

   LOG_DEBUG("Computing the hash for " + this->getFullPath());

   QList<QByteArray> result;

   QCryptographicHash crypto(QCryptographicHash::Sha1);

   QFile file(this->getFullPath());
   if (!file.open(QIODevice::ReadOnly))
   {
      this->toStopHashing = false;
      this->hashing = false;
      this->hashingMutex.unlock();
      throw FileNotFoundException(this->getFullPath());
   }

#if DEBUG
   QTime time;
   time.start();
#endif

   char buffer[BUFFER_SIZE];
   bool endOfFile = false;
   int chunkNum = 0;
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
         this->hashingMutex.unlock();
         return false;
      }

      qint64 bytesReadTotal = 0;
      while (bytesReadTotal < CHUNK_SIZE)
      {
         qint64 bytesRead = file.read(buffer, BUFFER_SIZE);
         if (bytesRead == 0)
         {
            endOfFile = true;
            break;
         }
         crypto.addData(buffer, bytesRead);
         bytesReadTotal += bytesRead;
      }

      this->chunks[chunkNum]->setHash(crypto.result());
      crypto.reset();
      chunkNum += 1;
   }

#ifdef DEBUG
   const int delta = time.elapsed();
   if (delta == 0)
      LOG_DEBUG("Hashing speed : ?? MB/s (delta too small)");
   else
   {
      const double speed = static_cast<double>(this->size) / 1024 / 1024 / (static_cast<double>(delta) / 1000);
      LOG_DEBUG(QString("Hashing speed : %1 MB/s").arg(speed < 0.1 ? "< 0.1" : QString::number(speed)));
   }
#endif

   this->toStopHashing = false;
   this->hashing = false;
   this->hashingMutex.unlock();
   return true;
}

void File::stopHashing()
{
   QMutexLocker locker(&this->hashingMutex);
   this->toStopHashing = true;
   if (this->hashing)
   {
      LOG_DEBUG(QString("File::stopHashing() for %1 ..").arg(this->getFullPath()));
      this->hashingStopped.wait(&this->hashingMutex);
      LOG_DEBUG("Hashing stopped");
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
   //LOG_DEBUG(QString("this->size = %1, CHUNK_SIZE = %2, res = %3, this->nbChunks = %4").arg(this->size).arg(CHUNK_SIZE).arg(this->size / CHUNK_SIZE + (this->size % CHUNK_SIZE == 0 ? 0 : 1)).arg(this->nbChunks));

   qint64 currentSize = 0;
   for (int i = 0; i < this->chunks.size(); i++)
      currentSize += this->chunks[i]->getKnownBytes();

   return this->size == currentSize;
}

void File::physicallyRemoveUnfinished()
{
   if (!this->isComplete())
   {
      if (!QFile::remove(this->getFullPath()))
         LOG_WARN(QString("File::physicallyRemoveUnfinished() : Cannot remove the file '%1'").arg(this->getFullPath()));
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


