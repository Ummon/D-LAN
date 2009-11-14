#include <priv/Cache/File.h>
using namespace FM;

#include <QString>
#include <QFile>
#include <QCryptographicHash>

#include <priv/Log.h>
#include <priv/Exceptions.h>
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
   const Common::Hashes& hashes
)
   : Entry(dir->getCache(), name, size),
     dir(dir),
     dateLastModified(dateLastModified),
     numDataWriter(0),
     numDataReader(0),
     fileInWriteMode(0),
     fileInReadMode(0),
     writeLock(0),
     readLock(0),
     nbChunks(0),
     hashing(false)
{
   //QMutexLocker locker(&this->getCache()->getMutex());
   LOG_DEBUG(QString("New file : %1").arg(this->getFullPath()));

   if (!hashes.isEmpty())
   {
      for (int i = 0; i < hashes.size(); i++)
      {
         QSharedPointer<Chunk> chunk(new Chunk(*this, hashes[i], i, 0));
         this->nbChunks += 1;
         this->chunks.append(chunk);
      }
   }

   this->dir->addFile(this);
}

File::~File()
{
   LOG_DEBUG(QString("File deleted : %1").arg(this->getFullPath()));

   this->dir->fileDeleted(this);
}

File* File::restoreFromFileCache(const Protos::FileCache::Hashes_File& file)
{
   LOG_DEBUG(QString("Restoring file '%1' from file cache..").arg(this->getFullPath()));

   this->chunks.clear();

   if (
      file.filename().data() == this->getName() &&
      (qint64)file.size() == this->getSize() &&
      file.date_last_modified() == this->getDateLastModified().toTime_t()
   )
   {
      LOG_DEBUG(QString("Restoring file '%1' from the file cache").arg(this->getFullPath()));
      // TODO : maybe test whether the chunks size is correct..
      for (int i = 0; i < file.chunk_size(); i++)
      {
         this->chunks << QSharedPointer<Chunk>(new Chunk(*this, i, file.chunk(i)));
         this->nbChunks += 1;
      }
   }

   return this;
}

void File::populateHashesFile(Protos::FileCache::Hashes_File& fileToFill) const
{
   fileToFill.set_filename(this->name.toStdString());
   fileToFill.set_size(this->size);
   fileToFill.set_date_last_modified(this->getDateLastModified().toTime_t());
   for (QListIterator< QSharedPointer<Chunk> > i(this->chunks); i.hasNext();)
   {
      Protos::FileCache::Hashes_Chunk* chunk = fileToFill.add_chunk();
      i.next()->populateHashesChunk(*chunk);
   }
}

void File::populateFileEntry(Protos::Common::FileEntry* entry) const
{
   entry->mutable_file()->set_path(this->getPath().toStdString());
   entry->mutable_file()->set_name(this->getName().toStdString());
}

void File::eliminate()
{
   LOG_DEBUG(QString("File::eliminate() : %1").arg(this->getFullPath()));

   this->setDeleted();
   this->stopHashing();

   if (this->nbChunks == 0)
      delete this;

   foreach (QSharedPointer<Chunk> c, this->chunks)
      c->setDeleted();

   this->chunks.clear();
}

void File::chunkDeleted(Chunk* chunk)
{
   LOG_DEBUG(QString("File::chunkDeleted() : nbChunks = %1").arg(this->nbChunks));
   this->nbChunks -= 1;
   if (this->nbChunks == 0 && this->isDeleted())
      delete this;
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
   if (this->numDataWriter == 0)
   {
      this->fileInWriteMode = new QFile(this->getPath());
      this->fileInWriteMode->open(QIODevice::WriteOnly);
      this->writeLock = new QMutex();
   }
   this->numDataWriter += 1;
}

void File::newDataReaderCreated()
{
   if (this->numDataReader == 0)
   {
      this->fileInReadMode = new QFile(this->getPath());
      this->fileInReadMode->open(QIODevice::ReadOnly);
      this->readLock = new QMutex();
   }
   this->numDataReader += 1;
}

void File::dataWriterDeleted()
{
   this->numDataWriter -= 1;
   if  (this->numDataWriter == 0)
   {
      delete this->fileInWriteMode;
      this->fileInWriteMode = 0;
      delete this->writeLock;
      this->writeLock = 0;
   }
}

void File::dataReaderDeleted()
{
   this->numDataReader -= 1;
   if  (this->numDataReader == 0)
   {
      delete this->fileInReadMode;
      this->fileInReadMode = 0;
      delete this->readLock;
      this->readLock = 0;
   }
}

qint64 File::read(QByteArray& buffer, qint64 offset)
{
   if (offset >= this->size)
      return 0;

   QMutexLocker locker(this->readLock);

   this->fileInReadMode->seek(offset);
   qint64 bytesRead = this->fileInReadMode->read(buffer.data(), buffer.size());

   return bytesRead;
}

bool File::write(const QByteArray& buffer, qint64 offset)
{
   if (offset >= this->size)
      return true;

   QMutexLocker locker(this->writeLock);

   this->fileInWriteMode->seek(offset);
   int maxSize = (this->size - offset);
   qint64 n = this->fileInWriteMode->write(buffer.data(), buffer.size() > maxSize ? maxSize : buffer.size());

   return offset + n >= (qint64)this->size || n == -1;
}

bool File::computeHashes()
{
   LOG_DEBUG("Computing the hash for " + this->getFullPath());

   if (this->isDeleted())
   {
      LOG_DEBUG("Cannot compute hash for a deleted file : " + dir->getFullPath());
      return false;
   }

   this->hashingMutex.lock();
   this->hashing = true;
   this->hashingMutex.unlock();

   QList<QByteArray> result;

   QCryptographicHash crypto(QCryptographicHash::Sha1);

   QFile file(this->getFullPath());
   if (!file.open(QIODevice::ReadOnly))
      throw FileNotFoundException(this->getFullPath());

#if DEBUG
   QTime time;
   time.start();
#endif

   char buffer[BUFFER_SIZE];
   bool endOfFile = false;
   int chunkNum = 0;
   while (!endOfFile)
   {
      this->hashingMutex.lock();
      if (!this->hashing)
      {
         this->hashingMutex.unlock();
         this->hashingStopped.wakeOne();
         return false;
      }
      this->hashingMutex.unlock();

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
      this->chunks.append(QSharedPointer<Chunk>(new Chunk(*this, crypto.result(), chunkNum)));
      this->nbChunks += 1;
      chunkNum += 1;
      crypto.reset();
   }

   this->hashingMutex.lock();
   this->hashing = false;
   this->hashingStopped.wakeOne();
   this->hashingMutex.unlock();

#ifdef DEBUG
   const int delta = time.elapsed();
   if (delta == 0)
      LOG_DEBUG("Hashing speed : ?? MB/s");
   else
   {
      const double speed = static_cast<double>(this->size) / 1024 / 1024 / (static_cast<double>(delta) / 1000);
      LOG_DEBUG(QString("Hashing speed : %1 MB/s").arg(speed < 0.1 ? "< 0.1" : QString::number(speed)));
   }
#endif
   return true;
}

void File::stopHashing()
{
   QMutexLocker locker(&this->hashingMutex);
   if (this->hashing)
   {
      LOG_DEBUG(QString("File::stopHashing() for %1 ..").arg(this->getFullPath()));
      this->hashing = false;
      this->hashingStopped.wait(&this->hashingMutex);
      LOG_DEBUG("Hashing stopped");
   }
}

bool File::haveAllHashes()
{
   //LOG_DEBUG(QString("this->size = %1, CHUNK_SIZE = %2, res = %3, this->nbChunks = %4").arg(this->size).arg(CHUNK_SIZE).arg(this->size / CHUNK_SIZE + (this->size % CHUNK_SIZE == 0 ? 0 : 1)).arg(this->nbChunks));

   // TODO : Some chunks can be not complete
   return this->nbChunks == this->size / CHUNK_SIZE + (this->size % CHUNK_SIZE == 0 ? 0 : 1);
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


