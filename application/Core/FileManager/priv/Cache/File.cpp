#include <priv/Cache/File.h>
using namespace FM;

#include <QString>
#include <QFile>
#include <QCryptographicHash>

#include <priv/FileManager.h>
#include <priv/Exceptions.h>
#include <priv/Cache/Directory.h>
#include <priv/Cache/SharedDirectory.h>
#include <priv/Cache/Chunk.h>

const QString File::FILE_TEMP_POSTFIX(".temp");

File::File(Directory* dir, const QString& name, qint64 size, const Common::Hashes& hashes)
   : Entry(name, size), dir(dir),
   numDataWriter(0), numDataReader(0),
   fileInWriteMode(0), fileInReadMode(0),
   writeLock(0), readLock(0)
{
   if (!hashes.isEmpty())
   {
      for (int i = 0; i < hashes.size(); i++)
      {
         Chunk* chunk = new  Chunk(*this, hashes[i], i, 0);
         this->chunks.append(chunk);
      }
   }

   this->dir->addFile(this);

   // The root must be a shared directory. If not, someone will be fired !
   static_cast<SharedDirectory*>(this->getRoot())->getCache()->onEntryAdded(this);
}

QString File::getPath()
{
   QString path = this->dir->getPath();
   if (!path.isEmpty())
      path.append('/');
   return path.append(this->dir->getName());
}

QString File::getFullPath()
{
   return this->dir->getFullPath().append("/").append(this->name);
}

Directory* File::getRoot()
{
   return this->dir->getRoot();
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

void File::computeHashes()
{
   LOG_DEBUG("Computing the hash for " + this->getPath());

   QList<QByteArray> result;

   QCryptographicHash crypto(QCryptographicHash::Sha1);

   QFile file(this->getFullPath());
   if (!file.open(QIODevice::ReadOnly))
      throw FileNotFoundException(this->getFullPath());

   char buffer[BUFFER_SIZE];
   bool endOfFile = false;
   int chunkNum = 0;
   while (!endOfFile)
   {
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
      Chunk* chunk = new  Chunk(*this, crypto.result(), chunkNum);
      this->chunks.append(chunk);
      crypto.reset();
      chunkNum += 1;
   }
}

QList<IChunk*> File::getChunks()
{
   // TODO
   return QList<IChunk*>(); //this->chunks;
}

const QList<Chunk*>& File::getChunksRef()
{
   return this->chunks;
}

void File::populateFileEntry(Protos::Common::FileEntry* entry)
{
   entry->mutable_file()->set_path(this->getPath().toStdString());
   entry->mutable_file()->set_name(this->getName().toStdString());
}
