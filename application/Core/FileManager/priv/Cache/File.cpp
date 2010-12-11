#include <priv/Cache/File.h>
using namespace FM;

#include <QString>
#include <QFile>
#include <QElapsedTimer>

#include <Common/Global.h>
#include <Common/Settings.h>
#include <Common/ProtoHelper.h>

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
  * If it is an unfinished one, the name ends with ".unfinished" (see setting "unfinished_suffix_term").
  * When a file is just finished the suffix ".unfinished" is removed and the file is renamed.
  */

/**
  * Create a new file into a given directory.
  * The file may or may not have a correponding local file.
  * If 'createPhysically' is true then the file is created as unfinished with no byte known.
  * @param hashes Optional hashes, if given it must contain ALL hashes.
  */
File::File(
   Directory* dir,
   const QString& name,
   qint64 size,
   const QDateTime& dateLastModified,
   const Common::Hashes& hashes,
   bool createPhysically
)
   : Entry(dir->getCache(), name + (createPhysically ? SETTINGS.get<QString>("unfinished_suffix_term") : ""), size),
     CHUNK_SIZE(SETTINGS.get<quint32>("chunk_size")),
     BUFFER_SIZE(SETTINGS.get<quint32>("buffer_size")),
     dir(dir),
     dateLastModified(dateLastModified),
     nbChunkComplete(0),
     complete(!Cache::isFileUnfinished(Entry::getName())),
     tryToRename(false),
     numDataWriter(0),
     numDataReader(0),
     fileInWriteMode(0),
     fileInReadMode(0),
     mutex(QMutex::Recursive),
     hashing(false),
     toStopHashing(false)
{
   // QMutexLocker locker(&this->getCache()->getMutex());

   L_DEBU(QString("New file : %1 (%2), createPhysically = %3").arg(this->getFullPath()).arg(Common::Global::formatByteSize(this->size)).arg(createPhysically));

   if (createPhysically)
      this->createPhysicalFile();

   this->setHashes(hashes);

   this->dir->addFile(this);
}

File::~File()
{
   // QMutexLocker(&this->cache->getMutex()); // TODO : Is it necessary ?

   this->dir->fileDeleted(this);

   foreach (QSharedPointer<Chunk> c, this->chunks)
      c->fileDeleted();

   this->deleteAllChunks();

   if (!this->isComplete())
   {
      QMutexLocker lockerWrite(&this->writeLock);
      QMutexLocker lockerRead(&this->readLock);

      if (this->fileInReadMode)
         this->fileInReadMode->close();
      if (this->fileInWriteMode)
         this->fileInWriteMode->close();

      if (!QFile::remove(this->getFullPath()))
         L_ERRO(QString("File::~File() : cannot delete an unfinished file : %1").arg(this->getFullPath()));
   }

   L_DEBU(QString("File deleted : %1").arg(this->getFullPath()));
}

/**
  * Set the file as unfinished, this is use when an existing file is re-downloaded.
  * The file is removed from the index and a new physcally file named "<name>.unfinished" is created.
  * The old physical file is not removed and will be replaced only when this one is finished.
  * @exception UnableToCreateNewFileException
  */
void File::setToUnfinished(qint64 size, const Common::Hashes& hashes)
{
   const QString oldPath(this->getFullPath());

   this->complete = false;
   this->stopHashing();
   this->tryToRename = false;
   this->cache->onEntryRemoved(this);
   this->name.append(SETTINGS.get<QString>("unfinished_suffix_term"));
   this->dateLastModified = QDateTime::currentDateTime();
   this->nbChunkComplete = 0;
   this->deleteAllChunks();
   this->setHashes(hashes);

   this->createPhysicalFile();
}

bool File::restoreFromFileCache(const Protos::FileCache::Hashes_File& file)
{
   if (
      Common::ProtoHelper::getStr(file, &Protos::FileCache::Hashes_File::filename) == this->getName() &&
      (qint64)file.size() == this->size &&
      // file.date_last_modified() == this->getDateLastModified().toMSecsSinceEpoch() &&
      this->chunks.size() == file.chunk_size()
   )
   {
      L_DEBU(QString("Restoring file '%1' from the file cache").arg(this->getFullPath()));

      for (int i = 0; i < file.chunk_size(); i++)
      {
         this->chunks[i]->restoreFromFileCache(file.chunk(i));
         if (this->chunks[i]->hasHash() && this->chunks[i]->isComplete())
         {
            this->nbChunkComplete++;
            this->cache->onChunkHashKnown(this->chunks[i]);
         }
      }

      return true;
   }
   return false;
}

void File::populateHashesFile(Protos::FileCache::Hashes_File& fileToFill) const
{
   QMutexLocker locker(&this->mutex);

   Common::ProtoHelper::setStr(fileToFill, &Protos::FileCache::Hashes_File::set_filename, this->name);
   fileToFill.set_size(this->size);
   fileToFill.set_date_last_modified(this->getDateLastModified().toMSecsSinceEpoch());

   for (QListIterator< QSharedPointer<Chunk> > i(this->chunks); i.hasNext();)
   {
      Protos::FileCache::Hashes_Chunk* chunk = fileToFill.add_chunk();
      i.next()->populateHashesChunk(*chunk);
   }
}

/**
  * Will add the hashes to the entry.
  */
void File::populateEntry(Protos::Common::Entry* entry, bool setSharedDir) const
{
   Entry::populateEntry(entry, setSharedDir);

   entry->set_type(Protos::Common::Entry_Type_FILE);

   entry->clear_chunk();
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
      this->fileInWriteMode = new QFile(this->getFullPath());
      if (!this->fileInWriteMode->open(QIODevice::ReadWrite))
         throw UnableToOpenFileInWriteModeException();
   }
   this->numDataWriter += 1;
}

void File::newDataReaderCreated()
{
   QMutexLocker locker(&this->readLock);

   if (this->numDataReader == 0)
   {
      this->fileInReadMode = new QFile(this->getFullPath());
      if (!this->fileInReadMode->open(QIODevice::ReadOnly))
         throw UnableToOpenFileInReadModeException();
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

      if (this->tryToRename)
         this->setAsComplete();
   }
}

/**
  * Write some bytes to the file at the given offset.
  * If the buffer exceed the file size then only the begining of the buffer is
  * used, the file is not resizing.
  * @exception IOErrorException
  * @param buffer The buffer.
  * @param offset An offset.
  */
qint64 File::write(const char* buffer, int nbBytes, qint64 offset)
{
   QMutexLocker locker(&this->writeLock);

   if (offset >= this->size || !this->fileInWriteMode->seek(offset))
      throw IOErrorException();

   qint64 maxSize = this->size - offset;
   qint64 n = this->fileInWriteMode->write(buffer, nbBytes > maxSize ? maxSize : nbBytes);

   if (n == -1)
      throw IOErrorException();

   return n;
}

/**
  * Fill the buffer with the read bytes from the given offset.
  * If the end of file is reached the buffer will be partialy filled.
  * @param buffer The buffer.
  * @param offset An offset.
  * @return the number of bytes read.
  */
qint64 File::read(char* buffer, qint64 offset, int maxBytesToRead)
{
   QMutexLocker locker(&this->readLock);

   if (offset >= this->size)
      return 0;

   this->fileInReadMode->seek(offset);
   qint64 bytesRead = this->fileInReadMode->read(buffer, maxBytesToRead);

   if (bytesRead == -1)
      throw IOErrorException();

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
   QMutexLocker locker(&this->hashingMutex);

   if (this->toStopHashing)
   {
      this->toStopHashing = false;
      return false;
   }

   this->hashing = true;

   L_DEBU("Computing the hash for " + this->getFullPath());

   QList<QByteArray> result;

   Common::Hasher hasher;

   QFile file(this->getFullPath());
   if (!file.open(QIODevice::ReadOnly))
   {
      this->toStopHashing = false;
      this->hashing = false;
      L_WARN(QString("Unable to open this file : %1").arg(this->getFullPath()));
      return false;
   }

   // Skip the already known full hashes.
   qint64 bytesSkipped = 0;
   int chunkNum = 0;
   while (
      chunkNum < this->chunks.size() &&
      this->chunks[chunkNum]->hasHash() &&
      this->chunks[chunkNum]->getKnownBytes() == CHUNK_SIZE) // Maybe the file has grown and the last chunk must be recomputed.
   {
      bytesSkipped += CHUNK_SIZE;
      chunkNum++;
      file.seek(file.pos() + CHUNK_SIZE);
   }

#if DEBUG
   QElapsedTimer timer;
   timer.start();
#endif

   char buffer[this->BUFFER_SIZE];
   bool endOfFile = false;
   qint64 bytesReadTotal = 0;
   while (!endOfFile)
   {
      // See 'stopHashing()'.
      locker.unlock();
      locker.relock();

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
         int bytesRead = file.read(buffer, this->BUFFER_SIZE);
         switch (bytesRead)
         {
         case -1:
            L_ERRO(QString("Error during reading the file %1").arg(this->getFullPath()));
         case 0:
            endOfFile = true;
            goto endReading;
         }

         hasher.addData(buffer, bytesRead);

         bytesReadChunk += bytesRead;
      }
      endReading:

      bytesReadTotal += bytesReadChunk;

      if (bytesReadChunk > 0)
      {
         if (this->chunks.size() <= chunkNum) // The size of the file has increased during the read..
            this->chunks.append(QSharedPointer<Chunk>(new Chunk(this, chunkNum, bytesReadChunk, hasher.getResult())));
         else
         {
            if (this->chunks[chunkNum]->hasHash())
               this->cache->onChunkRemoved(this->chunks[chunkNum]); // To remove the chunk from the chunk index (TODO : fin a more elegant way).

            this->chunks[chunkNum]->setHash(hasher.getResult());
            this->chunks[chunkNum]->setKnownBytes(bytesReadChunk);
         }

         this->cache->onChunkHashKnown(this->chunks[chunkNum]);
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
      L_DEBU(QString("Hashing speed : %1").arg(Common::Global::formatByteSize(speed)));
   }
#endif

   this->toStopHashing = false;
   this->hashing = false;

   if (bytesReadTotal + bytesSkipped != this->size)
   {
      L_DEBU(QString("The file content has changed during the hashes computing process. File = %1, bytes read = %2, previous size = %3").arg(this->getFullPath()).arg(bytesReadTotal).arg(this->size));
      this->dir->fileSizeChanged(this->size, bytesReadTotal + bytesSkipped);
      this->size = bytesReadTotal + bytesSkipped;
      this->dateLastModified = QFileInfo(this->getFullPath()).lastModified();

      if (bytesReadTotal + bytesSkipped < this->size) // In this case, maybe some chunk must be deleted.
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
   QMutexLocker locker(&this->mutex);
   if (this->size == 0)
      return false;

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

/**
  * A file is complete when all its chunk has been downloaded.
  * The '.unfinished' suffix of the complete file will be removed later, see 'setAsComplete'.
  */
bool File::isComplete()
{
   QMutexLocker locker(&this->mutex);
   return this->complete;
}

/**
  * Called from a downloading thread.
  * Set the file as complete, change its name from "<name>.unfinished" to "<name>".
  * If a file with the same name already exists it will be deleted.
  * The rename process can be only made if there is no reader, in a such case we will wait for the last reader finished.
  * TODO : if the rename fail must we attempt later ? With a timer ?
  */
void File::setAsComplete()
{
   QMutexLocker locker(&this->mutex);

   L_DEBU(QString("File set as complete : %1").arg(this->getFullPath()));

   this->complete = true;

   if (Cache::isFileUnfinished(this->name))
   {
      if (this->numDataReader > 0)
      {
         L_DEBU(QString("Delay file renaming, %1 reader(s)").arg(this->numDataReader));
         this->tryToRename = true;
         return;
      }
      this->tryToRename = false;

      if (this->fileInWriteMode)
      {
         this->fileInWriteMode->close();
         this->fileInWriteMode = 0;
      }

      const int PREFIX_SIZE = SETTINGS.get<QString>("unfinished_suffix_term").size();
      const QString oldPath = this->getFullPath();
      const QString newPath = oldPath.left(oldPath.size() - PREFIX_SIZE);

      if (!Common::Global::rename(oldPath, newPath))
      {
         L_ERRO(QString("Unable to rename the file %1 to %2").arg(oldPath).arg(newPath));
      }
      else
      {
         this->dateLastModified = QFileInfo(newPath).lastModified();
         this->name = this->name.left(this->name.size() - PREFIX_SIZE);
         this->cache->onEntryAdded(this); // To add the name to the index. (a bit tricky).
      }
   }
}

void File::chunkComplete(const Chunk* chunk)
{
   QMutexLocker locker(&this->mutex);

   // TODO : very cpu consumer! We have to find a better way!
   for (int i = 0; i < this->chunks.size(); i++)
      if (this->chunks[i].data() == chunk)
      {
         this->cache->onChunkHashKnown(this->chunks[i]);
         break;
      }

   if (++this->nbChunkComplete == this->getNbChunks())
   {
      this->setAsComplete();
   }
}

int File::getNbChunks()
{
   return this->size / CHUNK_SIZE + (this->size % CHUNK_SIZE == 0 ? 0 : 1);
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
  * The file removed must ended by the setting "unfinished_suffix_term".
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

/**
  * If dir is a parent dir of the file return true.
  */
bool File::hasAParentDir(Directory* dir)
{
   if (this->dir == dir)
      return true;
   return this->dir->isAChildOf(dir);
}

void File::deleteAllChunks()
{
   foreach (QSharedPointer<Chunk> c, this->chunks)
      this->cache->onChunkRemoved(c);
   this->chunks.clear();
}

/**
  * Create a new physical file, using when a new download begins. The new filename must end with ".unfinished".
  * @exception UnableToCreateNewFileException
  */
void File::createPhysicalFile()
{
   if (!Cache::isFileUnfinished(this->name))
      L_ERRO(QString("File::createPhysicalFile(..) : Cannot create a file (%1) without the 'unfinished' suffix").arg(this->getPath()));
   else if (static_cast<SharedDirectory*>(this->getRoot())->getRights() == SharedDirectory::READ_ONLY)
      L_ERRO(QString("File::createPhysicalFile(..) : Cannot create a file (%1) in a read only shared directory (%2)").arg(this->getPath()).arg(static_cast<SharedDirectory*>(this->getRoot())->getFullPath()));
   else
   {
      QFile file(this->getFullPath());
      if (!file.open(QIODevice::WriteOnly) || !file.resize(this->size))
         throw UnableToCreateNewFileException();
      this->dateLastModified = QFileInfo(file).lastModified();
   }
}

void File::setHashes(const Common::Hashes& hashes)
{
   if (!hashes.isEmpty())
      if (this->getNbChunks() != hashes.size()) // It can occur when IFileManager::newFile(..) is called with an entry not owning all its hashes.
         L_WARN(QString("File::File(..) : The number of hashes (%1) doesn't correspond to the calculate number of chunk (%2)").arg(hashes.size()).arg(this->getNbChunks()));

   for (int i = 0; i < this->getNbChunks(); i++)
   {
      int chunkKnownBytes = !this->isComplete() ? 0 : i == this->getNbChunks() - 1 && this->size % CHUNK_SIZE != 0 ? this->size % CHUNK_SIZE : CHUNK_SIZE;

      if (i < hashes.size())
      {
         QSharedPointer<Chunk> chunk(new Chunk(this, i, chunkKnownBytes, hashes[i]));
         this->chunks << chunk;
         if (chunk->isComplete())
            this->cache->onChunkHashKnown(chunk);
      }
      else
         // If there is too few hashes then null hashes are added.
         this->chunks.append(QSharedPointer<Chunk>(new Chunk(this, i, chunkKnownBytes)));
   }
}
