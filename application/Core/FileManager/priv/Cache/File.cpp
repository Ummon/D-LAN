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
  
#include <priv/Cache/File.h>
using namespace FM;

#ifdef Q_OS_WIN32
   #include <io.h>
   #include <windows.h>
   #include <WinIoCtl.h>
#endif

#include <QString>
#include <QFile>

#include <Common/Global.h>
#include <Common/Settings.h>
#include <Common/ProtoHelper.h>

#include <Exceptions.h>
#include <priv/Global.h>
#include <priv/Exceptions.h>
#include <priv/Log.h>
#include <priv/Constants.h>
#include <priv/Cache/Cache.h>
#include <priv/Cache/Directory.h>
#include <priv/Cache/SharedDirectory.h>
#include <priv/Cache/Chunk.h>

/**
  * @class FM::File
  *
  * Represents a physical file, it knows its name, size and last modified date.
  * Capabilities :
  *  - Create a new file (which becomes an unfinished file). It's used when downloading a remote file.
  *  - Read or write the file.
  *
  * A file can be finished or unfinished.
  * If it is an unfinished one, the name ends with ".unfinished" (see setting 'unfinished_suffix_term').
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
) :
   Entry(dir->getCache(), name + (createPhysically ? Global::getUnfinishedSuffix() : ""), size),
   dir(dir),
   dateLastModified(dateLastModified),
   nbChunkComplete(0),
   complete(!Global::isFileUnfinished(Entry::getName())),
   numDataWriter(0),
   numDataReader(0),
   fileInWriteMode(0),
   fileInReadMode(0),
   mutex(QMutex::Recursive)
{
   L_DEBU(QString("New file : %1 (%2), createPhysically = %3").arg(this->getFullPath()).arg(Common::Global::formatByteSize(this->size)).arg(createPhysically));

   if (createPhysically)
      this->createPhysicalFile();

   this->setHashes(hashes);

   this->dir->add(this);
}

File::~File()
{
   // QMutexLocker(&this->cache->getMutex()); // Is it necessary ?

   this->dir->fileDeleted(this);

   foreach (QSharedPointer<Chunk> c, this->chunks)
      c->fileDeleted();

   this->deleteAllChunks();

   QMutexLocker lockerWrite(&this->writeLock);
   this->cache->getFilePool().release(this->fileInWriteMode, true);

   QMutexLocker lockerRead(&this->readLock);
   this->cache->getFilePool().release(this->fileInReadMode, true);

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
   L_DEBU(QString("File::setToUnfinished : %1").arg(this->getFullPath()));

   this->complete = false;
   this->cache->onEntryRemoved(this);
   this->name.append(Global::getUnfinishedSuffix());
   this->size = size;
   this->dateLastModified = QDateTime::currentDateTime();
   this->nbChunkComplete = 0;
   this->deleteAllChunks();
   this->setHashes(hashes);

   this->createPhysicalFile();
}

bool File::restoreFromFileCache(const Protos::FileCache::Hashes::File& file)
{
   if (
      Common::ProtoHelper::getStr(file, &Protos::FileCache::Hashes_File::filename) == this->getName() &&
      (qint64)file.size() == this->size &&
         (
            Global::isFileUnfinished(this->getName()) ||
            (qint64)file.date_last_modified() == this->getDateLastModified().toMSecsSinceEpoch() // We test the date only for finished files.
          ) &&
      this->chunks.size() == file.chunk_size()
   )
   {
      L_DEBU(QString("Restoring file '%1' from the file cache").arg(this->getFullPath()));

      for (int i = 0; i < file.chunk_size(); i++)
      {
         this->chunks[i]->restoreFromFileCache(file.chunk(i));
         if (this->chunks[i]->hasHash())
         {
            if (this->chunks[i]->isComplete())
               this->nbChunkComplete++;

            if (this->chunks[i]->getKnownBytes() > 0)
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

bool File::matchesEntry(const Protos::Common::Entry& entry) const
{
   QMutexLocker locker(&this->mutex);

   return
      this->getRoot()->getId() == entry.shared_dir().id().hash() &&
      this->getPath() == Common::ProtoHelper::getStr(entry, &Protos::Common::Entry::path) &&
      this->getSize() == static_cast<qint64>(entry.size()) &&
      Global::removeUnfinishedSuffix(this->getName()) == Global::removeUnfinishedSuffix(Common::ProtoHelper::getStr(entry, &Protos::Common::Entry::name));
}

/**
  * Return true if the size and the last modification date correspond to the given file information.
  */
bool File::correspondTo(const QFileInfo& fileInfo, bool checkTheDateToo)
{
   return this->getSize() == fileInfo.size() && (!checkTheDateToo || this->getDateLastModified() == fileInfo.lastModified());
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
   return this->dir->getFullPath().append(this->name);
}

SharedDirectory* File::getRoot() const
{
   return this->dir->getRoot();
}

void File::changeName(const QString& newName)
{
   Entry::changeName(newName);
   this->dir->fileNameChanged(this);
}

QDateTime File::getDateLastModified() const
{
   return this->dateLastModified;
}

/**
  * @exception UnableToOpenFileInWriteModeException
  */
void File::newDataWriterCreated()
{
   QMutexLocker locker(&this->writeLock);

   this->numDataWriter++;
   if (this->numDataWriter == 1)
   {
      // We have the same performance with or without "QIODevice::Unbuffered".
      this->fileInWriteMode = this->cache->getFilePool().open(this->getFullPath(), QIODevice::ReadWrite | QIODevice::Unbuffered);
      if (!this->fileInWriteMode)
         throw UnableToOpenFileInWriteModeException();
   }
}

/**
  * @exception UnableToOpenFileInReadModeException
  */
void File::newDataReaderCreated()
{
   QMutexLocker locker(&this->readLock);

   this->numDataReader++;
   if (this->numDataReader == 1)
   {
      // Why a file in readonly need to be buffered? Without the flag "QIODevice::Unbuffered" a lot of memory is consumed for nothing
      // and this memory is not freed when the file is closed ('close()') but only when the QFile is deleted.
      this->fileInReadMode = this->cache->getFilePool().open(this->getFullPath(), QIODevice::ReadOnly | QIODevice::Unbuffered);
      if (!this->fileInReadMode)
         throw UnableToOpenFileInReadModeException();
   }
}

/**
  * 'setAsComplete()' must be called before 'dataWriter' and 'dataReader' are deleted.
  * This is a bit tricky... We should use a signal 'void fileClosed(QFile* )' in 'FilePool' connected to a slot in the 'File' class.
  * In this case, 'File' must inherits 'QObject' which is actually too heavy.
  */
void File::dataWriterDeleted()
{
   QMutexLocker locker(&this->writeLock);

   if (--this->numDataWriter == 0)
   {
      this->cache->getFilePool().release(this->fileInWriteMode);
      this->fileInWriteMode = 0;
   }
}

void File::dataReaderDeleted()
{
   QMutexLocker locker(&this->readLock);

   if (--this->numDataReader == 0)
   {
      this->cache->getFilePool().release(this->fileInReadMode);
      this->fileInReadMode = 0;
   }
}

/**
  * Write some bytes to the file at the given offset.
  * If the buffer exceed the file size then only the begining of the buffer is
  * used, the file is not resizing.
  * @exception IOErrorException
  * @param buffer The buffer containing the data to write.
  * @param nbBytes The number of bytes my buffer contains.
  * @param offset The offset into the file where the data will be written.
  */
qint64 File::write(const char* buffer, int nbBytes, qint64 offset)
{
   QMutexLocker locker(&this->writeLock);

   if (!this->fileInWriteMode || offset >= this->size || !this->fileInWriteMode->seek(offset))
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
  * @param buffer The buffer where my data will be put after the reading.
  * @param offset An offset into the file where the data will be read.
  * @param maxBytesToRead The number of bytes to read, the buffer size must be at least this value.
  * @return the number of bytes read.
  */
qint64 File::read(char* buffer, qint64 offset, int maxBytesToRead)
{
   QMutexLocker locker(&this->readLock);

   if (!this->fileInReadMode || offset >= this->size)
      return 0;

   if (!this->fileInReadMode->seek(offset))
      throw IOErrorException();

   qint64 bytesRead = this->fileInReadMode->read(buffer, maxBytesToRead);

   if (bytesRead == -1)
      throw IOErrorException();

   return bytesRead;
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
   for (QListIterator< QSharedPointer<Chunk> > i(this->chunks); i.hasNext();)
     if (i.next()->hasHash())
         return true;
   return false;
}

/**
  * A file is complete when all its chunk has been downloaded and the ".unfinished" suffix has been removed.
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
  * TODO: if the rename fail must we attempt later? With a timer?
  */
void File::setAsComplete()
{
   QMutexLocker locker(&this->mutex);

   L_DEBU(QString("File set as complete : %1").arg(this->getFullPath()));

   if (Global::isFileUnfinished(this->name))
   {
      if (this->numDataReader > 0 || this->numDataWriter > 0)
      {
         QMutexLocker lockerWrite(&this->writeLock);
         QMutexLocker lockerRead(&this->readLock);
         this->cache->getFilePool().forceReleaseAll(this->getFullPath()); // Some uploads may be interrupted.
         this->fileInReadMode = 0;
         this->fileInWriteMode = 0;
      }

      const QString oldPath = this->getFullPath();
      const QString newPath = Global::removeUnfinishedSuffix(oldPath);

      if (!Common::Global::rename(oldPath, newPath))
      {
         L_ERRO(QString("Unable to rename the file %1 to %2").arg(oldPath).arg(newPath));
      }
      else
      {
         this->complete = true;
         this->dateLastModified = QFileInfo(newPath).lastModified();
         this->name = Global::removeUnfinishedSuffix(this->name);
         this->cache->onEntryAdded(this); // To add the name to the index. (a bit tricky).
      }
   }
}

void File::chunkComplete(const Chunk* chunk)
{
   QMutexLocker locker(&this->mutex);

   // TODO: very cpu consumer! We have to find a better way!
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
   static const int CHUNK_SIZE = SETTINGS.get<quint32>("chunk_size");
   return this->size / CHUNK_SIZE + (this->size % CHUNK_SIZE == 0 ? 0 : 1);
}

void File::deleteIfIncomplete()
{
   this->mutex.lock();

   if (!this->complete)
   {
      this->removeUnfinishedFiles();
      this->mutex.unlock();
      delete this;
      return;
   }

   this->mutex.unlock();
}

/**
  * Remove the file physically only if it's not complete.
  * The file removed must ended by the setting "unfinished_suffix_term".
  */
void File::removeUnfinishedFiles()
{
   QMutexLocker locker(&this->mutex);

   if (!this->complete)
   {
      QMutexLocker lockerWrite(&this->writeLock);
      QMutexLocker lockerRead(&this->readLock);

      this->cache->getFilePool().forceReleaseAll(this->getFullPath());
      this->fileInReadMode = 0;
      this->fileInWriteMode = 0;

      if (!QFile::remove(this->getFullPath()))
         L_WARN(QString("File::removeUnfinishedFiles() : unable to delete an unfinished file : %1").arg(this->getFullPath()));
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

void File::setSize(qint64 size)
{
   if (this->size != size)
   {
      this->dir->fileSizeChanged(this->size, size);
      this->size = size;
   }
}

void File::updateDateLastModified(const QDateTime& date)
{
   this->dateLastModified = date;
}

void File::addChunk(const QSharedPointer<Chunk>& chunk)
{
   this->chunks << chunk;
}

QSharedPointer<Chunk> File::removeLastChunk()
{
   return this->chunks.takeLast();
}

void File::deleteAllChunks()
{
   for (QListIterator< QSharedPointer<Chunk> > i(this->chunks); i.hasNext();)
      this->cache->onChunkRemoved(i.next());
   this->chunks.clear();
}

/**
  * Create a new physical file, using when a new download begins. The new filename must end with ".unfinished".
  * @exception UnableToCreateNewFileException
  */
void File::createPhysicalFile()
{
   if (!Global::isFileUnfinished(this->name))
      L_ERRO(QString("File::createPhysicalFile(..) : Cannot create a file (%1) without the 'unfinished' suffix").arg(this->getPath()));
   else
   {
      QFile file(this->getFullPath());
      if (!file.open(QIODevice::WriteOnly) || !file.resize(this->size))
      {
         QFile::remove(this->getFullPath());
         throw UnableToCreateNewFileException();
      }
#ifdef Q_OS_WIN32
      DWORD bytesWritten;
      HANDLE hdl = (HANDLE)_get_osfhandle(file.handle());
      // To avoid to initialize all the file, when you seek at the end of a file then you write some data the file will be initialized without this call.
      // File initialization can take several minutes for a large file (> 5 GiB).
      // See : http://msdn.microsoft.com/en-us/library/aa364596%28v=vs.85%29.aspx
      if (!DeviceIoControl(hdl, FSCTL_SET_SPARSE, NULL, 0, NULL, 0, &bytesWritten, NULL))
         L_WARN("DeviceIoControl(..) failed");
#endif
      this->dateLastModified = QFileInfo(file).lastModified();
   }
}

/**
  * The number of given hashes may not match the total number of chunk.
  */
void File::setHashes(const Common::Hashes& hashes)
{
   static const int CHUNK_SIZE = SETTINGS.get<quint32>("chunk_size");

   this->chunks.reserve(this->getNbChunks());
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
