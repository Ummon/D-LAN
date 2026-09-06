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
#else
   #include <unistd.h>
#endif

#include <QString>
#include <QFile>
#include <QScopeGuard>

#include <Common/KnownExtensions.h>
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
#include <priv/Cache/SharedEntry.h>
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
  * When a file becomes complete the suffix ".unfinished" is removed.
  */

/**
  * Create a new file into a given directory.
  * The file may or may not have a corresponding local file.
  * If 'createPhysically' is true then the file is created as unfinished with no byte known.
  *
  * @param dir The directory that owns the file.
  * @param name The name of the file.
  * @param size The size of the file in byte.
  * @param dateLastModified The date of the last modification of the file.
  * @param hashes Optional hashes, if given it must contain ALL hashes.
  * @param createPhysically If 'true' the file will be created. Default is 'false'.
  * @exception UnableToCreateNewFileException
  */
File::File(
   SharedEntry* root,
   const QString& name,
   qint64 size,
   bool hidden,
   const QDateTime& dateLastModified,
   Directory* parentDirectory,
   const QList<Common::Hash>& hashes,
   bool createPhysically
) :
   Entry(
      root,
      name + (createPhysically && size > 0 ? Global::getUnfinishedSuffix() : ""),
      parentDirectory,
      size,
      hidden
   ),
   dateLastModified(dateLastModified),
   complete(!Global::isFileUnfinished(Entry::getName())),
   numDataWriter(0),
   numDataReader(0),
   fileInWriteMode(nullptr),
   fileInReadMode(nullptr)
{
   QMutexLocker locker(&this->mutex);

   L_DEBU(
      QString("New file: %1 (%2), createPhysically = %3, dateLastModified = %4")
         .arg(this->File::getAbsolutePath().toString(), Common::Global::formatByteSize(this->getSize()))
         .arg(createPhysically)
         .arg(dateLastModified.toString("dd.MM.yyyy-hh:mm:ss.zzz"))
   );

   if (auto cache = this->getCache())
   {
      if (!createPhysically && hashes.isEmpty())
         this->loadHashes();

      cache->onEntryAdded(this);
   }

   if (createPhysically)
      try
      {
         this->createPhysicalFile();
      }
      catch (UnableToCreateNewFileException&)
      {
         Entry::del(false);
         throw;
      }

   if (this->chunks.isEmpty())
      this->setHashes(hashes);

   if (this->parentDirectory)
      this->parentDirectory->add(this);
}

File::~File()
{
   L_DEBU(QString("File deleted: %1").arg(this->getAbsolutePath()));

   // A file deleted without a prior call to 'del()' (by its parent directory or at shutdown) must still leave the indexes.
   if (!this->deletePending.exchange(true))
      this->getCache()->onEntryRemoved(this);

   QMutexLocker locker(&this->mutex);
   this->deleteAllChunks();
}

void File::del(bool invokeDelete)
{
   {
      QMutexLocker locker(&this->mutex);
      if (this->deletePending)
         return;

      // Use the same child-to-parent order as completion, including detaching directory membership.
      if (this->parentDirectory)
         this->parentDirectory->fileDeleted(this);

      this->deleteAllChunks();
   }
   // Removal notifications may wait for the hasher; do not hold the file mutex here.
   Entry::del(invokeDelete);
}

/**
  * Closes the files opened in read and write mode, if any.
  * The pool matches files by pointer, so the pointers are reset to avoid releasing a file reopened at the same address.
  */
void File::closePhysicalFiles()
{
   QMutexLocker fileLocker(&this->mutex);
   QMutexLocker lockerWrite(&this->writeLock);
   this->getCache()->getFilePool().release(this->fileInWriteMode, true);
   this->fileInWriteMode = nullptr;
   this->numDataWriter = 0;

   QMutexLocker lockerRead(&this->readLock);
   this->getCache()->getFilePool().release(this->fileInReadMode, true);
   this->fileInReadMode = nullptr;
   this->numDataReader = 0;
}

/**
  * Set the file as unfinished, this is use when an existing file is re-downloaded.
  * The file is removed from the index and a new physically file named "<name>.unfinished" is created.
  * The old physical file is not removed and will be replaced only when this one is finished.
  * @exception UnableToCreateNewFileException
  */
void File::setToUnfinished(qint64 size, const QList<Common::Hash>& hashes)
{
   QMutexLocker locker(&this->mutex);
   L_DEBU(QString("File::setToUnfinished: %1").arg(this->getAbsolutePath().toString()));

   // Reject new hashing work before stopping the current hasher. Removal callbacks
   // acquire the hashing mutex and may wait for file access, so release our mutex
   // until they return. Keep the old name and chunks intact during that wait.
   this->complete.store(false, std::memory_order_release);
   locker.unlock();
   this->getCache()->onEntryRemoved(this);
   locker.relock();

   this->getCache()->getHashCache()->rmHashes(this->getAbsolutePath());
   this->name.append(Global::getUnfinishedSuffix());
   if (this->parentDirectory)
      this->parentDirectory->fileNameChanged(this);
   this->setSize(size);
   this->dateLastModified = QDateTime::currentDateTime();
   this->deleteAllChunks();
   this->setHashes(hashes);

   this->createPhysicalFile();
}

/**
  * If all hashes are known, save them to the hash cache. Only completed files hashes are saved.
  */
void File::saveHashes()
{
   QMutexLocker locker(&this->mutex);

   if (!this->complete)
      return;

   QList<Common::Hash> hashes;
   for (const auto& chunk : std::as_const(this->chunks))
      hashes << chunk->getHash();

   this->getCache()->getHashCache()->setHashes(this->getAbsolutePath(), hashes, this->size, this->dateLastModified);
}

void File::loadHashes()
{
   QMutexLocker locker(&this->mutex);

   const QList<Common::Hash> hashes =
      this->getCache()->getHashCache()->getHashes(
         this->getAbsolutePath(),
         this->dateLastModified
      );

   this->setHashes(hashes);
}

/**
  * Will add the hashes to the entry.
  */
void File::populateEntry(Protos::Common::Entry* entry, bool setSharedDir) const
{
   this->populateEntry(entry, setSharedDir, std::numeric_limits<int>::max());
}

void File::populateEntry(Protos::Common::Entry* entry, bool setSharedDir, int maxHashes) const
{
   QMutexLocker locker(&this->mutex);

   Entry::populateEntry(entry, setSharedDir);

   entry->set_type(Protos::Common::Entry_Type_FILE);

   entry->clear_chunks();

   int nb = 0;
   for (QListIterator<QSharedPointer<Chunk>> i(this->chunks); i.hasNext();)
   {
      Protos::Common::Hash* protoHash = entry->add_chunks();

      Common::Hash hash = i.next()->getHash();
      if (!hash.isNull() && ++nb <= maxHashes)
         protoHash->set_hash(hash.getData(), Common::Hash::HASH_SIZE);
   }
}

bool File::matchesEntry(const Protos::Common::Entry& entry) const
{
   QMutexLocker locker(&this->mutex);

   return
      this->getRoot()->getId() == entry.shared_entry().id().hash() &&
      this->getRelativePath().getDirs() == Common::ProtoHelper::getPath(entry).getDirs() &&
      this->getSize() == static_cast<qint64>(entry.size()) &&
      Global::removeUnfinishedSuffix(this->getName()) ==
         Global::removeUnfinishedSuffix(QString::fromStdString(entry.name()));
}

/**
  * Return true if the size and the last modification date correspond to the given file information.
  */
bool File::correspondTo(const QFileInfo& fileInfo, bool checkTheDateToo) const
{
   QMutexLocker locker(&this->mutex);

   return
      this->getSize() == fileInfo.size() &&
      (!checkTheDateToo || this->getDateLastModified() == fileInfo.lastModified());
}

void File::fileHasChangedOnDisk(const QFileInfo fileInfo)
{
   // L_DEBU(QString("~~~ File::fileHasChangedOnDisk, chunks size: %1").arg(this->chunks.size()));
   // L_DEBU(QString("~~~ file.size: %1, fileInfo.size: %2").arg(this->getSize()).arg(fileInfo.size()));
   // L_DEBU(QString("~~~ file.dateLastModified: %1, fileInfo.lastModified: %2").arg(this->dateLastModified.toString()).arg(fileInfo.lastModified().toString()));

   QMutexLocker locker(&this->mutex);

   this->setSize(fileInfo.size());
   this->dateLastModified = fileInfo.lastModified();

   this->getCache()->getHashCache()->rmHashes(this->getAbsolutePath());

   this->deleteAllChunks();
   QList<Common::Hash> hashes((qsizetype)this->getNbChunks());
   this->setHashes(hashes);
}

Common::Path File::getRelativePath() const
{
   QMutexLocker locker(&this->mutex);

   if (this->parentDirectory)
      return this->parentDirectory->getRelativePath().setFilename(this->name);
   else
      return Common::Path(this->name);
}

Common::Path File::getAbsolutePath() const
{
   QMutexLocker locker(&this->mutex);

   if (this->parentDirectory)
      return this->parentDirectory->getAbsolutePath().setFilename(this->name);
   else
      return this->getRoot()->path.setFilename(this->name);
}

Entry* File::getEntry(const Common::Path& path)
{
   QMutexLocker locker(&this->mutex);

   if (path.isFile() && !path.isAbsolute() && path.getDirs().isEmpty() && path.getFilename() == this->name)
      return this;
   return nullptr;
}

/**
  * Returns the extension of the file only if it is a known extension, see 'KnownExtensions'.
  */
QString File::getExtension() const
{
   return Common::KnownExtensions::getExtension(this->name);
}

void File::rename(const QString& newName)
{
   QMutexLocker locker(&this->mutex);

   Entry::rename(newName);

   if (this->parentDirectory)
      this->parentDirectory->fileNameChanged(this);
}

QDateTime File::getDateLastModified() const
{
   QMutexLocker locker(&this->mutex);
   return this->dateLastModified;
}

/**
  * @exception UnableToOpenFileInWriteModeException
  * @exception FileResetException
  */
void File::newDataWriterCreated()
{
   QMutexLocker fileLocker(&this->mutex);
   QMutexLocker locker(&this->writeLock);

   // Completion closes the handles for the rename while existing adapters remain registered.
   if (!this->fileInWriteMode)
   {
      // We have the same performance with or without "QIODevice::Unbuffered".
      bool fileCreated;
      this->fileInWriteMode =
         this->getCache()->getFilePool().open(
            this->getAbsolutePath(),
            QIODevice::ReadWrite | QIODevice::Unbuffered,
            &fileCreated
         );

      if (!this->fileInWriteMode)
         throw UnableToOpenFileInWriteModeException();

      // A failed DataWriter constructor has no matching dataWriterDeleted() call. Close the
      // newly acquired handle on failure and register the adapter only after setup succeeds.
      auto rollbackHandle = qScopeGuard([this] {
         this->getCache()->getFilePool().release(this->fileInWriteMode, true);
         this->fileInWriteMode = nullptr;
      });

      // If the file is created then we reset all the chunks.
      bool fileReset = false;
      if (fileCreated)
      {
         if (!this->fileInWriteMode->resize(this->getSize()))
            throw UnableToOpenFileInWriteModeException();

         this->setFileAsSparse(*this->fileInWriteMode);

         for (QListIterator<QSharedPointer<Chunk>> i(this->chunks); i.hasNext();)
         {
            QSharedPointer<Chunk> chunk = i.next();
            if (chunk->getKnownBytes() != 0)
            {
               chunk->setKnownBytes(0);
               this->getCache()->onChunkRemoved(chunk);
               fileReset = true;
            }
         }
      }

      if (fileReset)
         // A file has been deleted and we know some data.
         // For example a user has shut down D-LAN then has removed a previously downloading ".unfinished" file
         // then he has restarted D-LAN.
         throw FileResetException();

      rollbackHandle.dismiss();
   }
   ++this->numDataWriter;
}

/**
  * @exception UnableToOpenFileInReadModeException
  */
void File::newDataReaderCreated()
{
   QMutexLocker fileLocker(&this->mutex);
   QMutexLocker locker(&this->readLock);

   if (!this->openReadHandle())
      throw UnableToOpenFileInReadModeException();
   this->numDataReader++;
}

// Requires the metadata mutex and readLock. A retained reader can reopen the renamed file too.
bool File::openReadHandle()
{
   if (!this->fileInReadMode)
      this->fileInReadMode = this->getCache()->getFilePool().open(
         this->getAbsolutePath(), QIODevice::ReadOnly | QIODevice::Unbuffered);
   return this->fileInReadMode != nullptr;
}

/**
  * Adapter counts survive completion's handle closure. Release the current handle only when the last
  * adapter is destroyed, whether that adapter was created before or after the rename.
  */
void File::dataWriterDeleted()
{
   QMutexLocker fileLocker(&this->mutex);
   QMutexLocker locker(&this->writeLock);

   if (--this->numDataWriter == 0)
   {
      this->getCache()->getFilePool().release(this->fileInWriteMode);
      this->fileInWriteMode = nullptr;
   }
}

void File::dataReaderDeleted()
{
   QMutexLocker fileLocker(&this->mutex);
   QMutexLocker locker(&this->readLock);

   if (--this->numDataReader == 0)
   {
      this->getCache()->getFilePool().release(this->fileInReadMode);
      this->fileInReadMode = nullptr;
   }
}

/**
  * Write some bytes to the file at the given offset.
  * If the buffer exceed the file size then only the beginning of the buffer is
  * used, the file is not resizing.
  * @exception IOErrorException
  * @param buffer The buffer containing the data to write.
  * @param nbBytes The number of bytes my buffer contains.
  * @param offset The offset into the file where the data will be written.
  */
qint64 File::write(const char* buffer, int nbBytes, qint64 offset)
{
   QMutexLocker locker(&this->writeLock);

   if (nbBytes < 0 || (nbBytes > 0 && !buffer) || offset < 0 ||
       !this->fileInWriteMode || offset >= this->getSize() || !this->fileInWriteMode->seek(offset))
      throw IOErrorException();

   const qint64 maxSize = this->getSize() - offset;
   const qint64 sizeToWrite = nbBytes > maxSize ? maxSize : nbBytes;
   qint64 written = 0;
   while (written < sizeToWrite)
   {
      const qint64 n = this->writePhysicalFile(buffer + written, sizeToWrite - written);
      if (n <= 0 || n > sizeToWrite - written)
         throw IOErrorException();
      written += n;
   }

   // Chunk::write advances knownBytes only after the whole request succeeds. On failure a retry
   // seeks back to the same offset and overwrites any uncommitted partial data.
   return written;
}

qint64 File::writePhysicalFile(const char* buffer, qint64 nbBytes)
{
   return this->fileInWriteMode->write(buffer, nbBytes);
}

/**
  * Ask the OS to write the cached data of the file to the disk.
  * Called by 'Chunk::write(..)' each time a chunk is complete: without this the whole cached data would be flushed
  * in one go when the file is closed in 'setAsComplete()', it can take several seconds for a big file and during
  * this time the peer is unable to answer to the other peers.
  * Only the calling thread (a downloader) waits for the disk.
  */
void File::flushWrittenData()
{
   QMutexLocker locker(&this->writeLock);

   if (!this->fileInWriteMode)
      return;

   const int fd = this->fileInWriteMode->handle();
   if (fd == -1)
      return;

#ifdef Q_OS_WIN32
   _commit(fd);
#else
   fsync(fd);
#endif
}

/**
  * Fill the buffer with the read bytes from the given offset.
  * If the end of file is reached the buffer will be partially filled.
  * @param buffer The buffer where my data will be put after the reading.
  * @param offset An offset into the file where the data will be read.
  * @param maxBytesToRead The number of bytes to read, the buffer size must be at least this value.
  * @return the number of bytes read.
  */
qint64 File::read(char* buffer, qint64 offset, int maxBytesToRead)
{
   QMutexLocker fileLocker(&this->mutex);
   QMutexLocker locker(&this->readLock);

   if (this->numDataReader == 0 || offset >= this->getSize())
      return 0;

   // Completion may have closed this reader's handle. Resolve the current path under the same
   // metadata lock as the rename, so a missing handle is never mistaken for end-of-file.
   if (!this->openReadHandle())
      throw IOErrorException();

   if (!this->fileInReadMode->seek(offset))
      throw IOErrorException();

   const qint64 bytesRead = this->fileInReadMode->read(buffer, maxBytesToRead);

   if (bytesRead == -1)
      throw IOErrorException();

   return bytesRead;
}

QList<QSharedPointer<Chunk>> File::getChunks() const
{
   QMutexLocker locker(&this->mutex);
   return this->chunks;
}

bool File::hasAllHashes() const
{
   QMutexLocker locker(&this->mutex);

   if (this->getSize() == 0)
      return false;

   for (QListIterator<QSharedPointer<Chunk>> i(this->chunks); i.hasNext();)
      if (!i.next()->hasHash())
         return false;

   return true;
}

bool File::hasOneOrMoreHashes() const
{
   for (QListIterator<QSharedPointer<Chunk>> i(this->chunks); i.hasNext();)
     if (i.next()->hasHash())
         return true;
   return false;
}

/**
  * A file is complete when all its chunk has been downloaded and the ".unfinished" suffix has been removed.
  */
bool File::isComplete() const
{
   return this->complete.load(std::memory_order_acquire);
}

void File::chunkComplete(const Chunk* chunk)
{
   QMutexLocker locker(&this->mutex);

   int nbChunkComplete = 0;
   for (int i = 0; i < this->chunks.size(); ++i)
   {
      if (this->chunks[i].data() == chunk)
         this->getCache()->onChunkHashKnown(this->chunks[i]);

      if (this->chunks[i]->isComplete())
         ++nbChunkComplete;
   }

   if (nbChunkComplete == this->getNbChunks())
      this->setAsComplete();
}

int File::getNbChunks() const
{
   QMutexLocker locker(&this->mutex);

   return Common::Global::nbChunks(this->getSize());
}

void File::setSize(qint64 size)
{
   QMutexLocker locker(&this->mutex);

   if (this->size != size)
   {
      this->getCache()->onFileResizing(this);
      qint64 oldSize = this->size;
      Entry::setSize(size);
      this->getCache()->onFileResized(this, oldSize);

      if (this->parentDirectory)
         this->parentDirectory->fileSizeChanged(oldSize, size);
   }
}

void File::deleteIfIncomplete()
{
   this->mutex.lock();

   if (!this->complete)
   {
      this->removeUnfinishedFiles();
      this->mutex.unlock();
      this->del();
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

      this->getCache()->getFilePool().forceReleaseAll(this->getAbsolutePath());

      this->fileInReadMode = nullptr;
      this->fileInWriteMode = nullptr;

      // this->getCache()->getHashCache()->rmHashes(this->getAbsolutePath());

      if (!QFile::remove(this->getAbsolutePath()))
         L_WARN(QString("File::removeUnfinishedFiles(): unable to delete an unfinished file: %1").arg(this->getAbsolutePath()));
   }
}

void File::moveInto(Directory* directory)
{
   QMutexLocker locker(&this->mutex);

   if (this->parentDirectory == directory)
      return;

   if (this->parentDirectory)
      this->parentDirectory->fileDeleted(this);

   // A shared root must not be moved with this method, see 'SharedEntry::moveInto(..)'.
   if (this->getRoot() != directory->getRoot())
      this->setRootRecursively(directory->getRoot());

   directory->add(this);
   this->parentDirectory = directory;
}

/**
  * If dir is a parent dir of the file return true.
  */
bool File::hasAParentDir(Directory* dir)
{
   QMutexLocker locker(&this->mutex);

   if (this->parentDirectory == dir)
      return true;
   else if (this->parentDirectory)
      return this->parentDirectory->isAChildOf(dir);
   else
      return false;
}

/**
  * Called from a downloading thread.
  * Set the file as complete, change its name from "<name>.unfinished" to "<name>".
  * If a file with the same name already exists it will be deleted.
  * Close the handles while excluding active I/O and new openers. Existing readers reopen lazily after the rename.
  */
void File::setAsComplete()
{
   L_DEBU(QString("File set as complete: %1").arg(this->getAbsolutePath()));

   if (Global::isFileUnfinished(this->name))
   {
      const QString oldPath = this->getAbsolutePath();
      const QString newPath = Global::removeUnfinishedSuffix(oldPath);

      // The opened handles are taken from the pool while holding the read and write locks but they are closed
      // after releasing the I/O locks: on Windows 'CloseHandle(..)' flushes all the cached data and can block
      // for several seconds with a big file (see 'flushWrittenData()' which limits the amount of data to flush here).
      // Keep the metadata mutex held so chunk retirement cannot destroy the file during completion.
      QList<QFile*> filesToClose;
      {
         QMutexLocker lockerWrite(&this->writeLock);
         QMutexLocker lockerRead(&this->readLock);
         filesToClose = this->getCache()->getFilePool().takeAll(oldPath);
         this->fileInReadMode = nullptr;
         this->fileInWriteMode = nullptr;
      }

      for (QFile* file : std::as_const(filesToClose))
         delete file;

      if (!Common::Global::rename(oldPath, newPath))
      {
         L_ERRO(QString("Unable to rename the file %1 to %2").arg(oldPath, newPath));
      }
      else
      {
         this->complete.store(true, std::memory_order_release);
         if (this->hidden)
            this->setFileAsHidden(newPath);
         this->dateLastModified = QFileInfo(newPath).lastModified();
         this->name = Global::removeUnfinishedSuffix(this->name);
         if (this->parentDirectory)
            this->parentDirectory->fileNameChanged(this);
         this->saveHashes();
         this->getCache()->onEntryAdded(this); // To add the name to the index. (a bit tricky).
      }
   }
}

void File::deleteAllChunks()
{
   QMutexLocker locker(&this->mutex);
   // Retained chunks must not access a replacement file generation or outlive the owning File.
   for (const auto& chunk : std::as_const(this->chunks))
   {
      chunk->fileDeleted();
      this->getCache()->onChunkRemoved(chunk);
   }
   this->chunks.clear();

   // Readers and writers of detached chunks no longer call back into this file when destroyed.
   this->closePhysicalFiles();
}

/**
  * Create a new physical file, using when a new download begins. The new filename must end with ".unfinished".
  * @exception UnableToCreateNewFileException
  */
void File::createPhysicalFile()
{
   if (this->getSize() > 0 && !Global::isFileUnfinished(this->name))
      L_ERRO(
         QString("File::createPhysicalFile(..): Cannot create a file (%1) without the '%2' suffix")
            .arg(this->File::getRelativePath(), Global::getUnfinishedSuffix())
      );
   else
   {
      QFile file(this->File::getAbsolutePath());
      if (!file.open(QIODevice::WriteOnly) || !file.resize(this->getSize()))
      {
         QFile::remove(this->File::getAbsolutePath());
         throw UnableToCreateNewFileException();
      }
      this->setFileAsSparse(file);
      this->dateLastModified = QFileInfo(file).lastModified();
   }
}

void File::setFileAsSparse(const QFile& file)
{
// TODO: Do we need that on linux? see 'fallocate(..)',
#ifdef Q_OS_WIN32
   DWORD bytesWritten;
   HANDLE hdl = (HANDLE)_get_osfhandle(file.handle());
   // To avoid to initialize and write all data.
   // File initialization can take several minutes for a large file (> 5 GiB).
   // See : http://msdn.microsoft.com/en-us/library/aa364596%28v=vs.85%29.aspx
   if (!DeviceIoControl(hdl, FSCTL_SET_SPARSE, NULL, 0, NULL, 0, &bytesWritten, NULL))
      L_WARN("DeviceIoControl(...) failed");
#endif
}

void File::setFileAsHidden(const QString& filepath)
{
#ifdef Q_OS_WIN32
   const DWORD attrs = GetFileAttributesW((LPCWSTR)filepath.utf16());
   if (
      attrs == INVALID_FILE_ATTRIBUTES ||
      !SetFileAttributesW((LPCWSTR)filepath.utf16(), attrs | FILE_ATTRIBUTE_HIDDEN)
   )
      L_WARN(QString("Unable to set the hidden attribute on %1").arg(filepath));
#else
   Q_UNUSED(filepath)
#endif
}

/**
  * The number of given hashes may not match the total number of chunk.
  */
void File::setHashes(const QList<Common::Hash>& hashes)
{
   QMutexLocker locker(&this->mutex);

   this->chunks.reserve(this->getNbChunks());
   for (int i = 0; i < this->getNbChunks(); i++)
   {
      int chunkKnownBytes =
         !this->isComplete()
            ? 0
            : i == this->getNbChunks() - 1 && this->getSize() % Chunk::CHUNK_SIZE != 0
               ? this->getSize() % Chunk::CHUNK_SIZE
               : Chunk::CHUNK_SIZE;

      if (i < hashes.size() && !hashes[i].isNull())
      {
         QSharedPointer<Chunk> chunk(new Chunk(this, i, chunkKnownBytes, hashes[i]));
         this->chunks << chunk;
         if (chunk->isComplete())
            this->getCache()->onChunkHashKnown(chunk);
      }
      else
         // If there is too few hashes then null hashes are added.
         this->chunks << QSharedPointer<Chunk>(new Chunk(this, i, chunkKnownBytes));
   }
}

void File::setRootRecursively(SharedEntry* sharedEntry)
{
   QMutexLocker locker(&this->mutex);
   this->root = sharedEntry;
}

void File::updateDateLastModified(const QDateTime& date)
{
   QMutexLocker locker(&this->mutex);
   this->dateLastModified = date;
}

/////

/**
  * @class FM::FileIterator
  *
  * Iterate recursively over all files in the tree structure.
  */

FileIterator::FileIterator(Entry* entry)
{
   if (File* file = dynamic_cast<File*>(entry))
      this->nextFiles << file;
   else if (Directory* dir = dynamic_cast<Directory*>(entry))
      this->dirsToVisit << dir;
}

/**
  * Return the next file, 0 if there is no more directory.
  */
File* FileIterator::next()
{
   if (!this->nextFiles.isEmpty())
   {
      File* file = this->nextFiles.front();
      this->nextFiles.removeFirst();
      return file;
   }

   if (this->dirsToVisit.isEmpty())
      return nullptr;

   Directory* dir = this->dirsToVisit.front();
   this->dirsToVisit.removeFirst();
   this->dirsToVisit << dir->getSubDirs();
   this->nextFiles << dir->getFiles();

   return this->next();
}
