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

#include <priv/FileUpdater/FileUpdater.h>
using namespace FM;

#include <QList>
#include <QSet>
#include <QDir>
#include <QDirIterator>
#include <QElapsedTimer>

#include <Common/Settings.h>

#include <Exceptions.h>
#include <priv/Global.h>
#include <priv/Exceptions.h>
#include <priv/Log.h>
#include <priv/Constants.h>
#include <priv/FileManager.h>
#include <priv/Cache/SharedEntry.h>
#include <priv/Cache/Directory.h>
#include <priv/Cache/File.h>
#include <priv/FileUpdater/WaitCondition.h>

namespace
{
   bool isHiddenEntry(const QFileInfo& info)
   {
#ifdef Q_OS_MACOS
      // Qt's macOS iterator metadata reports native hidden flags but can omit
      // dot-prefixed names. Preserve both without refreshing all scan metadata.
      if (info.fileName().startsWith('.'))
         return true;
#endif
      return info.isHidden();
   }

   bool entryTypeChanged(const Entry* entry, const QFileInfo& info)
   {
      return entry &&
         ((dynamic_cast<const File*>(entry) && info.isDir()) ||
          (dynamic_cast<const Directory*>(entry) && info.isFile()));
   }
}

/**
  * @class FM::FileUpdater
  *
  */

FileUpdater::FileUpdater(FileManager* fileManager) :
   SCAN_PERIOD_UNWATCHABLE_DIRS(SETTINGS.get<quint32>("scan_period_unwatchable_dirs")),
   IO_ERROR_WAITING_BEFORE_RETRY(3000),
   fileManager(fileManager),
   dirWatcher(DirWatcher::getNewWatcher()),
   toStop(false),
   currentScanningEntry(nullptr),
   toStopHashing(false)
{
   try
   {
      this->dirEvent = WaitCondition::getNewWaitCondition();
   }
   catch (...)
   {
      // The destructor is not called if construction fails.
      delete this->dirWatcher;
      throw;
   }
   this->schedulerClock.start();
}

FileUpdater::~FileUpdater()
{
   this->stop();

   if (this->dirEvent)
      delete this->dirEvent;

   if (this->dirWatcher)
      delete this->dirWatcher;

   L_DEBU("FileUpdater deleted");
}

void FileUpdater::stop()
{
   L_DEBU("Stopping FileUpdater...");

   this->toStop = true;

   L_DEBU("Stopping hashing...");
   this->stopHashing();
   L_DEBU("Hashing stopped");

   L_DEBU("Stopping scanning...");
   this->stopScanning();
   L_DEBU("Scanning stopped");

   // Simulate a dirEvent to stop the main loop.
   this->dirEvent->release();

   this->wait();
}

/**
  * Called by another thread.
  * An entry which can't be watched (no watcher, too many watched entries, access denied, ...) is periodically rescanned.
  */
void FileUpdater::addRoot(SharedEntry* sharedEntry)
{
   QMutexLocker locker(&this->mutex);

   L_DEBU(QString("FileUpdater: addRoot: %1").arg(sharedEntry->getPath()));

   const Common::Path& entryPath = sharedEntry->getPath();

   // Browse requests must wait until the initial scan has populated the root directory.
   if (auto directory = dynamic_cast<Directory*>(sharedEntry->getRootEntry()))
      directory->setScanned(false);

   bool watchable = false;
   if (this->dirWatcher)
   {
      try
      {
         watchable = this->dirWatcher->addPath(entryPath.toString(false), entryPath.getFilename());
      }
      catch (FileSystemEntryNotFoundException& e)
      {
         L_WARN(QString("Unable to watch this path: %1").arg(e.path));
      }
   }

   this->enqueueEntryToScan(sharedEntry->getRootEntry());

   if (!watchable)
   {
      L_WARN(QString("This entry is not watchable: %1").arg(entryPath.toString()));
      this->unwatchableEntries << sharedEntry->getRootEntry();
   }

   this->dirEvent->release();
}

// Follow a rename performed by the downloader, whose cached name is already updated.
void FileUpdater::updateRootPath(SharedEntry* sharedEntry, const Common::Path& oldPath)
{
   QMutexLocker locker(&this->mutex);
   if (!this->dirWatcher)
      return;
   this->dirWatcher->rmPath(oldPath.toString(false), oldPath.getFilename());
   const auto path = sharedEntry->getPath();
   bool watchable = false;
   try
   {
      watchable = this->dirWatcher->addPath(path.toString(false), path.getFilename());
   }
   catch (FileSystemEntryNotFoundException&)
   {
   }
   auto root = sharedEntry->getRootEntry();
   if (watchable)
      this->unwatchableEntries.removeOne(root);
   else if (!this->unwatchableEntries.contains(root))
      this->unwatchableEntries << root;
}

/**
  * Called by another thread.
  * If 'dir' is given it will steal the content of 'sharedEntry' and append
  * them to itself.
  */
void FileUpdater::rmRoot(SharedEntry* sharedEntry, Directory* dir)
{
   Entry* root = sharedEntry->getRootEntry();

   // If there is a scan for this directory stop it.
   this->stopScanning(root);

   QMutexLocker lockerHashing(&this->hashingMutex);
   QMutexLocker locker(&this->mutex);

   this->fileHasher.stop();
   this->toStopHashing = true;

   // TODO: Find a more elegant way!
   Directory* rootDirectory = dynamic_cast<Directory*>(root);
   if (dir && rootDirectory)
      dir->stealContent(rootDirectory);

   this->removeFromHashingQueue(root);
   this->removeFromEntriesToScan(root);
   this->unwatchableEntries.removeOne(root);
   this->rootEntriesToRemove << root;

   this->dirEvent->release();
}

void FileUpdater::prioritizeAFileToHash(File* file)
{
   QMutexLocker locker(&this->mutex);

   L_DEBU(QString("FileUpdater::prioritizeAFileToHash: %1").arg(file->getAbsolutePath()));

   const qint64 remaining = file->getRemainingBytesToHash();
   if (remaining > 0)
   {
      this->hashingQueue.enqueue(file, remaining, true);
      // Let the in-flight chunk finish. The next selection observes the new priority.
      this->dirEvent->release();
   }
   else
      L_DEBU(QString("FileUpdater::prioritizeAFileToHash, unable to prioritize: %1").arg(file->getAbsolutePath()));

}

// Called on the cache thread immediately before destruction, outside entry locks.
void FileUpdater::prepareToDeleteEntry(Entry* entry)
{
   this->stopScanning(entry);
   QMutexLocker hashingLocker(&this->hashingMutex);
   QMutexLocker locker(&this->mutex);
   this->removeFromHashingQueue(entry);
   this->removeFromEntriesToScan(entry);
}

bool FileUpdater::isScanning() const
{
   QMutexLocker scanningLocker(&this->scanningMutex);
   return this->currentScanningEntry != nullptr;
}

bool FileUpdater::isHashing() const
{
   QMutexLocker locker(&this->mutex);
   return !this->hashingQueue.isEmpty();
}

int FileUpdater::getProgress() const
{
   // Cache operations can call into the updater, so never acquire the cache mutex
   // while holding the scheduler mutex.
   const qint64 total = this->fileManager->getAmount();
   QMutexLocker locker(&this->mutex);
   if (total <= 0 || this->hashingQueue.isEmpty())
      return 0;
   const qint64 remaining = qBound<qint64>(0, this->hashingQueue.remainingBytes(), total);
   return static_cast<int>(10000.0L * (total - remaining) / total);
}

void FileUpdater::run()
{
   this->timerScanUnwatchable.start();

   QString threadName = "FileUpdater";
#if DEBUG
   threadName.append("_").append(QString::number((intptr_t)QThread::currentThreadId()));
#endif
   QThread::currentThread()->setObjectName(threadName);

   // Initial scan. 'entriesToScan' may be modified concurrently by 'addRoot(..)' or 'rmRoot(..)'.
   forever
   {
      Entry* entry = nullptr;
      {
         // Same locking order as 'stopScanning(..)'.
         QMutexLocker scanningLocker(&this->scanningMutex);
         QMutexLocker locker(&this->mutex);
         if (this->entriesToScan.isEmpty())
            break;
         entry = this->takeEntryToScan(true);
         this->currentScanningEntry = entry;
      }
      this->scan(entry, true);
   }

   emit initialScanFinished();

   forever
   {
      this->computeSomeHashes();

      this->mutex.lock();

      foreach (Entry* entry, this->rootEntriesToRemove)
      {
         L_DEBU(QString("Stop watching this path: %1").arg(entry->getAbsolutePath()));
         if (this->dirWatcher)
         {
            auto path = entry->getAbsolutePath();
            this->dirWatcher->rmPath(path.toString(false), path.getFilename());
         }

         entry->removeUnfinishedFiles();
         entry->del();
      }
      this->rootEntriesToRemove.clear();

      // If there is no watcher capability or no directory to watch then
      // we wait for an added directory.
      if (!this->dirWatcher || this->dirWatcher->nbWatchedPath() == 0 || !this->entriesToScan.isEmpty())
      {
         if (
            this->entriesToScan.isEmpty() &&
            !this->hashingQueue.next()
         )
         {
            L_DEBU("Waiting for a new entry added..");
            const int timeout = this->nextWaitTimeout(this->timerScanUnwatchable.elapsed(), this->schedulerClock.elapsed());
            this->mutex.unlock();

            this->dirEvent->wait(timeout);
         }
         else
         {
            this->mutex.unlock();
         }

         Entry* nextEntryToScan = nullptr;
         {
            // Same locking order as 'stopScanning(..)'.
            QMutexLocker scanningLocker(&this->scanningMutex);
            QMutexLocker locker(&this->mutex);
            if (!this->entriesToScan.isEmpty())
            {
               nextEntryToScan = this->takeEntryToScan(false);
               this->currentScanningEntry = nextEntryToScan;
            }
         }

         // Synchronize the new directory.
         if (nextEntryToScan)
            this->scan(nextEntryToScan);
      }
      else
      {
         // If we have no dir to scan and no file to hash we wait for a new shared file
         // or a filesystem event.
         if (
            this->entriesToScan.isEmpty() &&
            !this->hashingQueue.next()
         )
         {
            const int timeout = this->nextWaitTimeout(this->timerScanUnwatchable.elapsed(), this->schedulerClock.elapsed());
            this->mutex.unlock();

            this->processEvents(
               // Wait for filesystem modifications.
               this->dirWatcher->waitEvent(
                  timeout,
                  QList<WaitCondition*> { this->dirEvent }
               )
            );
         }
         else
         {
            this->mutex.unlock();
            this->processEvents(this->dirWatcher->waitEvent(0)); // Just pick the new events. (Don't wait for new event).
         }
      }

      if (this->timerScanUnwatchable.elapsed() >= SCAN_PERIOD_UNWATCHABLE_DIRS)
      {
         QList<Entry*> unwatchableEntriesCopy;
         {
            QMutexLocker locker(&this->mutex);
            unwatchableEntriesCopy = this->unwatchableEntries;
         }

         // Read the shared list only under mutex; perform scans after releasing it.
         for (Entry* entry : unwatchableEntriesCopy)
            this->scan(entry);
      }

      if (this->toStop)
      {
         L_DEBU("FileUpdater mainloop finished");
         return;
      }
   }
}

// Both wait backends wake for the earliest pending maintenance task. Calculate
// this before releasing mutex so concurrent queue changes cannot race the reads.
int FileUpdater::nextWaitTimeout(qint64 elapsedSinceScan, qint64 now) const
{
   QMutexLocker locker(&this->mutex);
   int timeout = this->hashingQueue.retryTimeout(now);
   if (!this->unwatchableEntries.isEmpty())
   {
      const int scanTimeout = static_cast<int>(qMax<qint64>(0, this->SCAN_PERIOD_UNWATCHABLE_DIRS - elapsedSinceScan));
      timeout = timeout < 0 ? scanTimeout : qMin(timeout, scanTimeout);
   }
   return timeout;
}

void FileUpdater::computeSomeHashes()
{
   QElapsedTimer timer;
   timer.start();
   const quint32 MINIMUM_DURATION_WHEN_HASHING = SETTINGS.get<quint32>("minimum_duration_when_hashing");

   for (;;)
   {
      // Keep the selected file alive until both hashing and bookkeeping finish.
      // Cache destruction waits on this mutex before removing pending work.
      QMutexLocker hashingLocker(&this->hashingMutex);
      File* file;
      {
         // Retain the existing stop/removal ordering, but queue state itself is
         // always protected by mutex, including reads from the status thread.
         if (this->toStopHashing.exchange(false) || this->toStop)
            break;
         QMutexLocker locker(&this->mutex);
         this->hashingQueue.releaseDueRetries(this->schedulerClock.elapsed());
         file = this->hashingQueue.next();
         if (!file)
            break;
      }

      bool ioError = false;
      try
      {
         this->fileHasher.start(file, 1, nullptr, true);
      }
      catch (IOErrorException&)
      {
         ioError = true;
      }

      {
         QMutexLocker locker(&this->mutex);
         // Update only existing work; never resurrect a removed job.
         if (this->hashingQueue.contains(file))
            this->hashingQueue.finishPass(file, file->getRemainingBytesToHash(), ioError,
               this->schedulerClock.elapsed(), this->IO_ERROR_WAITING_BEFORE_RETRY);
      }

      if (static_cast<quint32>(timer.elapsed()) >= MINIMUM_DURATION_WHEN_HASHING)
         break;
   }

   // Persistence and disk I/O never run under the scheduler mutex.
   this->fileHasher.flushHashes();
}

/**
  * Stop the current hashing process or the next hashing process.
  * The file is re-queued.
  */
void FileUpdater::stopHashing()
{
   QMutexLocker lockerHashing(&this->hashingMutex);
   L_DEBU("Stop hashing . . .");

   this->fileHasher.stop();

   L_DEBU("Hashing stopped");
   this->toStopHashing = true;
}

/**
  * Synchronize the cache with the file system.
  * Scan recursively all the directories and files contained
  * in entry (if 'entry' is a directory). Create the associated cached tree structure under a
  * given 'Directory*'.
  * The directories may already exist in the cache.
  */
void FileUpdater::scan(Entry* entry, bool addUnfinished)
{
   L_DEBU(QString("Start scanning an entry: %1").arg(entry->getAbsolutePath()));

   this->scanningMutex.lock();
   this->currentScanningEntry = entry;
   this->scanningMutex.unlock();

   const auto abortIfRequested =
      [this](Directory* currentDir)
      {
         if (!this->scanAbortRequested.load(std::memory_order_relaxed) && !this->toStop)
            return false;

         QMutexLocker locker(&this->scanningMutex);
         L_DEBU(QString("Scanning aborted: %1").arg(currentDir->getAbsolutePath()));
         this->currentScanningEntry = nullptr;
         this->scanAbortRequested.store(false, std::memory_order_relaxed);
         this->scanningStopped.wakeOne();
         return true;
      };

   // Recovery may be the only notification of a root's deletion or type change.
   // Remove a directory's trailing slash so its replacement can be recognized as a file.
   const QFileInfo entryInfo(QDir::cleanPath(entry->getAbsolutePath()));
   if (entry->isRoot() && (!entryInfo.exists() || entryTypeChanged(entry, entryInfo)))
      emit deleteSharedEntry(entry->getRoot());
   else if (File* file = dynamic_cast<File*>(entry))
   {
      this->addScannedFile(entryInfo, file);
   }
   else if (Directory* dir = dynamic_cast<Directory*>(entry))
   {
      QList<Directory*> dirsToVisit;
      dirsToVisit << dir;

      while (!dirsToVisit.isEmpty())
      {
         Directory* currentDir = dirsToVisit.takeFirst();

         if (abortIfRequested(currentDir))
            return;

         const QList<Directory*> currentSubDirs = currentDir->getSubDirs();
         const QList<File*> currentFiles = currentDir->getCompleteFiles(); // We don't care about the unfinished files.
         // Track unseen entries without repeatedly searching and shrinking the lists.
         // Keep the snapshots to preserve deletion order after enumeration.
         QSet<Directory*> unseenSubDirs(currentSubDirs.cbegin(), currentSubDirs.cend());
         QSet<File*> unseenFiles(currentFiles.cbegin(), currentFiles.cend());

         // Only new files need hash-cache lookups. Keep a bounded metadata batch;
         // neither cache nor scheduler locks may be held while waiting for the database.
         constexpr int HASH_LOOKUP_BATCH_SIZE = 128;
         QList<QFileInfo> newFiles;
         const auto addNewFiles = [&]()
         {
            if (newFiles.isEmpty())
               return true;
            if (abortIfRequested(currentDir))
               return false;

            QList<HC::IHashCache::FileMetadata> requests;
            requests.reserve(newFiles.size());
            for (const auto& info : std::as_const(newFiles))
               requests.append({ info.absoluteFilePath(), info.size(), info.lastModified() });
            const auto hashes = currentDir->getCache()->getHashCache()->getHashesBatch(requests);
            for (int i = 0; i < newFiles.size(); ++i)
            {
               if (abortIfRequested(currentDir))
                  return false;
               const auto& info = newFiles.at(i);
               // Resolve again after the blocking lookup; another caller may have created the file.
               if (File* file = this->addScannedFile(info, currentDir->getFile(info.fileName()), currentDir, &hashes.at(i), addUnfinished))
                  unseenFiles.remove(file);
            }
            newFiles.clear();
            return true;
         };

         // Stream metadata instead of retaining a QFileInfo for every entry in the directory.
         // Directory::add maintains cache ordering independently of enumeration order.
         // TODO: Add an option to follow or not symlinks.
         QDirIterator entries(currentDir->getAbsolutePath(),
            QDir::AllEntries | QDir::NoDotAndDotDot | QDir::NoSymLinks | QDir::Hidden);
         while (entries.hasNext())
         {
            if (abortIfRequested(currentDir))
               return;

            const QFileInfo fileInfo = entries.nextFileInfo();
            if (fileInfo.isDir())
            {
               Directory* subDir = currentDir->createSubDir(fileInfo.fileName(), false, isHiddenEntry(fileInfo));
               subDir->setScanned(false);
               dirsToVisit << subDir;
               unseenSubDirs.remove(subDir);
            }
            else if (addUnfinished || !Global::isFileUnfinished(fileInfo.fileName()))
            {
               if (File* file = currentDir->getFile(fileInfo.fileName()))
                  unseenFiles.remove(this->addScannedFile(fileInfo, file, currentDir));
               else
               {
                  newFiles.append(fileInfo);
                  if (newFiles.size() == HASH_LOOKUP_BATCH_SIZE && !addNewFiles())
                     return;
               }
            }
         }
         if (!addNewFiles())
            return;

         // Delete cached entries not encountered on the file system.
         for (File* f : currentFiles)
            if (unseenFiles.contains(f))
               this->deleteEntry(f);

         for (Directory* d : currentSubDirs)
            if (unseenSubDirs.contains(d))
               this->deleteEntry(d);

         currentDir->setScanned(true);
      }
   }

   this->scanningMutex.lock();
   this->currentScanningEntry = nullptr;
   this->scanAbortRequested.store(false, std::memory_order_relaxed);
   this->scanningStopped.wakeOne();
   this->scanningMutex.unlock();

   this->mutex.lock();
   if (this->unwatchableEntries.contains(entry))
      this->timerScanUnwatchable.start();
   this->mutex.unlock();

   L_DEBU(QString("Scanning terminated: %1").arg(entry->getAbsolutePath()));
}

/**
  * Add a scanned file from the filesystem, the cached file ('file') and its parent directory ('parentDirectory')
  * may be given.
  */
File* FileUpdater::addScannedFile(const QFileInfo& fileInfo, File* file, Directory* parentDirectory,
   const QList<Common::Hash>* cachedHashes, bool addUnfinished)
{
   QMutexLocker locker(&this->mutex);

   if (file)
      file->updateFromScan(fileInfo);

   if (parentDirectory && !file)
   {
      // Very special case: there is a file 'a' without File* in cache and a file 'a.unfinished'.
      // This case occurs when a file is redownloaded, the File* 'a' is renamed as 'a.unfinished' but the physical file 'a'
      // is not deleted.
      const QString unfinishedName = fileInfo.fileName() + Global::getUnfinishedSuffix();
      File* unfinishedFile = parentDirectory->getFile(unfinishedName);
      if (unfinishedFile)
      {
         return unfinishedFile;
      }
      else
      {
         // During the initial scan the replacement may not be cached yet. Suppress
         // the old completed file regardless of enumeration or hash-batch order.
         // Other scans never cache unfinished files, so an uncached sibling is stale.
         // Match the scanner's file filtering: directories and symlinks don't count.
         // Use the scanned location; the cached directory may have moved meanwhile.
         if (addUnfinished && !Global::isFileUnfinished(fileInfo.fileName()))
         {
            const QFileInfo unfinishedInfo(fileInfo.dir(), unfinishedName);
            if (unfinishedInfo.isFile() && !unfinishedInfo.isSymLink())
               return nullptr;
         }

         // A directory may have moved while the batch was being fetched. In that
         // case retain the normal lookup using the file's current cache path.
         if (cachedHashes && parentDirectory->getAbsolutePath().setFilename(fileInfo.fileName()) != Common::Path(fileInfo.absoluteFilePath()))
            cachedHashes = nullptr;
         file =
            new File(
               parentDirectory->getRoot(),
               fileInfo.fileName(),
               fileInfo.size(),
               isHiddenEntry(fileInfo),
               fileInfo.lastModified(),
               parentDirectory,
               cachedHashes ? *cachedHashes : QList<Common::Hash>(),
               false,
               cachedHashes == nullptr
         );
      }
   }

   if (file)
      this->hashingQueue.enqueue(file, file->getRemainingBytesToHash());

   return file;
}

/**
  * Returns true if 'entry' is 'root' or is located somewhere under 'root'.
  */
static bool isEntryUnder(Entry* entry, Entry* root)
{
   if (entry == root)
      return true;

   Directory* rootDir = dynamic_cast<Directory*>(root);
   if (!rootDir)
      return false;

   if (Directory* dir = dynamic_cast<Directory*>(entry))
      return dir->isAChildOf(rootDir);

   if (File* file = dynamic_cast<File*>(entry))
      return file->hasAParentDir(rootDir);

   return false;
}

/**
  * Stops the current scan if it concerns 'entry' or one of its sub entries and remove 'entry' from the queue.
  * If you omit 'entry' then all scanning will be stopped and removed from the queue.
  */
void FileUpdater::stopScanning(Entry* entry)
{
   QMutexLocker scanningLocker(&this->scanningMutex);

   // A new scan of another entry under 'entry' may start right after the current one is aborted, hence the loop.
   while (this->currentScanningEntry && (!entry || isEntryUnder(this->currentScanningEntry, entry)))
   {
      this->scanAbortRequested.store(true, std::memory_order_relaxed);
      this->scanningStopped.wait(&this->scanningMutex);
   }

   QMutexLocker locker(&this->mutex);
   if (entry)
   {
      if (this->pendingScanEntries.remove(entry))
         this->entriesToScan.removeOne(entry);
   }
   else
   {
      this->entriesToScan.clear();
      this->pendingScanEntries.clear();
   }
}

/**
  * Delete an entry and if it's a directory remove it and its sub children from 'this->dirsToScan'.
  * It can't be used to remove a 'SharedDirectory', only the 'Cache' is able to do that.
  */
void FileUpdater::deleteEntry(Entry* entry)
{
   if (!entry)
      return;

   QMutexLocker locker(&this->mutex);

   this->removeFromHashingQueue(entry);
   this->removeFromEntriesToScan(entry);

   entry->removeUnfinishedFiles();
   entry->del();
}

void FileUpdater::enqueueEntryToScan(Entry* entry)
{
   QMutexLocker locker(&this->mutex);
   if (!entry || this->pendingScanEntries.contains(entry))
      return;

   this->pendingScanEntries.insert(entry);
   this->entriesToScan.append(entry);
}

Entry* FileUpdater::takeEntryToScan(bool oldestFirst)
{
   QMutexLocker locker(&this->mutex);
   if (this->entriesToScan.isEmpty())
      return nullptr;

   // Initial scans keep insertion order; subsequent scans prefer recent events.
   Entry* entry = oldestFirst ? this->entriesToScan.takeFirst() : this->entriesToScan.takeLast();
   this->pendingScanEntries.remove(entry);
   return entry;
}

/**
  * Remove a directory and its sub entries from 'this->entriesToScan'.
  */
void FileUpdater::removeFromEntriesToScan(Entry* entry)
{
   QMutexLocker locker(&this->mutex);

   // Walk the queue rather than the subtree: the queue is usually short, and the children of a directory
   // may only be read under its own mutex because downloads and scans modify them concurrently.
   this->entriesToScan.removeIf(
      [this, entry](Entry* queued)
      {
         if (!isEntryUnder(queued, entry))
            return false;
         this->pendingScanEntries.remove(queued);
         return true;
      }
   );
}

/**
  * Remove the pending work for an entry and its descendants.
  */
void FileUpdater::removeFromHashingQueue(Entry* entry)
{
   QMutexLocker locker(&this->mutex);

   if (File* file = dynamic_cast<File*>(entry))
      this->hashingQueue.remove(file);
   else
      this->hashingQueue.removeIf([entry](File* file) { return isEntryUnder(file, entry); });
}

/**
  * Event from the filesystem like a file created or renamed.
  * return true is at least one event is a timeout.
  */
bool FileUpdater::processEvents(const QList<WatcherEvent>& events)
{
   const auto newOrContentChanged =
      [this](const QString& path)
      {
         Entry* entry = this->fileManager->getEntry(path);
         if (!entry)
            entry = this->fileManager->getEntry(Common::Path(path + '/'));

         const QFileInfo info(path);
         if (entryTypeChanged(entry, info))
         {
            // Shared roots belong to the cache; their old share is no longer valid.
            if (entry->isRoot())
            {
               emit deleteSharedEntry(entry->getRoot());
               return;
            }

            Directory* parent = this->fileManager->getFittestDirectory(Common::Path(path).removeLastElement());
            this->deleteEntry(entry);
            this->enqueueEntryToScan(parent);
            return;
         }

         File* file = dynamic_cast<File*>(entry);
         if (file)
         {
            this->enqueueEntryToScan(file);
         }
         else
         {
            Directory* dir = this->fileManager->getFittestDirectory(path);

            this->enqueueEntryToScan(dir);
         }
      };

   if (events.isEmpty())
      return false;

   bool timeout = false;
   for (const WatcherEvent& event : events)
   {
      if (event.type == WatcherEvent::TIMEOUT)
      {
         timeout = true;
         continue;
      }

      if (event.type == WatcherEvent::RESCAN || event.type == WatcherEvent::WATCH_LOST)
      {
         // Recovery concerns the watched root, not a filename notification. In particular,
         // directory paths have had their trailing slash removed by WatcherEvent.
         Entry* entry = this->fileManager->getEntry(Common::Path(
            event.isWatchedFile ? event.path1 : event.path1 + '/'));
         if (entry)
         {
            QMutexLocker locker(&this->mutex);
            if (!this->rootEntriesToRemove.contains(entry))
               this->enqueueEntryToScan(entry);
            if (event.type == WatcherEvent::WATCH_LOST &&
                !this->unwatchableEntries.contains(entry) && !this->rootEntriesToRemove.contains(entry))
               this->unwatchableEntries << entry;
         }
         continue;
      }

      // Unfinished files are ignored.
      if (Global::isFileUnfinished(event.path1))
         continue;

      L_DEBU(QString("A file structure event occurs:\n%1").arg(event.toStr()));

      switch (event.type)
      {
      case WatcherEvent::MOVE:
         {
            const auto pathDestination = Common::Path::fromExistingPath(event.path2);
            const auto pathOrigin =
                  Common::Path(
                     !pathDestination.isFile() && !event.path1.endsWith('/') ? event.path1 + '/' : event.path1
                  );

            // L_DEBU(QString("MOVE from %1, to %2").arg(pathOrigin.toString(), pathDestination.toString()));

            Directory* destination =
               dynamic_cast<Directory*>(this->fileManager->getEntry(pathDestination.removeLastElement()));

            Entry* entryToMove = this->fileManager->getEntry(pathOrigin);

            if (entryToMove)
            {
               entryToMove->rename(pathDestination.getLastElement());

               if (destination)
               {
                  // A shared root moved into another shared directory is merged into it, its shared entry is removed.
                  if (entryToMove->isRoot())
                  {
                     entryToMove->getRoot()->moveInto(destination);

                     // The entry of a shared file isn't transferred, the destination is rescanned to find the file.
                     if (dynamic_cast<File*>(entryToMove))
                     {
                        this->enqueueEntryToScan(destination);
                     }
                  }
                  else
                  {
                     entryToMove->moveInto(destination);
                  }
               }
               else if (entryToMove->isRoot())
               {
                  // A shared root moved outside any other shared directory: only its location changes.
                  // 'SharedEntry::path' is the directory containing the shared entry, the last element is its name (already renamed above).
                  SharedEntry* sharedEntry = entryToMove->getRoot();
                  sharedEntry->setPath(pathDestination.removeLastElement());

                  if (this->dirWatcher)
                  {
                     this->dirWatcher->rmPath(pathOrigin.toString(false), pathOrigin.getFilename());

                     const Common::Path newPath = sharedEntry->getPath();
                     bool watchable = false;
                     try
                     {
                        watchable = this->dirWatcher->addPath(newPath.toString(false), newPath.getFilename());
                     }
                     catch (FileSystemEntryNotFoundException&)
                     {
                     }

                     QMutexLocker locker(&this->mutex);
                     if (watchable)
                        this->unwatchableEntries.removeOne(entryToMove);
                     else
                     {
                        L_WARN(QString("This entry is not watchable: %1").arg(newPath.toString()));
                        if (!this->unwatchableEntries.contains(entryToMove))
                           this->unwatchableEntries << entryToMove;
                     }
                  }
               }
               else
               {
                  this->deleteEntry(entryToMove);
               }
            }
            else
            {
               L_DEBU(QString("Can't find the entry to move: %1").arg(event.path1));

               // When a file is renamed by changing only it case in explorer there are three events triggered:
               // REMOVED, RENAMED_OLD_NAME, RENAMED_NEW_NAME.
               // In this case the entry to move do not exist anymore (REMOVED) and we treat RENAMED_NEW_NAME as
               // a new file event.
               newOrContentChanged(event.path2);
            }

            break;
         }

      case WatcherEvent::DELETED:
         {
            // Replacing a file can report deletion of the previous destination.
            // If its path exists again, synchronize the replacement instead of
            // retiring the completed download now stored at that same path.
            if (QFileInfo::exists(event.path1))
            {
               newOrContentChanged(event.path1);
               break;
            }

            SharedEntry* sharedEntry = this->fileManager->getSharedEntry(event.path1);
            if (!sharedEntry)
               sharedEntry = this->fileManager->getSharedEntry(event.path1 + '/');

            if (sharedEntry)
               emit deleteSharedEntry(sharedEntry);
            else
            {
               Entry* entry = this->fileManager->getEntry(event.path1);
               if (!entry)
                  entry = this->fileManager->getEntry(Common::Path(event.path1 + '/'));

               if (entry)
                  this->deleteEntry(entry);
            }
            break;
         }

      // TODO: Implement ::NEW, a new directory or file should be added directly without scanning an entire
      // directory tree.
      case WatcherEvent::NEW:
      case WatcherEvent::CONTENT_CHANGED:
         newOrContentChanged(event.path1);
         break;

      case WatcherEvent::RESCAN:
      case WatcherEvent::WATCH_LOST:
      case WatcherEvent::UNKNOWN:
      case WatcherEvent::TIMEOUT:
         break; // Do nothing.
      }
   }

   return timeout;
}
