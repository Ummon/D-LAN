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
#include <QDir>
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
   progress(0),
   currentScanningEntry(nullptr),
   toStopHashing(false),
   remainingSizeToHash(0)
{
   this->dirEvent = WaitCondition::getNewWaitCondition();
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
  * @exception DirNotFoundException
  */
void FileUpdater::addRoot(SharedEntry* sharedEntry)
{
   QMutexLocker locker(&this->mutex);

   L_DEBU(QString("FileUpdater: addRoot: %1").arg(sharedEntry->getPath()));

   const Common::Path& entryPath = sharedEntry->getPath();

   bool watchable = false;
   if (this->dirWatcher)
      watchable = this->dirWatcher->addPath(entryPath.toString(false), entryPath.getFilename());

   this->entriesToScan << sharedEntry->getRootEntry();

   if (!watchable)
   {
      L_WARN(QString("This entry is not watchable: %1").arg(entryPath.toString()));
      this->unwatchableEntries << sharedEntry->getRootEntry();
   }

   this->dirEvent->release();
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

   this->removeFromFilesWithoutHashes(root);
   this->removeFromEntriesToScan(root);
   this->unwatchableEntries.removeOne(root);
   this->rootEntriesToRemove << root;

   this->dirEvent->release();
}

void FileUpdater::prioritizeAFileToHash(File* file)
{
   // Same locking order as 'computeSomeHashes()' and 'rmRoot(..)': 'hashingMutex' then 'mutex'.
   QMutexLocker lockerHashing(&this->hashingMutex);
   QMutexLocker locker(&this->mutex);

   L_DEBU(QString("FileUpdater::prioritizeAFileToHash: %1").arg(file->getAbsolutePath()));

   // If a file is incomplete (unfinished) we can't compute its hashes because we don't have all data.
   if (!file->hasAllHashes() && file->isComplete())
   {
      if (this->filesWithoutHashes.removeOne(file))
      {
         this->remainingSizeToHash -= file->getSize();
      }

      if (!this->filesWithoutHashesPrioritized.contains(file))
      {
         this->filesWithoutHashesPrioritized << file;
         this->remainingSizeToHash += file->getSize();
      }

      // Commented to avoid this behavior:
      // When a lot of unhashed tiny file are asked the hashing process will constantly abort the current hashing file
      // and will never finish it thus slow down the global hashing rate.
      // this->fileHasher.stop();

      this->toStopHashing = true;
   }
   else
      L_DEBU(QString("FileUpdater::prioritizeAFileToHash, unable to prioritize: %1").arg(file->getAbsolutePath()));

}

bool FileUpdater::isScanning() const
{
   QMutexLocker scanningLocker(&this->scanningMutex);
   return this->currentScanningEntry != nullptr;
}

bool FileUpdater::isHashing() const
{
   QMutexLocker locker(&this->hashingMutex);
   return !this->filesWithoutHashes.isEmpty() || !this->filesWithoutHashesPrioritized.isEmpty();
}

int FileUpdater::getProgress() const
{
   QMutexLocker locker(&this->mutex);
   return this->progress;
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
         entry = this->entriesToScan.takeFirst();
         this->currentScanningEntry = entry;
      }
      this->scan(entry, true);
   }

   emit initialScanFinished();

   this->progress = 0;

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
            this->filesWithoutHashes.isEmpty() &&
            this->filesWithoutHashesPrioritized.isEmpty()
         )
         {
            L_DEBU("Waiting for a new entry added..");
            this->mutex.unlock();

            this->dirEvent->wait(this->unwatchableEntries.isEmpty() ? -1 : SCAN_PERIOD_UNWATCHABLE_DIRS);
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
               nextEntryToScan = this->entriesToScan.takeLast();
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
            this->filesWithoutHashes.isEmpty() &&
            this->filesWithoutHashesPrioritized.isEmpty()
         )
         {
            this->mutex.unlock();

            this->processEvents(
               // Wait for filesystem modifications.
               this->dirWatcher->waitEvent(
                  !this->filesWithoutHashesIOError.isEmpty() ?
                       IO_ERROR_WAITING_BEFORE_RETRY
                     : (!this->unwatchableEntries.isEmpty() ?
                          SCAN_PERIOD_UNWATCHABLE_DIRS
                        : -1
                     ),
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

      if (timerScanUnwatchable.elapsed() >= SCAN_PERIOD_UNWATCHABLE_DIRS && !this->unwatchableEntries.isEmpty())
      {
         this->mutex.lock();
         QList<Entry*> unwatchableEntriesCopy = this->unwatchableEntries;
         this->mutex.unlock();

         // Synchronize the new directory.
         for (QListIterator<Entry*> i(unwatchableEntriesCopy); i.hasNext();)
         {
            Entry* entry = i.next();
            this->scan(entry);
         }
      }

      {
         // The lists are also modified by 'rmRoot(..)' and 'prioritizeAFileToHash(..)' from other threads.
         QMutexLocker lockerHashing(&this->hashingMutex);
         QMutexLocker locker(&this->mutex);
         if (!this->filesWithoutHashesIOError.isEmpty())
         {
            this->filesWithoutHashes.append(this->filesWithoutHashesIOError);
            this->filesWithoutHashesIOError.clear();
         }
      }

      if (this->toStop)
      {
         L_DEBU("FileUpdater mainloop finished");
         return;
      }
   }
}

/**
  * It will take some files from 'filesWithoutHashesPrioritized' or 'fileWithoutHashes' and compute theirs hashes.
  * The minimum duration of the computation is equal to the setting 'minimum_duration_when_hashing'.
  */
void FileUpdater::computeSomeHashes()
{
   QMutexLocker locker(&this->hashingMutex);

   if (this->toStopHashing)
   {
      this->toStopHashing = false;
      return;
   }

   if (this->filesWithoutHashes.isEmpty() && this->filesWithoutHashesPrioritized.isEmpty())
      return;

   L_DEBU("Start computing some hashes . . .");

   QElapsedTimer timer;
   timer.start();

   // We take the file from the prioritized list first.
   QList<QList<File*>*> fileLists { &this->filesWithoutHashesPrioritized, &this->filesWithoutHashes };
   for (QMutableListIterator<QList<File*>*> i(fileLists); i.hasNext();)
   {
      QList<File*>* fileList = i.next();
      while (!fileList->empty())
      {
         File* nextFileToHash = fileList->first();

         // A file can change its state from 'completed' to 'unfinished' if it's redownloaded.
         if (nextFileToHash->isComplete())
         {
            locker.unlock();
            bool gotAllHashes = false;
            bool ioError = false;
            try
            {
               int hashedAmount = 0;
               // Be careful of methods 'prioritizeAFileToHash(..)' and 'rmRoot(..)' called concurrently here.
               // We ask to compute the next unknown chunk (only one).
               gotAllHashes = this->fileHasher.start(nextFileToHash->asFileForHasher(), 1, &hashedAmount);

               {
                  QMutexLocker locker(&this->mutex);
                  this->remainingSizeToHash -= hashedAmount;
               }

               this->updateHashingProgress();
            }
            catch (IOErrorException&)
            {
               ioError = true;
            }
            locker.relock();

            // The current hashing file may have been removed from 'filesWithoutHashes' or
            // 'filesWithoutHashesPrioritized' by 'rmRoot(..)' while the mutex was unlocked: it is about to be
            // deleted and must not be requeued.
            if (!fileList->isEmpty() && fileList->first() == nextFileToHash)
            {
               if (ioError)
               {
                  fileList->removeFirst();
                  this->filesWithoutHashesIOError << nextFileToHash;
               }
               else if (gotAllHashes)
                  fileList->removeFirst();
               // Special case for the prioritized list, we put the file at the end after the computation of a hash.
               else if (fileList == &this->filesWithoutHashesPrioritized && fileList->size() > 1)
                  fileList->move(0, fileList->size() - 1);
            }
         }
         else
         {
            QMutexLocker locker(&this->mutex);
            this->remainingSizeToHash -= fileList->first()->getSize();
            fileList->removeFirst();
         }

         if (this->toStopHashing)
         {
            this->toStopHashing = false;
            goto end;
         }

         static const quint32 MINIMUM_DURATION_WHEN_HASHING = SETTINGS.get<quint32>("minimum_duration_when_hashing");
         if (static_cast<quint32>(timer.elapsed()) >= MINIMUM_DURATION_WHEN_HASHING)
            goto end;
      }
   }

end:
   L_DEBU(
      QString(
         "Computing some hashes ended. this->filesWithoutHashes.size(): %1, this->filesWithoutHashesPrioritized.size(): %2"
      )
      .arg(this->filesWithoutHashes.size())
      .arg(this->filesWithoutHashesPrioritized.size())
   );

   {
      QMutexLocker locker(&this->mutex);
      if (this->filesWithoutHashes.isEmpty() && this->filesWithoutHashesPrioritized.isEmpty())
      {
         this->remainingSizeToHash = 0;
         this->progress = 0;
      }
   }
}

void FileUpdater::updateHashingProgress()
{
   const qint64 totalAmountOfData = this->fileManager->getAmount();

   QMutexLocker locker(&this->mutex);
   this->progress = totalAmountOfData == 0 ? 0 : 10000LL * (totalAmountOfData - this->remainingSizeToHash) / totalAmountOfData;
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

   if (File* file = dynamic_cast<File*>(entry))
   {
      this->addScannedFile(QFileInfo(file->getAbsolutePath()), file);
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

         QList<Directory*> currentSubDirs = currentDir->getSubDirs();
         QList<File*> currentFiles = currentDir->getCompleteFiles(); // We don't care about the unfinished files.

         // TODO: Add an option to follow or not symlinks.
         for (
            const QFileInfo& fileInfo :
            QDir(currentDir->getAbsolutePath()).entryInfoList(
               QDir::AllEntries | QDir::NoDotAndDotDot | QDir::NoSymLinks | QDir::Hidden
            )
         )
         {
            if (abortIfRequested(currentDir))
               return;

            if (fileInfo.isDir())
            {
               Directory* subDir = currentDir->createSubDir(fileInfo.fileName(), false, fileInfo.isHidden());
               subDir->setScanned(false);
               dirsToVisit << subDir;
               currentSubDirs.removeOne(subDir);
            }
            else if (addUnfinished || !Global::isFileUnfinished(fileInfo.fileName()))
            {
               if (File* file = this->addScannedFile(fileInfo, currentDir->getFile(fileInfo.fileName()), currentDir))
                  currentFiles.removeOne(file);
            }
         }

         // Deletes all the files and directories which doesn't exist on the file system.
         foreach (File* f, currentFiles)
            this->deleteEntry(f);

         foreach (Directory* d, currentSubDirs)
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
  * TODO: re-read carefully this method and think about all possible cases.
  */
File* FileUpdater::addScannedFile(const QFileInfo& fileInfo, File* file, Directory* parentDirectory)
{
   QMutexLocker locker(&this->mutex);

   if (file && file->isComplete() && !file->correspondTo(fileInfo))
      file->fileHasChangedOnDisk(fileInfo);

   if (parentDirectory && !file)
   {
      // Very special case: there is a file 'a' without File* in cache and a file 'a.unfinished'.
      // This case occurs when a file is redownloaded, the File* 'a' is renamed as 'a.unfinished' but the physical file 'a'
      // is not deleted.
      File* unfinishedFile = parentDirectory->getFile(fileInfo.fileName().append(Global::getUnfinishedSuffix()));
      if (unfinishedFile)
      {
         return unfinishedFile;
      }
      else
      {
         file =
            new File(
               parentDirectory->getRoot(),
               fileInfo.fileName(),
               fileInfo.size(),
               fileInfo.isHidden(),
               fileInfo.lastModified(),
               parentDirectory
         );
      }
   }

   if (
      file &&
      file->getSize() > 0 &&
      // If a file is incomplete (unfinished) we can't compute its hashes because we don't have all data.
      file->isComplete() &&
      (!file->hasAllHashes() || !file->correspondTo(fileInfo)) &&
      !this->filesWithoutHashes.contains(file) &&
      !this->filesWithoutHashesPrioritized.contains(file))
   {
      this->filesWithoutHashes << file;
      this->remainingSizeToHash += file->getSize();
   }

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
      this->entriesToScan.removeOne(entry);
   else
      this->entriesToScan.clear();
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

   this->removeFromFilesWithoutHashes(entry);
   this->removeFromEntriesToScan(entry);

   entry->removeUnfinishedFiles();
   entry->del();
}

/**
  * Remove a directory and its sub entries from 'this->entriesToScan'.
  */
void FileUpdater::removeFromEntriesToScan(Entry* entry)
{
   QMutexLocker locker(&this->mutex);

   this->entriesToScan.removeOne(entry);

   if (Directory* dir = dynamic_cast<Directory*>(entry))
   {
      DirIterator i(dir);
      while (Entry* entry = i.next())
         this->entriesToScan.removeOne(entry);
   }
}

/**
  * Remove all the pending files owned by 'dir'.
  */
void FileUpdater::removeFromFilesWithoutHashes(Entry* entry)
{
   QMutexLocker locker(&this->mutex);

   if (Directory* dir = dynamic_cast<Directory*>(entry))
   {
      for (QMutableListIterator<File*> i(this->filesWithoutHashes); i.hasNext();)
      {
         File* f = i.next();
         if (f->hasAParentDir(dir))
         {
            this->remainingSizeToHash -= f->getSize();
            i.remove();
         }
      }

      for (QMutableListIterator<File*> i(this->filesWithoutHashesPrioritized); i.hasNext();)
      {
         File* f = i.next();
         if (f->hasAParentDir(dir))
         {
            this->remainingSizeToHash -= f->getSize();
            i.remove();
         }
      }

      for (QMutableListIterator<File*> i(this->filesWithoutHashesIOError); i.hasNext();)
      {
         File* f = i.next();
         if (f->hasAParentDir(dir))
         {
            this->remainingSizeToHash -= f->getSize();
            i.remove();
         }
      }
   }
   else if (File* file = dynamic_cast<File*>(entry))
   {
      bool fileInAList = this->filesWithoutHashes.removeOne(file);
      fileInAList |= this->filesWithoutHashesPrioritized.removeOne(file);
      fileInAList |= this->filesWithoutHashesIOError.removeOne(file);

      if (fileInAList)
         this->remainingSizeToHash -= file->getSize();
   }
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
         File* file = dynamic_cast<File*>(this->fileManager->getEntry(path));
         if (file)
         {
            QMutexLocker locker(&this->mutex);
            if (!this->entriesToScan.contains(file))
               this->entriesToScan << file;
         }
         else
         {
            Directory* dir = this->fileManager->getFittestDirectory(path);

            QMutexLocker locker(&this->mutex);
            if (dir && !this->entriesToScan.contains(dir))
               this->entriesToScan << dir;
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
                        QMutexLocker locker(&this->mutex);
                        if (!this->entriesToScan.contains(destination))
                           this->entriesToScan << destination;
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

      case WatcherEvent::UNKNOWN:
      case WatcherEvent::TIMEOUT:
         break; // Do nothing.
      }
   }

   return timeout;
}
